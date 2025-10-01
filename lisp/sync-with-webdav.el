(defun rb/auth-password (host user &optional port realm)
  "Hent passord for HOST/USER (PORT/REALM) fra auth-source."
  (let* ((m (car (auth-source-search
                  :host host
                  :user user
                  :port (or port 443)
                  :realm realm
                  :require '(:user :secret :host))))
         (sec (plist-get m :secret)))
    (when sec (if (functionp sec) (funcall sec) sec))))

(defun rb/url-head (url user pass &optional timeout)
  "HEAD URL med Basic Auth. Returner plist med :status :last-modified :content-type."
  (let* ((url-user user)
         (url-password pass)
         (url-request-method "HEAD")
         (buf (url-retrieve-synchronously url t t (or timeout 12))))
    (unless buf (user-error "HEAD mislyktes for %s" url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (let (status lm ct)
            (when (re-search-forward "^HTTP/1\\.[01] \\([0-9]+\\)" nil t)
              (setq status (string-to-number (match-string 1))))
            (goto-char (point-min))
            (when (re-search-forward "^Last-Modified: \\(.*\\)$" nil t)
              (setq lm (date-to-time (match-string 1))))
            (goto-char (point-min))
            (when (re-search-forward "^Content-Type: \\([^;\r\n]+\\)" nil t)
              (setq ct (downcase (match-string 1))))
            (list :status status :last-modified lm :content-type ct)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun rb/http-get-to-file (url user pass dest)
  "GET URL med Basic Auth og skriv body til DEST. Avviser HTML."
  (let* ((url-user user)
         (url-password pass)
         (url-request-method "GET")
         ;; be om ukomprimert for å unngå rare klient-paths
         (url-request-extra-headers '(("Accept-Encoding" . "identity")))
         (buf (url-retrieve-synchronously url t t 20)))
    (unless buf (user-error "GET mislyktes for %s" url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (let* ((hdr-end (if (boundp 'url-http-end-of-headers)
                              url-http-end-of-headers
                            (progn (re-search-forward "\r?\n\r?\n" nil 'move)
                                   (match-end 0))))
                 (headers (buffer-substring (point-min) hdr-end)))
            ;; status
            (unless (and (string-match "HTTP/1\\.[01] \\([0-9]+\\)" headers)
                         (member (string-to-number (match-string 1 headers)) '(200 203 206)))
              (user-error "Ikke OK-respons:\n%s" headers))
            ;; MIME (kan være nil hos deg) – bare blokker *positivt* på HTML
            (let ((ctype (and (string-match "^Content-Type: \\([^;\r\n]+\\)" headers)
                              (downcase (match-string 1 headers)))))
              (when (and ctype (string-prefix-p "text/html" ctype))
                (with-temp-file "/tmp/init.el.http" (insert headers))
                (user-error "Fikk HTML (Content-Type=%s). Se /tmp/init.el.http" ctype)))
            ;; skriv body, men sniff for HTML hvis Content-Type var nil
            (let ((body-start hdr-end)
                  (body-end (point-max)))
              (when (<= body-start body-end)
                ;; HTML-sniff hvis ctype manglet
                (save-excursion
                  (goto-char body-start)
                  (let ((peek (buffer-substring body-start
                                                (min body-end (+ body-start 800)))))
                    (when (string-match-p "<\\!DOCTYPE\\|<html\\b" peek)
                      (with-temp-file "/tmp/init.el.dump" (insert peek))
                      (user-error "Body ser ut som HTML, avbryter (dump i /tmp/init.el.dump)"))))
                (write-region body-start body-end dest nil 'silent)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun rb/http-put-file (url user pass src)
  "PUT lokal fil SRC til URL med Basic Auth."
  (let* ((url-user user)
         (url-password pass)
         (url-request-method "PUT")
         (url-request-extra-headers '(("Content-Type" . "application/octet-stream")))
         ;; her: les filen som rå bytes og gjør den unibyte
         (url-request-data (with-temp-buffer
                             (set-buffer-multibyte nil)     ; <- viktig
                             (insert-file-contents-literally src)
                             (buffer-string)))
         (buf (url-retrieve-synchronously url t t 20)))
    (unless buf (user-error "PUT mislyktes for %s" url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (unless (re-search-forward "^HTTP/1\\.[01] \\([0-9]+\\)" nil t)
            (user-error "Fant ingen HTTP-statuslinje"))
          (let ((status (string-to-number (match-string 1))))
            (unless (member status '(200 201 204))
              (user-error "PUT feilet (status %s)\n%s"
                          status
                          (buffer-substring (point-min) (min (point-max) 500))))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun rb/sync-init-with-webdav ()
  "Synkroniser ~/.emacs.d/init.el med WebDAV – last ned hvis fjern er nyere, legg opp hvis lokal er nyere."
  (interactive)
  (let* ((host "dav.grendel.no")
         (user "webdav")
         (realm "WebDAV")
         (pass (rb/auth-password host user 443 realm))
         (remote "https://dav.grendel.no/webdav/init.el")
         (local  (expand-file-name "~/.emacs.d/init.el")))
    (unless pass
      (user-error "Finner ikke passord i auth-source for %s/%s" host user))
    (let* ((head (rb/url-head remote user pass))
           (remote-time (plist-get head :last-modified))
           (local-time (and (file-exists-p local) (nth 5 (file-attributes local)))))
      (cond
       ((and local-time remote-time
             (time-less-p local-time remote-time))
        ;; fjern nyere
        (rb/http-get-to-file remote user pass local)
        (message "Hentet ny init.el fra WebDAV."))
       ((and local-time
             (or (null remote-time)
                 (time-less-p remote-time local-time)))
        ;; lokal nyere
        (rb/http-put-file remote user pass local)
        (message "Lastet opp init.el til WebDAV."))
       (t
        (message "Ingen oppdatering nødvendig."))))))

(rb/sync-init-with-webdav)

(provide 'sync-with-webdav)
