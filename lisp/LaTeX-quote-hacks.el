(defun rm/tex-setup-quote-auto ()
  "Aktiver buffer-lokale triggere for å sette anførselstegn i LaTeX."
  (when (derived-mode-p 'latex-mode)
    ;; Kjør én gang nå, i tilfelle vi allerede kan gjette noe
    (rm/tex-apply-quotes-based-on-language)
    ;; Triggere (buffer-lokale)
    (add-hook 'before-save-hook              #'rm/tex-apply-quotes-based-on-language nil t)
    (add-hook 'hack-local-variables-hook     #'rm/tex-apply-quotes-based-on-language nil t)
    (add-hook 'ispell-change-dictionary-hook #'rm/tex-apply-quotes-based-on-language nil t)
    (add-hook 'flyspell-mode-hook            #'rm/tex-apply-quotes-based-on-language nil t)))

;; 1) Mapping fra språk → anførselstegn
(defvar rm/tex-quote-map
  '(("nb" . ("«"  . "»"))   ; norsk bokmål
    ("nn" . ("«"  . "»"))
    ("no" . ("«"  . "»"))   ; generisk norsk
    ("en-GB" . ("“" . "”"))
    ("en-US" . ("“" . "”"))
    ("de" . ("„" . "“"))
    ("fr" . ("«~" . "~»")))
  "Alist fra språk-kode til (TeX-open-quote . TeX-close-quote).")

(defun rm/tex--normalize-lang (s)
  "Normaliser språkkoder fra babel/polyglossia/magic til korte koder."
  (when s
    (let ((x (downcase s)))
      (cond
       ;; polyglossia: \setdefaultlanguage{norwegian} / {norsk} / {bokmal}
       ((string-match-p "\\`\\(norwegian\\|norsk\\|bokmal\\)\\'" x) "nb")
       ((string-match-p "\\`nynorsk\\'" x) "nn")
       ;; babel: norwegian, norsk, bokmal, ngerman/german, british/american, french
       ((member x '("norwegian" "norsk" "bokmal")) "nb")
       ((member x '("nynorsk")) "nn")
       ((member x '("british" "ukenglish")) "en-GB")
       ((member x '("american" "usenglish")) "en-US")
       ((member x '("english")) "en-GB")    ; heuristikk
       ((string-prefix-p "de" x) "de")
       ((string-prefix-p "fr" x) "fr")
       ((string-prefix-p "no" x) "no")
       ;; % !TeX spellcheck = nb_NO
       ((string-match "\\`\\([a-z][a-z]\\)_\\([A-Z][A-Z]\\)\\'" s)
        (let ((lo (downcase (match-string 1 s)))
              (hi (match-string 2 s)))
          (cond
           ((and (string= lo "nb") (string= hi "NO")) "nb")
           ((and (string= lo "nn") (string= hi "NO")) "nn")
           ((and (string= lo "en") (string= hi "GB")) "en-GB")
           ((and (string= lo "en") (string= hi "US")) "en-US")
           ((and (string= lo "de") (string= hi "DE")) "de")
           ((and (string= lo "fr") (string= hi "FR")) "fr")
           (t lo))))
       (t x)))))

(defun rm/tex--scan-buffer-for-language (&optional buffer)
  "Prøv å finne språk i BUFFER (default current) via babel/polyglossia/magic."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let (lang)
          ;; % !TeX spellcheck = nb_NO  eller  % !TeX language = norwegian
          (when (re-search-forward
                 "^%\\s-*!TeX\\s-+\\(spellcheck\\|language\\)\\s-*=\\s-*\\(.+\\)$" nil t)
            (setq lang (rm/tex--normalize-lang (string-trim (match-string 2)))))
          ;; \usepackage[<opts>]{babel}
          (unless lang
            (goto-char (point-min))
            (when (re-search-forward "\\\\usepackage\\[\\([^]]+\\)\\]{\\s-*babel\\s-*}" nil t)
              (let* ((opts (match-string 1))
                     (first-opt (car (split-string opts "[, ]+" t))))
                (setq lang (rm/tex--normalize-lang first-opt)))))
          ;; polyglossia: \setdefaultlanguage{<lang>}
          (unless lang
            (goto-char (point-min))
            (when (re-search-forward "\\\\setdefaultlanguage{\\([^}]+\\)}" nil t)
              (setq lang (rm/tex--normalize-lang (match-string 1)))))
          lang)))))

(defun rm/tex--detect-language ()
  "Heuristisk: buffer → master → ispell."
  (or
   ;; a) direkte i denne fila
   (rm/tex--scan-buffer-for-language)
   ;; b) i TeX-master (om satt)
   (let* ((master TeX-master)
          (file (cond
                 ((stringp master) master)          ; eksplisitt fil
                 ((eq master t) (buffer-file-name)) ; denne fila er master
                 (t nil))))
     (when (and file (file-readable-p file)
                (not (string= (file-truename file)
                              (and (buffer-file-name) (file-truename (buffer-file-name))))))
       (with-temp-buffer
         (insert-file-contents file nil 0 4096) ; les litt (holder for pakke/ preamble)
         (rm/tex--scan-buffer-for-language (current-buffer)))))
   ;; c) fall-back til ispell-ordbok
   (rm/tex--normalize-lang (or ispell-local-dictionary ispell-current-dictionary))))

(defun rm/tex-apply-quotes-based-on-language ()
  "Sett TeX-anførselstegn i LaTeX-buffere basert på heuristisk språkdeteksjon."
  (when (derived-mode-p 'latex-mode)
    (let* ((lang (rm/tex--detect-language))
           (pair (cdr (assoc lang rm/tex-quote-map))))
      (when pair
        (setq-local TeX-open-quote  (car pair))
        (setq-local TeX-close-quote (cdr pair))
        ;; Om ønskelig:
        ;; (setq-local TeX-quote-after-quote t)
        ))))

;; Kjør når LaTeX åpnes og når ordbok endres
(add-hook 'LaTeX-mode-hook #'rm/tex-apply-quotes-based-on-language)
(add-hook 'ispell-change-dictionary-hook #'rm/tex-apply-quotes-based-on-language)

;; Valgfritt: oppdater også ved lagring, i tilfelle du nettopp la til \usepackage[..]{babel}
(add-hook 'before-save-hook #'rm/tex-apply-quotes-based-on-language)

(add-hook 'LaTeX-mode-hook #'rm/tex-setup-quote-auto)

(provide 'LaTeX-quote-hacks)
