;;; -*- lexical-binding: t -*-

(message "init.el av %s"  (format-time-string "%Y-%m-%d %H:%M:%S" (nth 5 (file-attributes (buffer-file-name)))))

(use-package exec-path-from-shell
  :ensure t)

(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; (add-to-list
;;  'package-archives
;;  '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list
;;  'package-archives
;;  '("melpa" . "https://melpa.org/packages/") t)

;; (package-initialize)

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/Library/TeX/texbin/:" (getenv "PATH")))

(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(defun get-default-height ()
       (/ (display-pixel-height)
          (frame-char-height)))

(add-to-list 'default-frame-alist '(width . 80))
(add-to-list 'default-frame-alist (cons 'height (get-default-height)))

(global-unset-key (kbd "C-z"))

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t)

(require 'tree-sitter)
(require 'tree-sitter-langs)

(setq mac-option-key-is-meta nil)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(setq treesit-extra-load-path
      '("/Users/roffe/Library/Application Support/tree-sitter"))

(mapc #'frame-width
	(frames-on-display-list))

(add-hook 'before-make-frame-hook
          #'(lambda ()
              (let* ((all-frames (frames-on-display-list))
                     (rightmost-left 0))
                (dolist (frame all-frames)
                  (let ((frame-left (frame-parameter frame 'left))
                        (frame-width (frame-pixel-width frame)))
                    (setq rightmost-left
                          (max rightmost-left (+ frame-left frame-width)))))
                (add-to-list 'default-frame-alist `(left . ,rightmost-left))
                (add-to-list 'default-frame-alist '(top . 0)))))

(add-to-list 'default-frame-alist
             `(height . ,(/  (display-pixel-height) (line-pixel-height))))

(dolist (path '("/usr/local/bin" "/opt/homebrew/bin" "/Library/TeX/texbin"))
  (setenv "PATH" (concat path ":" (getenv "PATH"))))

(setenv "LC_ALL" "en_US.UTF-8")

(setq tramp-default-method "sshx")

(defun resolve-jobname-to-actual-filename ()
  "Hvis dokumentet bruker \\jobname i \\addbibresource, erstatt det med faktisk filnavn for RefTeX sin skyld."
  (when (and (buffer-file-name)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward
		"\\\\addbibresource{\\\\jobname\\.bib}" nil t)))
    (let* ((basename (file-name-base (buffer-file-name)))
           (actual-bib (concat basename ".bib"))
           (tex-file (buffer-file-name))
           (bib-dir (file-name-directory tex-file))
           (bib-path (expand-file-name actual-bib bib-dir)))
      (if (file-exists-p bib-path)
          (progn
            (message "RefTeX hack: Mapper \\jobname.bib til %s" bib-path)
            (setq-local reftex-extra-bindings
                        (list (cons "\\jobname.bib" bib-path))))
        (message "Advarsel: Kunne ikke finne bib-fil for \\jobname (%s)"
		 bib-path)))))

(add-hook 'LaTeX-mode-hook #'resolve-jobname-to-actual-filename)

(use-package web-mode
  :ensure t
  :mode "\\.php\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package rust-mode
  :ensure t
  :hook (rust-mode . cargo-minor-mode))


(use-package js2-mode
  :ensure nil                      ;; innebygget, ikke installer
  :hook (js2-mode . js2-imenu-extras-mode)
  :config
    (setq js-indent-level   2
          indent-tabs-mode  nil
          standard-indent   2))

;; For React/JSX:
(use-package rjsx-mode
  :mode "\\.jsx?\\'"
  :interpreter "node")

(use-package polymode
  :ensure t
  :config
  (setq polymode-exporter-output-file-format "%s"
        polymode-weaver-output-file-format "%s"))

(use-package ess
  :ensure t
  :config
  (setq ess-style 'GNU
        inferior-ess-r-program "/Library/Frameworks/R.framework/Versions/Current/Resources/bin/R"))

(defun sync-bibinputs-with-texlive ()
  "Synkroniser Emacs sin BIBINPUTS med TeXLive sin via kpsewhich."
  (let ((bibpath (string-trim (shell-command-to-string "kpsewhich -show-path=bib"))))
    (setenv "BIBINPUTS" bibpath)))

(defun my/LaTeX-setup ()
  "Alt jeg vil gjøre når jeg åpner en LaTeX-fil."
  (LaTeX-add-environments
             '("letter" LaTeX-env-args ["Address"] 0))
  (turn-on-reftex)
  (sync-bibinputs-with-texlive)
  (resolve-jobname-to-actual-filename)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-cite-format 'biblatex))  ;; siden du bruker addbibresource

(use-package auctex
  :ensure t
  :defer t
  :hook (LaTeX-mode . my/LaTeX-setup)
  :config
  (setq TeX-engine 'xetex
        TeX-tree-roots '("/usr/local/texlive/2025")))

(use-package reftex
  :ensure nil  ;; følger med Emacs
  :after auctex)  ;; ikke nødvendig med mer hook her – vi styrer alt via my/LaTeX-setup

(use-package lua-mode
  :ensure t
  :config
  (setq lua-default-application "/opt/homebrew/bin/lua"))

(use-package python
  :ensure t
  :config
  (setq python-indent-offset 2
        python-interpreter "/opt/homebrew/bin/python3"
        python-shell-interpreter "/opt/homebrew/bin/python3"))

(use-package ace-window
  :ensure t)

;; (use-package sly
;;   :ensure t
;;   :config
;;   :hook (lambda ()
;;             (unless (sly-connected-p)
;;               (save-excursion (sly))))
;;   (setq inferior-lisp-program "/opt/homebrew/bin/sbcl"))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

;; === Desktop ===

;; Hvor du vil ha desktop-fila
(setq desktop-dirname             (expand-file-name "~/.emacs.d/desktop/")
      desktop-path                (list desktop-dirname)
      desktop-base-file-name      "emacs-desktop"
      desktop-base-lock-name      "emacs-desktop.lock"
      desktop-save                t          ; spør ikke, bare lagre
      desktop-load-locked-desktop nil)

;; Sørg for at katalogen finnes
(make-directory desktop-dirname t)

;; Slå på desktop-lagring
(desktop-save-mode 1)

;; === Magit-mode ===

;; --- Magit-meny kun i git-repo-buffere (robust via minor mode) ---
(require 'easymenu)

(defun roffe/in-git-p (&optional dir)
  "Ikke-nil hvis DIR (eller `default-directory`) ligger i et git-repo."
  (let ((d (file-name-as-directory (or dir default-directory))))
    (locate-dominating-file d ".git")))

;; Keymap for minor mode, inneholder selve Magit-menyen
(defvar roffe-magit-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define roffe/magit-menu map "Magit"
      '("Magit"
        ["Status" magit-status t]
        ["Log All" magit-log-all t]
        ["Commit" magit-commit-create t]
        ["Push" magit-push-current-to-pushremote t]
        ["Pull" magit-pull-from-upstream t]
        ["Blame" magit-blame-addition t]
        ["Quit Magit Buffers" magit-mode-bury-buffer t]))
    map)
  "Keymap som gir en Magit-meny i menylinja når minor mode er aktiv.")

(define-minor-mode roffe-magit-menu-mode
  "Vis Magit-meny i denne bufferen."
  :lighter ""
  :keymap roffe-magit-menu-mode-map)

(defun roffe/magit-menu-refresh ()
  "Skru på/av Magit-meny i current buffer basert på om den er i et git-repo."
  (if (roffe/in-git-p)
      (roffe-magit-menu-mode 1)
    (roffe-magit-menu-mode 0)))

;; Oppdater ved filåpning, bufferbytte og i dired
(add-hook 'find-file-hook #'roffe/magit-menu-refresh)
(add-hook 'buffer-list-update-hook #'roffe/magit-menu-refresh)
(add-hook 'dired-mode-hook #'roffe/magit-menu-refresh)

;; Magit autoloades når du bruker et menypunkt
(use-package magit
  :ensure t
  :commands (magit-status magit-log-all magit-commit-create
             magit-push-current-to-pushremote magit-pull-from-upstream
             magit-blame-addition magit-mode-bury-buffer))



;; === YAML i Emacs med LSP og schema-støtte ===
;; Forutsetter Emacs 29+ (for yaml-ts-mode). Fungerer også med yaml-mode.

;; 1) Grunnleggende LSP
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  ;; mindre støy
  (setq lsp-log-io nil
        lsp-headerline-breadcrumb-enable nil
        lsp-idle-delay 0.2
        lsp-completion-provider :capf)
  :config
  ;; enkel sjekk: si fra hvis yaml-language-server mangler
  (unless (executable-find "yaml-language-server")
    (message "Tips: npm i -g yaml-language-server"))
  ;; bruk standard nøkkelbindinger kun i lsp-buffere
  (lsp-enable-which-key-integration t))

;; 2) Innrykk-guider (nyttig for YAML)
(use-package highlight-indent-guides
  :hook (yaml-ts-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; 3) YAML major mode – velg én av disse:

;; a) Emacs 29+: yaml-ts-mode (anbefales)
(use-package yaml-ts-mode
  :mode ("\\.ya?ml\\'" . yaml-ts-mode)
  :hook ((yaml-ts-mode . lsp-deferred)
         (yaml-ts-mode . highlight-indent-guides-mode)))

;; b) Hvis du trenger klassisk yaml-mode i stedet:
;; (use-package yaml-mode
;;   :mode ("\\.ya?ml\\'" . yaml-mode)
;;  :hook ((yaml-mode . lsp-deferred)
;;         (yaml-mode . highlight-indent-guides-mode)))

;; 4) Skjema/validering for GitHub Actions + annet
(with-eval-after-load 'lsp-mode
  ;; Koble YAML-filer til riktige schema.
  ;; Flere schema finnes på https://www.schemastore.org/json/
  (setq lsp-yaml-schemas
        ;; format: SCHEMA -> GLOB
        '( :https://json.schemastore.org/github-workflow.json  "/.github/workflows/*"
           :https://json.schemastore.org/github-action.json   "/.github/actions/*")
          ;; legg til egne behov her:
          ;; ( "https://json.schemastore.org/kubernetes.json" . "/*.k8s.yaml")
          )
  ;; YAML-innstillinger
  (setq lsp-yaml-format-enable t           ; autoformat fra språkserveren
        lsp-yaml-key-ordering nil          ; ikke tving nøkkelrekkefølge
        lsp-yaml-completion t))

;; 5) Valgfritt: automatisk formatering ved lagring kun for YAML
(defun roffe/yaml-format-buffer-maybe ()
  (when (bound-and-true-p lsp-mode)
    (lsp-format-buffer)))
(add-hook 'yaml-ts-mode-hook
          (lambda () (add-hook 'before-save-hook #'roffe/yaml-format-buffer-maybe nil t)))

(defun rb/straight-update-and-maybe-rebuild (pkg)
  "Pull straight PKG. Rebuild bare hvis det kom nye commits.
PKG skal være et symbol som f.eks. 'min-elisp."
  (interactive (list (intern (completing-read "Package: " (hash-table-keys straight--recipe-cache)))))
  (let* ((repo-dir (straight--repos-dir (symbol-name pkg)))
         (default-directory repo-dir)
         (before (string-trim (shell-command-to-string "git rev-parse HEAD"))))
    (straight-pull-package pkg)
    (let* ((after (string-trim (shell-command-to-string "git rev-parse HEAD"))))
      (if (equal before after)
          (message "%s er allerede oppdatert (%s)" pkg after)
        (straight-rebuild-package pkg)
        (message "%s oppdatert til %s og rebygd" pkg after)))))

(straight-use-package 'tsc)

(use-package ws-butler
  :straight (ws-butler
	     :type git
	     :host github
	     :repo "lewang/ws-butler"
	     :branch "master")
  :hook (prog-mode . ws-butler-mode))


(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

(push "/usr/local/src/eplot/" load-path)
(autoload 'eplot "eplot" nil t)
(autoload 'eplot-mode "eplot" nil t)
(unless (assoc "\.plt" auto-mode-alist)
  (setq auto-mode-alist (cons '("\.plt" . eplot-mode) auto-mode-alist)))

(setq treesit-language-source-alist
      '((typescript
         "https://github.com/tree-sitter/tree-sitter-typescript"
         "master"
         "typescript/src")
        (tsx
         "https://github.com/tree-sitter/tree-sitter-typescript"
         "master"
         "tsx/src")))

(use-package emacs-hacks
  :straight (:host github :repo "phimuemue/emacs-hacks"
		   :files ("flyspell-babel.el" "ispell-multi.el"))
  :hook (latex-mode . flyspell-babel-setup)
  :commands (flyspell-babel-setup))


(use-package anddo
  :straight (:host github :repo "larsmagne/anddo"
		   :files ("anddo.el")))


(use-package ispell-multi
  :straight (:host github :repo "phimuemue/emacs-hacks"
		   :files ("ispell-multi.el")))


(use-package min-elisp
  :straight (min-elisp
             :type git
             :host github
             :repo "rolfmblindgren/min-elisp"
             ;; :branch "main"         ; hvis du ikke bruker default
             :files ("*.el" "lisp/*.el") ; om koden ligger i undermapper
             )
  :commands (min-elisp-start)   ; autoload-kommandoer (frivillig)
  :init
  ;; konfig før load (frivillig)
  :config
  ;; konfig etter load
  (message "min-elisp loaded"))

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
(modify-coding-system-alist 'process "R" 'utf-8-unix)

(add-to-list 'text-mode-hook 'flyspell-mode-on)

(require 'LaTeX-quote-hacks)

(load custom-file)

(provide 'init)

;;;
