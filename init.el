
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init use-package

(require 'package)
(setq
 use-package-always-ensure t
 package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                    ("org" . "https://orgmode.org/elpa/")
                    ("melpa" . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/")))


; FIXME: is it still needed?
(when (version< emacs-version "26")
    (package-initialize))
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Striping & Defaults

(setq inhibit-startup-screen t)

(setq frame-title-format '("" "%b @ Emacs " emacs-version))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(fset `yes-or-no-p `y-or-n-p)

(setq-default indent-tabs-mode nil)

(set-frame-font "Source Code Pro")

;(setq-default browse-url-browser-function 'eww-browse-url)
(setq-default browse-url-browser-function 'browse-url-firefox)

(column-number-mode)

(setq-default fill-column 100)
(auto-fill-mode)

;; word boundaries
(modify-syntax-entry ?_ "w")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup & Undo

(use-package undo-tree
  :diminish
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `(("." . "~/.emacs.d/undotree")))
  (setq delete-old-versions t
        backup-directory-alist `(("." . "~/.emacs.d/saves"))
        backup-by-copying t
        kept-new-versions 6
        kept-old-versions 2
        version-control t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes

(use-package molokai-theme
  :config
  ;(load-theme 'molokai t)
  )

(use-package badwolf-theme
  :config
  (load-theme 'badwolf t))

;(use-package solarized-theme
;  :init
;  (progn
;    (setq solarized-distinct-fringe-background nil))
;  :config
;  (load-theme 'solarized-dark t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modeline

(use-package diminish
  :config
  (diminish 'eldoc-mode))

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/pre-minor-modes-separator " ")
  (sml/setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil

(use-package evil
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-normal-state-map (kbd "<XF86Tools>") 'evil-insert)
  (define-key evil-insert-state-map (kbd "<XF86Tools>") 'evil-force-normal-state))

(use-package evil-leader
  :config
  (progn
    (evil-leader/set-leader "SPC")
    (global-evil-leader-mode)
    ; kill buffer to create a new one with evil
    ; https://github.com/cofi/evil-leader/issues/10#issuecomment-31290512
    (kill-buffer "*Messages*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm

; show only header
(defun helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (helm-mode))
  :bind ("M-x" . helm-M-x)
  :config
  (progn
  (setq helm-echo-input-in-header-line t
        helm-display-header-line nil
        helm-always-two-windows t
        ;helm-follow-mode-persistent t
        helm-M-x-fuzzy-match t)
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  (evil-leader/set-key
   "b" 'helm-mini
   "fb" 'helm-bookmarks
   "ff" 'helm-find-files)))

(defun helm-do-ag-project-root-insert-at-point ()
  (interactive)
  (let ((helm-ag-insert-at-point 'symbol))
    (helm-do-ag-project-root)))

;; from spacemacs
(defun resume-last-search-buffer ()
  "open last helm-ag or hgrep buffer."
  (interactive)
  (cond ((get-buffer "*helm ag results*")
         (switch-to-buffer-other-window "*helm ag results*"))
        ((get-buffer "*helm-ag*")
         (helm-resume "*helm-ag*"))
        ((get-buffer "*hgrep*")
         (switch-to-buffer-other-window "*hgrep*"))
        (t
         (message "No previous search buffer found"))))


(use-package helm-ag
  :config
  (evil-define-key 'normal helm-ag-mode-map
    (kbd "RET") 'helm-ag-mode-jump-other-window
    "gr" 'helm-ag--update-save-results)
  (evil-leader/set-key
    "?" 'resume-last-search-buffer
    "/" 'helm-do-ag-project-root
    "." 'helm-do-ag-project-root-insert-at-point))

(use-package helm-swoop
  :config
  (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search))

; eshell-helm hacks
; https://github.com/emacs-helm/helm/wiki/Eshell
(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
            (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile & recentf

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)
  (add-to-list 'projectile-globally-ignored-directories ".ensime_cache")
  (add-to-list 'projectile-globally-ignored-directories "ltximg")
  (add-to-list 'projectile-globally-ignored-files ".ensime"))

(use-package helm-projectile
 :config (evil-leader/set-key
   "pf" 'helm-projectile-find-file
   "pp" 'helm-projectile-switch-project))

(use-package recentf
  :config
  (recentf-mode t)
  (add-to-list 'recentf-exclude "\\.ensime"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Octave

(use-package octave
  :mode ("\\.m\\'" . octave-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scala

(defun scala-settings ()
  (setq ensime-sem-high-faces
	'((var . scala-font-lock:var-face)
	  (val . (:inherit font-lock-constant-face :slant italic))
	  (varField . scala-font-lock:var-face)
	  (valField . (:inherit font-lock-constant-face :slant italic))
	  (functionCall . font-lock-function-name-face)
	  (operator . font-lock-keyword-face)
	  (param . (:slant italic))
	  (class . font-lock-type-face)
	  (trait .  (:inherit font-lock-type-face :slant italic))
	  (object . font-lock-constant-face)
	  (package . font-lock-preprocessor-face)
	  (implicitConversion . nil) ;fixed
	  (implicitParams . nil) ;fixed
	  (deprecated . (:strike-through "dark gray")))

	ensime-implicit-gutter-icons nil

	ensime-auto-generate-config t

	ensime-company-case-sensitive t

	ensime-search-interface 'helm

	ensime-startup-notification nil
	ensime-startup-snapshot-notification nil

        ensime-eldoc-hints 'type

	scala-indent:use-javadoc-style t
	scala-indent:default-run-on-strategy scala-indent:reluctant-strategy
	scala-indent:align-forms nil
	scala-indent:align-parameters nil

	;; layers/+lang/scala/config.el
	scala-auto-insert-asterisk-in-comments t

	flycheck-scalastyle-jar "~/bin/scalastyle_2.11-0.6.0.jar"
	flycheck-scalastylerc "scalastyle-config.xml"

        ;; workaround reread prompt: https://github.com/ensime/ensime-emacs/issues/678
        revert-without-query (quote (".*/\\.ensime_cache/dep-src/source-jars/.*")))

  (ensime-mode))

(use-package ensime
  :pin melpa
  :config
  (progn
    (evil-leader/set-key
      "rt" 'ensime-import-type-at-point
      "ht" 'ensime-print-type-at-point
      "gg" 'ensime-edit-definition
      "cc" 'ensime-sbt-do-compile)
    (add-hook 'scala-mode-hook 'scala-settings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go

(defun my-go-mode-hook ()
  ; eldoc shows the signature of the function at point in the status bar
  (go-eldoc-setup)
  ; Call Gofmt before saving                                                    
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding                                                      
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark))

(use-package flycheck-gometalinter
  :ensure t
  :config
  (setq flycheck-gometalinter-disable-linters '(
                                                "structcheck"
                                                "deadcode"
                                                "golint"
                                                "gas"
                                                "errcheck"
                                                "gocyclo"
                                                "goconst"
                                                "vetshadow"
                                                "gotype"
                                                "varcheck"
                                                "gotypex"
                                                "maligned"))
  (setq flycheck-gometalinter-vendor t)
  (setq flycheck-gometalinter-tests t)
  (setq flycheck-gometalinter-fast t)
  (setq flycheck-gometalinter-deadline "10s")
  (flycheck-gometalinter-setup)
  ;; change (flycheck-gometalinter-setup) with the following lines to use metalinter
  ;; after other linters
  ;; should be added to the end (t is append)
  ;; (add-to-list 'flycheck-checkers 'gometalinter t)
  ;; (flycheck-add-next-checker 'go-build 'gometalinter)
  )

(use-package go-mode
  :config

  ;; https://github.com/dominikh/go-mode.el/pull/233
  (defun go-packages-native ()
    "Return a list of all installed Go packages.
It looks for archive files in /pkg/."
    (sort
     (delete-dups
      (cl-mapcan
       (lambda (pkgdir)
         (cl-mapcan (lambda (dir)
                      (mapcar (lambda (file)
                                (let ((sub (substring file (length pkgdir) -2)))
                                  (mapconcat #'identity (cdr (split-string sub "/")) "/")))
                              (if (file-directory-p dir)
                                  (directory-files dir t "\\.a$")
                                (if (string-match-p "\\.a$" dir)
                                    `(,dir)))))
                    (if (file-directory-p pkgdir)
                        (append (directory-files pkgdir t "\\.a$") (go--directory-dirs pkgdir)))))
       (apply 'append
              (mapcar (lambda (dir)
                        (delete nil (let ((pkgdir (concat dir "/pkg")))
                                      (mapcar (lambda (sub)
                                                (unless (or (string-match-p
                                                             "\\(dep\\|race\\|dyn\\|shared\\|include\\|obj\\|tool\\)"
                                                             sub)
                                                            (member sub '("." ".."))) (concat pkgdir "/" sub)))
                                              (directory-files pkgdir nil nil t))))) (go-root-and-paths)))))
     #'string<))

  
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package go-guru)

(use-package go-rename)

(use-package company-go
  :config
  (setq company-go-show-annotation t)
  (add-to-list 'company-backends 'company-go))

(use-package go-stacktracer)

(use-package go-direx)

(use-package go-add-tags
  :config
  (setq go-add-tags-style 'lower-camel-case))

(use-package go-impl)

(use-package go-fill-struct)

(use-package go-eldoc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript

(use-package dockerfile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript

(use-package js2-mode
  :mode ("\\.js$" "\\.es6$")
  :config
  (add-hook 'js2-mode-hook
            (lambda ()
              (progn
                (setq js2-basic-offset 4)
                (setq js-switch-indent-offset 4)
                (setq js2-strict-missing-semi-warning nil)))))

(use-package js2-refactor)

;; (use-package tern
;;   :diminish " T"
;;   :commands (tern-mode)
;;   :init (progn
;;           (add-hook 'js2-mode-hook 'tern-mode)
;;           (add-hook 'web-mode-hook 'tern-mode)))

;; (use-package company-tern
;;             :config (progn
;;                       (add-to-list 'company-backends 'company-tern)))

(use-package indium
  :config (add-hook 'js2-mode-hook #'indium-interaction-mode))

(use-package web-mode
  :mode ("\\.tsx$" "\\.jsx$")
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package tide
  :init
  (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
  :config
  (add-hook 'typescript-mode-hook #'tide-setup)
  (add-hook 'js2-mode-hook #'tide-setup)
  (add-hook 'web-mode-hook #'tide-setup))

(use-package json-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit

(use-package magit
  :config (evil-leader/set-key
    "g s" 'magit-status))

(use-package evil-magit)

;(use-package magithub
;  :after magit
;  :ensure t
;  :config (magithub-feature-autoinject t))

(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
(setq-default ediff-prefer-iconified-control-frame t)

(use-package git-timemachine
  :config
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows

(evil-leader/set-key
  "wh" 'evil-window-left
  "wl" 'evil-window-right
  "wj" 'evil-window-down
  "wk" 'evil-window-up)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation

(defun compile-noask ()
  (interactive)
  (compile compile-command))

;(setq compilation-read-command nil)
(setq compilation-scroll-output 'first-error)
(global-set-key "\C-x\C-m" 'compile)
(evil-leader/set-key
  "cl" 'flycheck-list-errors
  "cn" 'next-error
  "cp" 'previous-error
  "cc" 'compile-noask
  "cr" 'recompile
  "ce" 'next-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; F-keys

(global-set-key (kbd "<f3>") (lambda () (interactive) (find-file "~/org/work.org")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init file

(defun find-user-init-file ()
  (interactive)
  (find-file-existing user-init-file))

(defun reload-user-init-file ()
  (interactive)
  (load-file user-init-file))

(evil-leader/set-key
  "fed" 'find-user-init-file
  "feR" 'reload-user-init-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smartparens

(defmacro def-pairs (pairs)
  `(progn
     ,@(loop for (key . val) in pairs
          collect
            `(defun ,(read (concat
                            "wrap-with-"
                            (prin1-to-string key)
                            "s"))
                 (&optional arg)
               (interactive "p")
               (sp-wrap-with-pair ,val)))))

(def-pairs ((paren        . "(")
            (bracket      . "[")
            (brace        . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote   . "`")))

(defun sp-kill-hybrid-sexp-and-insert (arg)
  (interactive "P")
  (sp-kill-hybrid-sexp arg)
  (evil-insert 1))

(use-package smartparens
  :diminish "S"
  :config
  (require 'smartparens-scala)
  (add-to-list 'sp-sexp-suffix (list 'js2-mode 'regexp "")) ;like smartparens-scala
  (add-to-list 'sp-sexp-suffix (list 'go-mode 'regexp ""))
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
  (add-hook 'scala-mode-hook 'smartparens-strict-mode)
  (add-hook 'js2-mode-hook 'smartparens-strict-mode)
  (add-hook 'go-mode-hook 'smartparens-strict-mode)
  (evil-define-key 'normal evil-smartparens-mode-map
    (kbd "D") #'sp-kill-hybrid-sexp
    (kbd "C") #'sp-kill-hybrid-sexp-and-insert)
  (bind-keys
   :map smartparens-mode-map
   ("M-]" . sp-unwrap-sexp)
   ("M-[" . sp-backward-unwrap-sexp)
   ("C-<right>" . sp-slurp-hybrid-sexp)
   ("M-<right>" . sp-backward-barf-sexp)
   ("C-<left>"  . sp-backward-slurp-sexp)
   ("M-<left>"  . sp-forward-barf-sexp)
   ("C-k (" . wrap-with-parens)
   ("C-k [" . wrap-with-brackets)
   ("C-k {" . wrap-with-braces)
   ("C-k '" . wrap-with-single-quotes)
   ("C-k \"" . wrap-with-double-quotes)
   ("C-k t" . sp-transpose-hybrid-sexp)
   ("C-k p" . sp-push-hybrid-sexp))

  (setq sp-highlight-pair-overlay nil)
  (sp-local-pair 'go-mode "{" nil :post-handlers '(("||\n[i]" "RET"))))

(use-package evil-smartparens
  :diminish
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

; always show mathing paren
(setq show-paren-delay 0)
(show-paren-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company

(use-package company
  :diminish
  :config
  ; http://emacs.stackexchange.com/questions/10837/how-to-make-company-mode-be-case-sensitive-on-plain-text
  (setq company-dabbrev-downcase nil)
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .2)
  (global-company-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet

(use-package yasnippet
  :config
  (yas-global-mode)
  (diminish 'yas-minor-mode))

(use-package yasnippet-snippets)

(use-package auto-yasnippet
  :after yasnippet
  :config
  (defun aya-expand-and-insert ()
    (interactive)
    (evil-insert 1)
    (aya-expand))

  ; switch to insert mode when snippet is expanded from visual mode
  (add-hook 'yas-before-expand-snippet-hook
            #'(lambda()
                (when (evil-visual-state-p)
                  (let ((p (point))
                        (m (mark)))
                    (evil-insert-state)
                    (goto-char p)
                    (set-mark m)))))

  (evil-leader/set-key
    "yy" 'aya-expand-and-insert
    "yc" 'aya-create))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avy

(use-package avy
  :config
  (evil-leader/set-key
    "ac" 'avy-goto-char-timer
    "aw" 'avy-goto-word-1
    "al" 'avy-goto-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck

(use-package flycheck
  :diminish "F"
  :config
  (global-flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restclient

(use-package restclient)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multitran

(use-package multitran)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protobuf

(use-package protobuf-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yaml

(use-package yaml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CockroachDB
;; Turn Postgres into Cockroach
;; https://logicgrimoire.wordpress.com/2018/04/27/how-to-use-cockroachdb-with-emacs-sql-mode/

(defun sql-cockroach ()
  (interactive)
  (require 'sql)
  (setq sql-postgres-program "cockroach")
  (setq sql-postgres-options
        '("sql" "--insecure"))
  (setq sql-postgres-login-params
        '((user :default "root")
          (database :default "")
          (server :default "localhost")
          (port :default 26257)))
  (defun sql-comint-cockroach (product options &optional buf-name)
    "Create comint buffer and connect to CockroachDB."
    (let ((params
           (append
            options
            (if (not (= 0 sql-port))
                (list "--port" (number-to-string sql-port)))
            (if (not (string= "" sql-user))
                (list "--user" sql-user))
            (if (not (string= "" sql-server))
                (list "--host" sql-server))
            (if (not (string= "" sql-database))
                (list "--database" sql-database)))))
      (sql-comint product params)))
  (sql-set-product-feature 'postgres
                           :sqli-comint-func #'sql-comint-cockroach)
  (sql-set-product-feature 'postgres
                           :prompt-regexp "^[a-z]+\@[a-zA-Z0-9\.-_]+:[0-9]+/\\([a-z]+\\)?> ")
  (sql-set-product-feature 'postgres
                           :prompt-length 0)
  (sql-set-product-feature 'postgres
                           :prompt-cont-regexp "^ +-> ")
  (sql-set-product-feature 'postgres
                           :terminator '(";" . ";"))
  (sql-set-product-feature 'postgres
                           :list-all "SHOW TABLES;")
  (sql-postgres))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org

(setq-default org-startup-indented t)
(setq-default org-src-fontify-natively t)

(setq org-preview-latex-default-process 'dvisvgm)

(use-package org
  :ensure org-plus-contrib
  :config
  (require 'ox)
  (require 'org-drill)
  (setq org-agenda-files (quote ("~/Dropbox/org/events.org"))))

(use-package org-evil)

;; this package maps too much (J, for instance)
;(use-package evil-org)

(use-package org-pomodoro)

;; guaranteed kill
(diminish 'auto-revert-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "31b2145c933e41fbbda48b15278cdcce3779db7e92ca434ad3044b3392ad6ae3" default))
 '(package-selected-packages
   '(flycheck-gometalinter go-rename go-eldoc emacs-go-eldoc magithub yasnippet-snippets go-fill-struct build-status badwolf-theme go-impl godoctor yaml-mode avy go-add-tags indium company-go go-mode json-mode web-mode use-package tide smart-mode-line restclient php-mode org-pomodoro org-evil multitran monokai-theme molokai-theme js2-refactor helm-swoop helm-projectile helm-ag git-timemachine evil-smartparens evil-magit evil-leader ensime company-tern go-guru go-direx go-stacktracer dockerfile-mode auto-yasnippet protobuf-mode org-plus-contrib diminish)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
