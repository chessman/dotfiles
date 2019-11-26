
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init use-package

(require 'package)
(setq
 use-package-always-ensure t
 package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                    ("org" . "https://orgmode.org/elpa/")
                    ("melpa" . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/")))

(when (not package-archive-contents)
  ;(package-refresh-contents)
  (package-initialize)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Striping & Defaults
(setq custom-file "~/.emacs.d/custom.el")

(setq inhibit-startup-screen t)

(setq frame-title-format '("" "%b @ Emacs " emacs-version))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(fset `yes-or-no-p `y-or-n-p)

(setq-default indent-tabs-mode nil)

(set-frame-font "Hack")
;(set-frame-font "Source Code Pro")

;(setq-default browse-url-browser-function 'eww-browse-url)
(setq-default browse-url-browser-function 'browse-url-chrome)

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

  ;; https://github.com/scalacenter/bloop/issues/1088
  (setq create-lockfiles nil)


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

(setq evil-want-keybinding nil)

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

(use-package evil-collection
  :config
  (evil-collection-init 'xref))

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
        helm-split-window-inside-p t ;for using helm in treemacs
        helm-buffer-max-length 40
        ;helm-follow-mode-persistent t
        helm-M-x-fuzzy-match t)
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  (evil-leader/set-key
   "?" 'helm-resume
   "b" 'helm-mini
   "fb" 'helm-bookmarks
   "ff" 'helm-find-files)))

(defun helm-do-ag-project-root-insert-at-point ()
  (interactive)
  (let ((helm-ag-command-option "--word-regexp")
     (helm-ag-insert-at-point 'symbol))
    (helm-do-ag-project-root)))

(use-package helm-ag
  :config
  (evil-define-key 'normal helm-ag-mode-map
    (kbd "RET") 'helm-ag-mode-jump-other-window
    "gr" 'helm-ag--update-save-results)
  (evil-leader/set-key
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

  (projectile-register-project-type 'bloop '(".bloop")
                                    :compile "bloop compile root"
                                    :test "bloop test --propagate --reporter scalac root"
                                    :src-dir "src/main/"
                                    :test-dir "src/test/"
                                    :test-suffix "Spec")

  (setq projectile-create-missing-test-files t)

  (add-to-list 'projectile-globally-ignored-directories ".bloop")
  (add-to-list 'projectile-globally-ignored-directories ".metals")
  (add-to-list 'projectile-globally-ignored-directories "ltximg"))

(use-package helm-projectile
 :config (evil-leader/set-key
   "pf" 'helm-projectile-find-file
   "pp" 'helm-projectile-switch-project))

(use-package recentf
  :config
  (recentf-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treemacs

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

  (evil-leader/set-key
    "tt" 'treemacs
    "tb" 'treemacs-bookmark
    "tf" 'treemacs-find-file))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Octave

(use-package octave
  :mode ("\\.m\\'" . octave-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP

(use-package lsp-mode
  :commands lsp
  :bind ("M-/" . lsp-find-references)
  :config
  (setq lsp-prefer-flymake nil)
  (evil-leader/set-key
    "ss" 'helm-lsp-global-workspace-symbol
    "te" 'lsp-treemacs-errors-list
    "ts" 'lsp-treemacs-symbols))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-position 'top)
  ;(setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-alignment 'window))

(use-package company-lsp)

(add-hook 'go-mode-hook #'lsp)
(add-hook 'scala-mode-hook #'lsp)

(use-package helm-lsp)
(use-package lsp-treemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scala

(defun complete-at-point ()
  (interactive)
  (evil-forward-word-end)
  (evil-append 1)
  (company-complete))

(use-package scala-mode
  :mode "^\w+\\.s\\(cala\\|bt\\)$"
  :bind
  (:map scala-mode-map
        ("M-RET" . complete-at-point)))

(add-hook 'scala-mode-hook (lambda ()
                             (setq prettify-symbols-alist scala-prettify-symbols-alist)
                             (prettify-symbols-mode)))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go

(defun my-go-mode-hook ()
  ; eldoc shows the signature of the function at point in the status bar
  (go-eldoc-setup)
  ; Call Gofmt before saving                                                    
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding                                                      
  (local-set-key (kbd "M-*") 'pop-tag-mark))

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
  (setq company-go-gocode-args '("-unimported-packages" "-builtin"))
  (add-to-list 'company-backends 'company-go))

;; (use-package company-tabnine
;;   :config
;;   (add-to-list 'company-backends #'company-tabnine))

(use-package go-stacktracer)

(use-package go-direx)

(use-package go-add-tags
  :config
  (setq go-add-tags-style 'lower-camel-case))

(use-package go-impl)

(use-package go-fill-struct)

(use-package go-eldoc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Docker

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

(use-package indium
  :config (add-hook 'js2-mode-hook #'indium-interaction-mode))

(use-package web-mode
  :mode ("\\.tsx$" "\\.jsx$")
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package json-mode)

(use-package elm-mode
  :config
(add-to-list 'company-backends 'company-elm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit

(use-package magit
  :config (evil-leader/set-key
    "g s" 'magit-status))

(use-package forge)
(use-package evil-magit)

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
  ;; "cl" 'flymake-show-diagnostics-buffer
  ;; "cn" 'flymake-goto-next-error
  ;; "cp" 'flymake-goto-prev-error
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
   ("C-k r" . sp-rewrap-sexp)
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

(use-package org-jira
  :config
  (setq request-log-level 'debug)
  (setq request-message-level 'debug)
  (setq jiralib-url "https://intertrusttechnologies.atlassian.net"))

;; guaranteed kill
(diminish 'auto-revert-mode)
