
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init use-package

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(setq straight-host-usernames '((github . "chessman")))
(setq straight-vc-git-force-protocol t)

(straight-use-package 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Striping & Defaults
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq inhibit-startup-screen t)

(setq frame-title-format '("" "%b @ Emacs " emacs-version))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(fset `yes-or-no-p `y-or-n-p)

(setq history-delete-duplicates t)

(setq-default indent-tabs-mode nil)

(set-frame-font "FiraCode 11")
;(set-frame-font "Hack")
;(set-frame-font "Source Code Pro")

;(setq-default browse-url-browser-function 'eww-browse-url)
(setq-default browse-url-browser-function 'browse-url-chrome)

(column-number-mode)

(setq-default fill-column 100)
(auto-fill-mode)

;(global-display-line-numbers-mode)
;(setq display-line-numbers-type 'relative)

;; word boundaries
(modify-syntax-entry ?_ "w")

(setq delete-old-versions t
      backup-directory-alist `(("." . "~/.emacs.d/saves"))
      backup-by-copying t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup & Undo

(use-package undo-tree
  :diminish
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `(("." . "~/.emacs.d/undotree")))
  ;(setq undo-tree-enable-undo-in-region nil)
  (global-undo-tree-mode))

  ;; https://github.com/scalacenter/bloop/issues/1088
  (setq create-lockfiles nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes

(use-package molokai-theme)
(use-package badwolf-theme)
(use-package gruvbox-theme)

(load-theme 'badwolf t)

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

;; (use-package smart-mode-line
;;   :config
;;   (setq sml/no-confirm-load-theme t)
;;   (setq sml/pre-minor-modes-separator " ")
;;   (sml/setup))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :bind (:map evil-normal-state-map
              ("M-." . nil))
  :config
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-set-undo-system 'undo-tree))

(use-package evil-leader
  :after evil
  :config
  (progn
    (evil-leader/set-leader "SPC")
    (global-evil-leader-mode)
                                        ; kill buffer to create a new one with evil
                                        ; https://github.com/cofi/evil-leader/issues/10#issuecomment-31290512
    (kill-buffer "*Messages*")))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init '(xref vterm magit edebug dired)))

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
  :bind (("M-x" . helm-M-x)
         (:map helm-buffer-map
           ("C-M-s-n" . helm-next-line)
           ("C-M-s-p" . helm-previous-line)))
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
  (setq helm-ag-base-command "rg --no-heading --sort path --smart-case")
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

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find treemacs-python-executable))))
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

(use-package which-key
  :config (which-key-mode))

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-terraform-server '("terraform-ls" "serve"))
  (setq lsp-keymap-prefix "C-l")
  ; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  :hook
  (scala-mode . lsp)
  (clojure-mode . lsp)
  (lsp-mode . lsp-lens-mode)
  (lsp-mode . lsp-enable-which-key-integration)
  :bind (:map global-map
        ;("M-/" . lsp-find-references)
        ("M-/" . lsp-ui-peek-find-references)
        ;("M-\\" . lsp-find-implementation)
        ("M-\\" . lsp-ui-peek-find-implementation)
        ("M-RET" . helm-lsp-code-actions))
  :config
  (setq lsp-restart 'ignore)
  (setq lsp-lens-enable t)
  (evil-leader/set-key
    "ss" 'helm-lsp-workspace-symbol
    "te" 'lsp-treemacs-errors-list
    "ts" 'lsp-treemacs-symbols))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-sideline-diagnostic-max-lines 10)
  ;(setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-alignment 'window))

(use-package lsp-metals
  :config
  (setq lsp-metals-show-implicit-arguments nil)
  (setq lsp-metals-show-inferred-type nil))

(use-package helm-lsp)
(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1))

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
)

(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  ;; :config
  ;;    (add-hook 'dap-stopped-hook
  ;;      (lambda (arg) (call-interactively #'dap-hydra))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go

(use-package go-mode)
(add-hook 'go-mode-hook #'lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java

(use-package lsp-java
  :after lsp
  :config (add-hook 'java-mode-hook 'lsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scala

(use-package scala-mode
  :mode ("\\.\\(scala\\|sbt\\|worksheet\\.sc\\|sc\\)\\'" . scala-mode))

;(add-hook 'scala-mode-hook (lambda ()
;                             (setq prettify-symbols-alist scala-prettify-symbols-alist)
;                             (prettify-symbols-mode)))

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
;; Clojure

(use-package clojure-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Docker

(use-package dockerfile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript

(use-package json-mode)

(use-package elm-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit

(use-package magit
  :config (evil-leader/set-key
    "g f" 'magit-file-dispatch
    "g s" 'magit-status))

(use-package forge)

(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
(setq-default ediff-prefer-iconified-control-frame t)

(add-hook 'ediff-load-hook (lambda ()
  (dolist (face-map '((diff-hl-insert              . magit-diff-added)
                      (diff-hl-change              . ediff-current-diff-C)
                      (diff-hl-delete              . magit-diff-removed)
                      (smerge-base                 . magit-diff-base)
                      (smerge-lower                . magit-diff-added)
                      (smerge-markers              . magit-diff-conflict-heading)
                      (smerge-refined-added        . magit-diff-added-highlight)
                      (smerge-refined-removed      . magit-diff-removed-highlight)
                      (smerge-upper                . magit-diff-removed)
                      (ediff-even-diff-A           . magit-diff-context-highlight)
                      (ediff-even-diff-Ancestor    . magit-diff-context)
                      (ediff-even-diff-B           . magit-diff-context-highlight)
                      (ediff-even-diff-C           . magit-diff-context-highlight)
                      (ediff-odd-diff-A            . magit-diff-context-highlight)
                      (ediff-odd-diff-Ancestor     . magit-diff-context)
                      (ediff-odd-diff-B            . magit-diff-context-highlight)
                      (ediff-odd-diff-C            . magit-diff-context-highlight)
                      (ediff-current-diff-A        . magit-diff-our)
                      (ediff-current-diff-Ancestor . magit-diff-base)
                      (ediff-current-diff-B        . magit-diff-their)
                      (ediff-fine-diff-A           . magit-diff-removed-highlight)
                      (ediff-fine-diff-Ancestor    . magit-diff-base-highlight)
                      (ediff-fine-diff-B           . magit-diff-added-highlight)
                      (diff-header                 . magit-diff-hunk-heading)
                      (diff-context                . magit-diff-context)
                      (diff-added                  . magit-diff-added)
                      (diff-removed                . magit-diff-removed)
                      (diff-changed                . smerge-refined-changed)
                      (diff-refine-added           . magit-diff-added-highlight)
                      (diff-refine-removed         . magit-diff-removed-highlight)
                      (diff-refine-changed         . ediff-fine-diff-C)
                      (diff-indicator-added        . magit-diffstat-added)
                      (diff-indicator-removed      . magit-diffstat-removed)))
    (let* ((face (car face-map))
           (alias (cdr face-map)))
      (put face 'theme-face nil)
      (put face 'face-alias alias)))))

(use-package git-timemachine
  :config
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows

(evil-leader/set-key
  "SPC" 'evil-window-next
  "wd" 'evil-window-delete
  "wh" 'evil-window-left
  "wl" 'evil-window-right
  "wj" 'evil-window-down
  "wk" 'evil-window-up)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation

(defun compile-noask ()
  (interactive)
  (compile compile-command))

;; use colors
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;(setq compilation-read-command nil)
(setq compilation-scroll-output 'first-error)
(setq compilation-skip-threshold 2)
(setq compilation-auto-jump-to-first-error t)
(global-set-key "\C-x\C-m" 'projectile-compile-project)
(global-set-key "\C-x\C-t" 'projectile-test-project)
(evil-leader/set-key
  ;; "cl" 'flymake-show-diagnostics-buffer
  ;; "cn" 'flymake-goto-next-error
  ;; "cp" 'flymake-goto-prev-error
  "cc" 'compile-noask
  "cr" 'recompile
  "ce" 'next-error)

;; regexp to match bloop output. without it, 'ant' regexp is used.  it works fine but doesn't support
;; separating warnings and column numbers.
;; note: ansi-color handles escape color sequences so that this matcher doesn't need to care about it.
;; why it is needed in modern LSP environment. there are two alternatives:
;; lsp-treemacs-errors-list - great UI but doesn't provide next-error.
;; flycheck-list-errors - good UI but lsp-mode notifies it only about errors of the current buffer.
(add-to-list 'compilation-error-regexp-alist-alist '(bloop "^\\[\\(?:\\(W\\)\\|E\\)\\] +\\[E[0-9]+\\] +\\([^: \t\n]+\\):\\([0-9]+\\):\\([0-9]+\\)$" 2 3 4 (1)))
  (add-to-list 'compilation-error-regexp-alist 'bloop)

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
     ,@(cl-loop for (key . val) in pairs
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
  :bind (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-M-s-n" . company-select-next)
        ("C-M-s-p" . company-select-previous))
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

(use-package jq-mode)

(use-package restclient
  :straight `(restclient :type git :host github :files ("restclient.el" "restclient-jq.el" "restclient-helm.el") :repo "pashky/restclient.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multitran

(use-package multitran)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protobuf

(use-package protobuf-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kubernetes

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package kubernetes-evil
  :ensure t
  :after kubernetes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yaml

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook #'lsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal

(use-package vterm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terraform

(use-package terraform-mode
  :config
  (add-hook 'terraform-mode-hook #'lsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org

(use-package org
  :ensure org-plus-contrib
  :init
    (setq org-startup-indented t)
    (setq org-startup-folded t)
    (setq org-src-fontify-natively t)
  :config
  (setq org-preview-latex-default-process 'dvisvgm))

;(use-package org-evil)

;; this package maps too much (J, for instance)
;(use-package evil-org)

(use-package org-pomodoro)

;; (use-package org-jira
;;   :config
;;   (setq request-log-level 'debug)
;;   (setq request-message-level 'debug)
;;   (setq jiralib-url "https://intertrusttechnologies.atlassian.net"))

(defun normal-state-and-save ()
  (interactive)
  (evil-force-normal-state)
  (save-buffer))

; Ergodox
(global-set-key (kbd "C-M-s-c") 'projectile-compile-project)
(global-set-key (kbd "C-M-s-l") 'evil-window-right)
(global-set-key (kbd "C-M-s-h") 'evil-window-left)
(global-set-key (kbd "C-M-s-<") 'xref-go-back)
(global-set-key (kbd "C-M-s->") 'xref-find-definitions)
(global-set-key (kbd "C-M-s-s") 'normal-state-and-save)
(global-set-key (kbd "C-M-s-b") 'helm-mini)
(global-set-key (kbd "C-M-s-u") 'sp-unwrap-sexp)
(global-set-key (kbd "C-M-s-r") 'lsp-rename)
(global-set-key (kbd "C-M-s-f") 'helm-lsp-code-actions)
(global-set-key (kbd "C-M-s-e") 'next-error)
(global-set-key (kbd "C-M-s-:") 'comment-dwim)
(global-set-key (kbd "C-M-s-m") 'magit-status)
(global-set-key (kbd "C-M-s-o") 'lsp-extend-selection)

;; guaranteed kill
(diminish 'auto-revert-mode)
