;;; init.el --- Initialization File -*- no-byte-compile: t; lexical-binding: nil; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init use-package

(setq straight-repository-branch "develop")
(setq straight-use-package-by-default t)
(setq straight-host-usernames '((github . "chessman")))
(setq straight-vc-git-force-protocol t)

 ; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

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

;; https://www.emacswiki.org/emacs/CopyAndPaste 
;; (setq wl-copy-process nil)
;; (defun wl-copy (text)
;;   (setq wl-copy-process (make-process :name "wl-copy"
;;                                       :buffer nil
;;                                       :command '("wl-copy" "-f" "-n")
;;                                       :connection-type 'pipe))
;;   (process-send-string wl-copy-process text)
;;   (process-send-eof wl-copy-process))
;; (defun wl-paste ()
;;   (if (and wl-copy-process (process-live-p wl-copy-process))
;;       nil ; should return nil if we're the current paste owner
;;       (shell-command-to-string "wl-paste -n")))
;; (setq interprogram-cut-function 'wl-copy)
;; (setq interprogram-paste-function 'wl-paste)

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

;; (use-package badwolf-theme
;;   :custom-face
;;   (font-lock-variable-use-face ((t (:inherit default))))
;;   (font-lock-function-call-face ((t (:inherit default))))
;;   (font-lock-property-use-face ((t (:inherit default)))))

(use-package gruvbox-theme)

(use-package catppuccin-theme)

;(load-theme 'catppuccin :no-confirm)
;(load-theme 'badwolf t)

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t)

(load-theme 'modus-vivendi-tritanopia :no-confirm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org

;; use built-in org
(require 'org)
(setq org-startup-indented t
      org-startup-folded t
      org-src-fontify-natively t
      org-default-notes-file (concat org-directory "/runes.org")
      org-preview-latex-default-process 'dvisvgm

      org-format-latex-options '(:foreground default :background Transparent :scale 0.5
		:html-foreground "Black" :html-background "Transparent"
		:html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
      org-capture-templates `(
        ("p" "Protocol" entry (file+headline ,(concat org-directory "/runes.org") "Papers")
         "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
        ("L" "Protocol Link" entry (file+headline ,(concat org-directory "/runes.org") "Papers")
         "* %?TODO [[%:link][%:description]] \nCaptured On: %U")))

(add-hook 'org-mode-hook (lambda ()
                           (auto-fill-mode)
                           (local-set-key (kbd "<tab>") 'org-cycle)))

(use-package anki-editor)

;; org-protocol allows to capture from the browser
(server-start)
(require 'org-protocol)

;(use-package org-evil)

;; this package maps too much (J, for instance)
;(use-package evil-org)

(use-package org-pomodoro
  :defer t
  :config
  (setq alert-default-style 'notifications
        org-pomodoro-audio-player (executable-find "cvlc")
        org-pomodoro-start-sound-args "--play-and-exit"
        org-pomodoro-finished-sound-args "--play-and-exit"
        org-pomodoro-overtime-sound-args "--play-and-exit"
        org-pomodoro-killed-sound-args "--play-and-exit"
        org-pomodoro-short-break-sound-args "--play-and-exit"
        org-pomodoro-long-break-sound-args "--play-and-exit"
        org-pomodoro-ticking-sound-args "--play-and-exit"))

;; (use-package org-jira
;;   :config
;;   (setq request-log-level 'debug)
;;   (setq request-message-level 'debug)
;;   (setq jiralib-url "https://intertrusttechnologies.atlassian.net"))

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
  :init
  (setq doom-modeline-modal nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-buffer-file-name-style 'file-name-with-project)
  (doom-modeline-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :bind (:map evil-normal-state-map
              ("M-." . nil))
  :config (evil-mode 1)
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
  :after (evil magit forge)
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
  :init (helm-mode)
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
        helm-move-to-line-cycle-in-source nil
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
  (setq helm-ag-base-command "rg --no-heading --sort path --smart-case --hidden --glob=!.git")
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
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
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
    "tf" 'treemacs-find-file)
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
;; Dired

(use-package dired+
  :config
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Octave

(use-package octave
  :mode ("\\.m\\'" . octave-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP

(use-package which-key
  :config (which-key-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-terraform-server '("terraform-ls" "serve"))
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-format-buffer-on-save t)
  (setq lsp-go-use-gofumpt nil)
  (setq lsp-go-analyses '((ST1003 . t)))
  ;(setq lsp-format-buffer-on-save-list '())
  :hook
  (scala-mode . lsp-deferred)
  (scala-ts-mode . lsp-deferred)
  (typescript-ts-mode . lsp-deferred)
  (tsx-ts-mode . lsp-deferred)
  (clojure-mode . lsp-deferred)
  (json-ts-mode . lsp-deferred)
  (java-ts-mode . lsp-deferred)
  (go-ts-mode . lsp-deferred)
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
    "gi" 'lsp-ui-peek-find-implementation
    "gr" 'lsp-ui-peek-find-references
    "ss" 'helm-lsp-workspace-symbol
    "te" 'lsp-treemacs-errors-list
    "ts" 'lsp-treemacs-symbols))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-sideline-diagnostic-max-lines 10)
  ;(setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-alignment 'window))

(use-package lsp-metals
  :after lsp-mode
  :config
  (setq lsp-metals-server-args '("-J-Xmx1G" "-J-XX:+UseStringDeduplication"))
  (setq lsp-metals-fallback-scala-version "3.3.3")
  (setq lsp-metals-show-implicit-arguments nil)
  (setq lsp-metals-show-inferred-type nil))

(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
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

;; (use-package go-mode)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java

(use-package lsp-java
  :after lsp-mode)
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python

(use-package lsp-pyright
  :defer t
  :hook (python-ts-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scala

(use-package scala-mode
  :mode ("\\.\\(scala\\|sbt\\|worksheet\\.sc\\|sc\\)\\'" . scala-mode))

(use-package scala-ts-mode
  :config
  (setq treesit-font-lock-level 4))

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

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit

(use-package magit
  :config
  (setq magit-prefer-remote-upstream t)
  (evil-leader/set-key
    "g f" 'magit-file-dispatch
    "g s" 'magit-status
    "g l" 'magit-log-buffer-file
    "g d" 'magit-diff-buffer-file))

(use-package forge
  :after magit)

(use-package magit-delta)

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

;; (use-package git-timemachine
;;   :config
;;   (evil-make-overriding-map git-timemachine-mode-map 'normal)
;;   ;; force update evil keymaps after git-timemachine-mode loaded
;;   (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))


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
  (add-hook 'scala-ts-mode-hook 'smartparens-strict-mode)
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

;; (use-package jq-mode)

;; (use-package restclient
;;   :straight `(restclient :type git :host github :files ("restclient.el" "restclient-jq.el" "restclient-helm.el") :repo "pashky/restclient.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multitran

(use-package multitran)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protobuf

(use-package protobuf-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kubernetes

(use-package kubernetes
  :commands (kubernetes-overview))

(use-package kubernetes-evil
  :after kubernetes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yaml

(use-package yaml-mode
  :config
  ;(add-hook 'yaml-mode-hook #'lsp)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal

(use-package vterm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terraform

(use-package terraform-mode
  :config
  (add-hook 'terraform-mode-hook #'lsp))

(defun normal-state-and-save ()
  (interactive)
  (evil-force-normal-state)
  (save-some-buffers t))

(defun ergodox-keys ()
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
  (global-set-key (kbd "C-M-s-o") 'lsp-extend-selection))

(ergodox-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dirvish

(straight-use-package 'dirvish
  :config
  ;(dirvish-override-dired-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree-sitter

(use-package treesit :straight (:type built-in))

(use-package evil-textobj-tree-sitter
  :config
  ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
(define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
(define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

;; You can also bind multiple items and we will match the first one we can find
(define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))))

;; Goto start of next function
(define-key evil-normal-state-map
            (kbd "]f")
            (lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer")))

;; Goto start of previous function
(define-key evil-normal-state-map
            (kbd "[f")
            (lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))

;; Goto next block
(define-key evil-normal-state-map
            (kbd "]]")
            (lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj '("conditional.outer" "loop.outer"))))

(setq major-mode-remap-alist
 '((scala-mode . scala-ts-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

;; guaranteed kill
(diminish 'auto-revert-mode)
