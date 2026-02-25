;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-roam-directory "~/org/roam")
(setq org-agenda-files (list org-directory org-roam-directory))


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `with-eval-after-load' block, otherwise Doom's defaults may override your
;; settings. E.g.
;;
;;   (with-eval-after-load! 'PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look them up).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

(with-eval-after-load 'go-mode
  ;; set formatter in go-mode just in case
  (customize-set-variable 'gofmt-command "goimports"))

;; set formatter in doom's (format +onsave) via apheleia
(setq-hook! 'go-ts-mode-hook +format-with 'goimports)

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (map! :leader
        (:prefix ("c" . "Copilot Chat")
         :desc "Copilot Chat" "h" #'copilot-chat-transient))
  (add-to-list 'copilot-indentation-alist '(go-mode 4)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(with-eval-after-load 'magit
  (setq git-commit-summary-max-length 200))

;; https://github.com/weijiangan/flycheck-golangci-lint/issues/28
(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-ts-mode . flycheck-golangci-lint-setup)
  :config
  ;; Fix version detection to use flycheck-checker-executable (issue #28)
  (defun flycheck-golangci-lint--parse-version ()
    "Parse golangci-lint version from --version output."
    (unless flycheck-golangci-lint--version
      (let* ((output (ignore-errors
                       (with-temp-buffer
                         (call-process (flycheck-checker-executable 'golangci-lint)
                                       nil t nil "--version")
                         (buffer-string))))
             (version-regex "version \\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)"))
        (when (and output (string-match version-regex output))
          (setq flycheck-golangci-lint--version
                (list (string-to-number (match-string 1 output))
                      (string-to-number (match-string 2 output))
                      (string-to-number (match-string 3 output)))))))
    flycheck-golangci-lint--version))


(with-eval-after-load 'auth-source
  (if (featurep :system 'macos)
      (add-to-list 'auth-sources 'macos-internet-keychain)))
