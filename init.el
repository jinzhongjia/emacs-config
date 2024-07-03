;; disable comp warnings
(setq native-comp-async-report-warnings-errors nil)

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; for custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; basic settings
;; -----------------------------------------------------------------
(require 'init-basic)

;; Package Management
;; -----------------------------------------------------------------
(require 'init-packages)

;; vscode theme
(require 'init-theme)

;; better defaults
;; -----------------------------------------------------------------
(require 'init-better-defaults)

;; ui
;; -----------------------------------------------------------------
(require 'init-ui)

;; enhance
;; -----------------------------------------------------------------
(require 'init-enhance)

;; lsp
;; -----------------------------------------------------------------
(require 'init-lsp)

; (require 'init-dashboard)

(require 'init-lang)
