;; disable comp warnings
(setq native-comp-async-report-warnings-errors nil)

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Package Management
;; -----------------------------------------------------------------
(require 'init-packages)

;; better defaults
;; -----------------------------------------------------------------
(require 'init-better-defaults)

;; ui
;; -----------------------------------------------------------------
(require 'init-ui)

;; undo-tree
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil))