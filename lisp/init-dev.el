;;; init-dev.el --- Development settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package transient :defer t)

(use-package magit
  :after transient)

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package diff-hl :defer t :init (global-diff-hl-mode t))

(use-package magit-delta :hook (magit-mode . magit-delta-mode))

(provide 'init-dev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dev.el ends here
