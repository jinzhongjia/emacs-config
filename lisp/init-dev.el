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

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package magit-file-icons
  :ensure t
  :after magit
  :init
  (magit-file-icons-mode 1))

(use-package treemacs-magit
  :after (treemacs magit))

(provide 'init-dev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dev.el ends here
