;;; init-treesitter.el --- Tools settings -*- lexical-binding: t -*-
;;; Commentary: Useful tools to make Emacs efficient!

;;; Code:

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-install t)
  (treesit-font-lock-level 4)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package ts-fold
  :defer t
  :ensure (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold"))

(provide 'init-treesitter)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treesitter.el ends here
