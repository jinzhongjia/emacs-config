; for tree-sitter
(use-package treesit-auto
  :defer t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist))

(use-package ts-fold
  :ensure (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold"))

; for zig lang
(use-package zig-mode
    :defer t)

(use-package nix-mode
  :mode "\\.nix\\'")

(provide 'init-lang)
