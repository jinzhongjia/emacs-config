; for tree-sitter
(use-package treesit-auto
  :defer t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

; for zig lang
(use-package zig-mode
    :defer t)

(provide 'init-lang)
