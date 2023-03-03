;; package benchmark
(use-package benchmark-init
             :init (benchmark-init/activate)
             :hook (after-init . benchmark-init/deactivate))

;; for better scroll
(use-package good-scroll
  :ensure t
  :if window-system          ; 在图形化界面时才使用这个插件
  :init (good-scroll-mode 1))

;; undo tree
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode))

;; smart mode line
(use-package smart-mode-line
  :ensure t
  :init (sml/setup))

;; highlight symbol
(use-package highlight-symbol
  :ensure t
  :init (highlight-symbol-mode)
  :bind ("<f3>" . highlight-symbol)) ;; 按下 F3 键就可高亮当前符号
(provide 'plugin)