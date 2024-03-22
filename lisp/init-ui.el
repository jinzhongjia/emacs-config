;; rainbow 
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; highlight-symbol
(use-package highlight-symbol
  :ensure t
  :init (highlight-symbol-mode)
  :bind ("<f3>" . highlight-symbol)) ;; 按下 F3 键就可高亮当前符号

;; good-scroll
(use-package good-scroll
  :ensure t
  :if window-system          ; 在图形化界面时才使用这个插件
  :init (good-scroll-mode))

;; vscode theme
(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300)
  )

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(provide 'init-ui)