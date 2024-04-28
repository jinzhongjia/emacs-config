(set-frame-font "Maple Mono SC NF" nil t)

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

(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))

(use-package vscode-icon
  :ensure t
  :commands (vscode-icon-for-file))

(use-package dirvish
  :ensure t
  :init 
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-attributes
    '(vc-state subtree-state vscode-icon collapse git-msg file-time file-size))
  :bind ("C-x C-d" . 'dirvish))



(provide 'init-ui)