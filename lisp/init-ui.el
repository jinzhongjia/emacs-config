(set-frame-font "Maple Mono SC NF" nil t)

;; rainbow 
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; highlight-symbol
(use-package highlight-symbol
  :init (highlight-symbol-mode)
  :bind ("<f3>" . highlight-symbol)) ;; 按下 F3 键就可高亮当前符号

;; good-scroll
;(use-package good-scroll
;  :defer t
;  :if window-system          ; 在图形化界面时才使用这个插件
;  :init (good-scroll-mode))

(use-package ace-window
  :defer t
  :bind (("C-x o" . 'ace-window)))

(use-package treemacs
  :defer t
  :config
  (progn
    (setq treemacs-default-visit-action 'treemacs-visit-node-close-treemacs)
    (treemacs-hide-gitignored-files-mode t))
  :bind
  (:map global-map
    ("M-0"       . treemacs-select-window)
    ("C-x t 1"   . treemacs-delete-other-windows)
    ("C-c m"     . treemacs)
    ("C-x t d"   . treemacs-select-directory)
    ("C-x t B"   . treemacs-bookmark)
    ("C-x t C-t" . treemacs-find-file)
    ("C-x t M-t" . treemacs-find-tag)))  

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(provide 'init-ui)
