;; for complete
(use-package company
  :ensure t
  ;; 等价于 (add-hook 'after-init-hook #'global-company-mode)
  :hook (after-init . global-company-mode)
  :config
  ;; setq 可以像这样连着设置多个变量的值
  (setq company-tooltip-align-annotations t ; 注释贴右侧对齐
        company-tooltip-limit 20            ; 菜单里可选项数量
        company-show-numbers t              ; 显示编号（然后可以用 M-数字 快速选定某一项）
        company-idle-delay .2               ; 延时多少秒后弹出
        company-minimum-prefix-length 1     ; 至少几个字符后开始补全
        company-selection-wrap-around t     ; 循环选择
        company-transformers '(company-sort-by-occurrence) ; 按使用频率排序
        ))

(use-package company-box
      :ensure t
      :if window-system
      :hook (company-mode . company-box-mode))

;; enable snippet
(use-package yasnippet
  :ensure t
  :hook
  (prog-mode . yas-minor-mode)
  :config
  ;; (yas-reload-all)
)

;; 再装一个通用模板库，省得没 template 用
(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet))

;; for check
(use-package flycheck
  :ensure t
  :config
  (setq truncate-lines nil) ; 如果单行信息很长会自动换行
  :hook
  (prog-mode . flycheck-mode))

;; magit
(use-package magit
  :ensure t)

;; search in bufer
(use-package ctrlf
  :ensure t
  :config
  (ctrlf-mode t)
  (dirvish-side-follow-mode t)
  (setq delete-by-moving-to-trash t)
  :bind 
  (("C-c m" . dirvish-side)))

(provide 'init-enhance)