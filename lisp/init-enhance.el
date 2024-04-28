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
        ))

;; enable snippet
(use-package yasnippet
  :ensure t
  :config
  ;; 全局启用这个 minor mode
  (yas-global-mode 1))

;; 再装一个通用模板库，省得没 template 用
(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet))

;; for check
(use-package flycheck
  :ensure t
  :init ;; 在 (require) 之前需要执行的
  (setq flycheck-emacs-lisp-load-path 'inherit)
  :config
  (global-flycheck-mode))

;; magit
;; (use-package magit
  ;; :ensure t)


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