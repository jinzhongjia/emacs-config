(use-package helm
  :defer t
  ;; 等价于 (bind-key "M-x" #'helm-M-x)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files))
  :config
  ;; 全局启用 Helm minor mode
  (helm-mode 1))

(use-package helm-ag
  :after heml)


(use-package helm-swoop
  :after heml
  ;; 更多关于它的配置方法: https://github.com/ShingoFukuyama/helm-swoop
  ;; 以下我的配置仅供参考
  :bind
  (("M-i" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all)
   :map isearch-mode-map
   ("M-i" . helm-swoop-from-isearch)
   :map helm-swoop-map
   ("M-i" . helm-multi-swoop-all-from-helm-swoop)
   ("M-m" . helm-multi-swoop-current-mode-from-helm-swoop))
  :config
  ;; 它像 helm-ag 一样，可以直接修改搜索结果 buffer 里的内容并 apply
  (setq helm-multi-swoop-edit-save t)
  ;; 如何给它新开分割窗口
  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows t))

(use-package amx
  :defer t
  :init (amx-mode))

;; restart-emacs
(use-package restart-emacs
  :defer t)

(use-package beacon
  :defer t
  :init (beacon-mode 1))

(provide 'init-better-defaults)
