(use-package helm
  :ensure t
  ;; 等价于 (bind-key "M-x" #'helm-M-x)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files))
  :config
  ;; 全局启用 Helm minor mode
  (helm-mode 1))

;; restart-emacs
(use-package restart-emacs
  :ensure t)

(use-package beacon
  :ensure t
  :init (beacon-mode 1))

(provide 'init-better-defaults)