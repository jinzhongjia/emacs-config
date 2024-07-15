(use-package doom-themes
  :config 
  (load-theme 'doom-dark+ t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(provide 'init-theme)
