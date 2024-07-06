(use-package counsel 
  :defer t
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        search-default-mode #'char-fold-to-regexp)
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

(use-package ivy-rich
  :after (counsel)
  :init (ivy-rich-mode 1))

(use-package amx
  :defer t
  :init (amx-mode))

(use-package marginalia
  :defer t
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle)))

;; restart-emacs
(use-package restart-emacs
  :defer t)

(use-package beacon
  :defer t
  :init (beacon-mode 1))

(provide 'init-better-defaults)
