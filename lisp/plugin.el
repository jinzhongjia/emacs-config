;; for melpa package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; for package initialize
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; for auto refresh package archive
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
;; make use-package default behavior better
;; with `use-package-always-ensure' you won't need ":ensure t" all the time
;; with `use-package-always-defer' you won't need ":defer t" all the time
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-enable-imenu-support t
        use-package-always-demand nil
        use-package-verbose t
        use-package-expand-minimally t)
  (require 'use-package))

;; for autoupdate package
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; easily restart emacs
(use-package restart-emacs)

(use-package gruvbox-theme
  :init (load-theme 'gruvbox-dark-soft t))

;;smart-mode-line
(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t
        sml/theme 'respectful)
  (sml/setup))

;; ivy
(use-package counsel
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-@" . 'counsel-mark-ring); 在某些终端上 C-x C-SPC 会被映射为 C-x C-@，比如在 macOS 上，所以要手动设置
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

;;an alternative interface for M-x in Emacs
;; TODO
;; (use-package amx
;;   :ensure t
;;   :init (amx-mode))

;;ace-windows
;; TODO
;; (use-package ace-window
;;   :ensure t
;;   :bind (("C-x o" . 'ace-window)))

;;mwim
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; unto-tree
;; (use-package undo-tree
;;   :ensure t
;;   :init (global-undo-tree-mode))



;; good-scroll
;; (use-package good-scroll
;;   :ensure t
;;   :if window-system          ; 在图形化界面时才使用这个插件
;;   :init (good-scroll-mode))

(provide 'plugin)
