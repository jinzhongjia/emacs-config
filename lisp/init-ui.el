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

(use-package dirvish
  :ensure t
  :init 
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-attributes
    '(vc-state subtree-state all-the-icons git-msg file-time file-size))
  (setq dirvish-preview-dispatchers
      (cl-substitute 'pdf-preface 'pdf dirvish-preview-dispatchers))
  (setq dired-mouse-drag-files t)
  (setq mouse-drag-and-drop-region-cross-program t)
  :bind ("C-x C-d" . 'dirvish))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; Addtional syntax highlighting for dired
(use-package diredfl
  :ensure t
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(provide 'init-ui)