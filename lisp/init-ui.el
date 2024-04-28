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

(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-plain-icons t)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-set-bar 'left)
  (setq centaur-tabs-set-modified-marker t)
  (defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*Dirvish" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p "*Messages" name)
     (string-prefix-p " *temp" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "*mybuf" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name)))
     )))
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(provide 'init-ui)