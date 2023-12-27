;;transparency
;; (add-to-list 'default-frame-alist '(alpha-background . 80))

;; autoparis

(electric-pair-mode t)  

;; auto reload file

(global-auto-revert-mode t)

;; disable startup

(setq inhibit-startup-message t)

;; disable file auto backup

(setq make-backup-files -1)

;; disable Menubar, Toolbars and Scrollbars

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; display Line Numbers and Truncated Lines

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

;; disable electric-indent-mode

(electric-indent-mode -1)

;; display relative line number

(setq display-line-numbers-type 'relative)

;; for font setting

(set-face-attribute 'default nil
  :font "Maple Mono SC NF"
  :height 110
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Maple Mono SC NF"
  :height 120
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "Maple Mono SC NF"
  :height 110
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "Maple Mono SC NF-11"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(defun display-startup-echo-area-message ())

;; 设置为只有发生错误时才报告
(setq warning-minimum-level :emergency)

;; provide
(provide 'basic)
