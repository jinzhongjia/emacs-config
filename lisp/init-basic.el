(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(global-display-line-numbers-mode t)

(set-face-attribute 'default nil :height 140)

(setq gc-cons-threshold most-positive-fixnum)

(prefer-coding-system 'utf-8)

; use y/n to replace yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'init-basic)
