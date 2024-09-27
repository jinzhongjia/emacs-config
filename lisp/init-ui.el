;;; init-ui.el --- UI settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(setq my-default-fonts '("Maple Mono SC NF"))

(setq my-emoji-fonts '("Noto Color Emoji"
                       "Segoe UI Emoji"
                       "Symbola"))

(setq my-bmp-fonts '("Segoe UI Symbol"
                     "Symbola" "Symbol"))

(setq my-cjk-fonts '("霞鹜文楷等宽"
                     "微软雅黑 CN"
                     "Microsoft Yahei UI"
                     "Microsoft Ya"))

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(when (display-graphic-p)
  (cl-loop for font in my-default-fonts
           when (font-installed-p font)
           return (set-face-attribute 'default nil :font
                                      (font-spec :family font :weight 'normal :slant 'normal :size
                                                 (cond ((eq system-type 'gnu/linux) 14.0)
                                                       ((eq system-type 'windows-nt) 12.5))))))

(when (display-graphic-p)
  (cl-loop for font in my-emoji-fonts
           when (font-installed-p font)
           return (set-fontset-font t 'emoji
                                    (font-spec :family font
                                               :size (cond ((eq system-type 'gnu/linux) 16.5)
                                                           ((eq system-type 'windows-nt) 15.0)))
                                    nil 'prepend)))

(when (display-graphic-p)
     (cl-loop for font in my-bmp-fonts
             when (font-installed-p font)
             return (set-fontset-font t 'unicode-bmp
                                      (font-spec :family font
                                                 :size (cond ((eq system-type 'gnu/linux) 16.5)
                                                             ((eq system-type 'windows-nt) 15.0)))
                                      nil 'prepend)))

(when (display-graphic-p)
  (cl-loop for font in my-cjk-fonts
           when (font-installed-p font)
           return (set-fontset-font t 'han
                                    (font-spec :name font
                                               :weight 'normal
                                               :slant 'normal
                                               :size (cond ((eq system-type 'gnu/linux) 16.5)
                                                           ((eq system-type 'windows-nt) 15.0))))))

(use-package modus-themes)

(use-package auto-dark
  :custom
  (auto-dark-dark-theme 'modus-vivendi "auto dark theme")
  (auto-dark-light-theme 'modus-operandi "auto light theme")
  :config
  (auto-dark-mode t))

(when (display-graphic-p)
  (pixel-scroll-precision-mode 1))
;; 禁用一些GUI特性
(setq use-dialog-box nil) ; 鼠标操作不使用对话框
(setq inhibit-default-init t) ; 不加载 `default' 库
(setq inhibit-startup-screen t) ; 不加载启动画面
(setq inhibit-startup-message t) ; 不加载启动消息
(setq inhibit-startup-buffer-menu t) ; 不显示缓冲区列表

;; 草稿缓冲区默认文字设置
(setq initial-scratch-message
      (concat
       ";; Happy hacking, " (capitalize user-login-name) " - Emacs ♥ you!\n\n"))

;; 设置缓冲区的文字方向为从左到右
(setq bidi-paragraph-direction 'left-to-right)
;; 禁止使用双向括号算法
(setq bidi-inhibit-bpa t)

;; 设置自动折行宽度为80个字符，默认值为70
(setq-default fill-column 80)

;; 设置大文件阈值为100MB，默认10MB
(setq large-file-warning-threshold 100000000)

;; 以16进制显示字节数
(setq display-raw-bytes-as-hex t)
;; 有输入时禁止 `fontification' 相关的函数钩子，能让滚动更顺滑
(setq redisplay-skip-fontification-on-input t)

;; 禁止响铃
(setq ring-bell-function 'ignore)

;; 禁止闪烁光标
(blink-cursor-mode -1)

;; 在光标处而非鼠标所在位置粘贴
(setq mouse-yank-at-point t)

;; 拷贝粘贴设置
(setq select-enable-primary nil) ; 选择文字时不拷贝
(setq select-enable-clipboard t) ; 拷贝时使用剪贴板

;; 鼠标滚动设置
                                        ;(setq scroll-step 2)
                                        ;(setq scroll-margin 2)
                                        ;(setq hscroll-step 2)
                                        ;(setq hscroll-margin 2)
                                        ;(setq scroll-conservatively 101)
                                        ;(setq scroll-up-aggressively 0.01)
                                        ;(setq scroll-down-aggressively 0.01)
                                        ;(setq scroll-preserve-screen-position 'always)

;; 对于高的行禁止自动垂直滚动
(setq auto-window-vscroll nil)

;; 设置新分屏打开的位置的阈值
(setq split-width-threshold (assoc-default 'width default-frame-alist))
(setq split-height-threshold nil)

;; TAB键设置，在Emacs里不使用TAB键，所有的TAB默认为4个空格
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; yes或no提示设置，通过下面这个函数设置当缓冲区名字匹配到预设的字符串时自动回答yes
(setq original-y-or-n-p 'y-or-n-p)
(defalias 'original-y-or-n-p (symbol-function 'y-or-n-p))
(defun default-yes-sometimes (prompt)
  "automatically say y when buffer name match following string"
  (if (or (string-match "has a running process" prompt)
          (string-match "does not exist; create" prompt)
          (string-match "modified; kill anyway" prompt)
          (string-match "Delete buffer using" prompt)
          (string-match "Kill buffer of" prompt)
          (string-match "still connected.  Kill it?" prompt)
          (string-match "Shutdown the client's kernel" prompt)
          (string-match "kill them and exit anyway" prompt)
          (string-match "Revert buffer from file" prompt)
          (string-match "Kill Dired buffer of" prompt)
          (string-match "delete buffer using" prompt)
          (string-match "Kill all pass entry" prompt)
          (string-match "for all cursors" prompt)
          (string-match "Do you want edit the entry" prompt))
      t
    (original-y-or-n-p prompt)))
(defalias 'yes-or-no-p 'default-yes-sometimes)
(defalias 'y-or-n-p 'default-yes-sometimes)

;; 设置剪贴板历史长度300，默认为60
(setq kill-ring-max 300)

;; 在剪贴板里不存储重复内容
(setq kill-do-not-save-duplicates t)

;; 设置位置记录长度为6，默认为16
;; 可以使用 `counsel-mark-ring' or `consult-mark' (C-x j) 来访问光标位置记录
;; 使用 C-x C-SPC 执行 `pop-global-mark' 直接跳转到上一个全局位置处
;; 使用 C-u C-SPC 跳转到本地位置处
(setq mark-ring-max 6)
(setq global-mark-ring-max 6)

;; 设置 emacs-lisp 的限制
(setq max-lisp-eval-depth 10000) ; 默认值为 800
(setq max-specpdl-size 10000) ; 默认值为 1600

;; 启用 `list-timers', `list-threads' 这两个命令
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)

;; 在命令行里支持鼠标
(xterm-mouse-mode 1)

;; 在模式栏上显示当前光标的列号
(column-number-mode t)

(if (eq system-type 'windows-nt)
    (progn
      ;;use unicode everywhere
      (when (fboundp 'set-charset-priority)
        (set-charset-priority 'unicode))
      (prefer-coding-system 'utf-8-unix)
      (modify-coding-system-alist 'process "*" 'utf-8-unix)
      (set-buffer-file-coding-system 'utf-8)
      (set-file-name-coding-system 'utf-8-unix)
      (set-default-coding-systems 'utf-8-unix)
      (set-keyboard-coding-system 'utf-8-unix)
      (set-terminal-coding-system 'utf-8-unix)
      (set-language-environment "UTF-8")
      (setq locale-coding-system 'utf-8-unix)
      (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
      
      ;;windows没有启用unicode时，中文语言是gbk编码gb18030会导致有些中文字符找不到字体
      ;; 系统如果开启了 unicode 支持，那么就不用设置这个
                                        ;(when (eq system-type 'windows-nt)
                                        ;  (setq locale-coding-system 'chinese-gbk))

      ;;The clipboard on windows dose not play well with utf8
      (unless (eq system-type 'windows-nt)
        (set-clipboard-coding-system 'utf-8)
        (set-selection-coding-system 'utf-8))

      ;; 英文日期，会影响日期格式
      (setq system-time-locale "C")))

(if (eq system-type 'gnu/linux)
    (progn
      (setq locale-coding-system 'utf-8)
      (set-terminal-coding-system 'utf-8)
      (set-keyboard-coding-system 'utf-8)
      (set-selection-coding-system 'utf-8)
      (set-default-coding-systems 'utf-8)
      (set-language-environment 'utf-8)
      (set-clipboard-coding-system 'utf-8)
      (set-file-name-coding-system 'utf-8)
      (set-buffer-file-coding-system 'utf-8)
      (prefer-coding-system 'utf-8)
      (modify-coding-system-alist 'process "*" 'utf-8)
      (when (display-graphic-p)
        (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
      )
    )

(use-package
 doom-modeline
 :config (doom-modeline-mode)
 :custom
 (doom-modeline-hub t)
 (doom-modeline-buffer-file-name-style 'file-name)
 (doom-modeline-total-line-number t)
 (doom-modeline-irc nil)
 (doom-modeline-mu4e nil)
 (doom-modeline-gnus nil)
 (doom-modeline-github nil)
 (doom-modeline-enable-word-count t))

(use-package nerd-icons
  :defer t)

(use-package nerd-icons-dired
  :after nerd-icons
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(setq dired-listing-switches "-alh --group-directories-first")

(provide 'init-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
