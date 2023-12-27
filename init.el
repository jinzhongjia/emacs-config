(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'basic)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; 安装use-package
(straight-use-package 'use-package) 

;; 自动安装所有包
(setq straight-use-package-by-default t)

;; 安装 which-key
(use-package which-key
  :custom (which-key-idle-delay 0.5) ; 延迟时间, 以秒为单位
  :config (which-key-mode)) ; 启用 which-key 模式

;; keycast 则会显示当前你使用的快捷键及对应的命令名
(use-package keycast
  :config (keycast-header-line-mode 1)) ; 在标题显示


(use-package helpful
  :bind
  ;; 重新定向 C-h 开始的命令
  (([remap describe-function] . #'helpful-callable)
   ([remap describe-variable] . #'helpful-variable)
   ([remap describe-key] . #'helpful-key)
   ([remap describe-command] . #'helpful-command)
   ([remap describe-symbol] . #'helpful-symbol)
   ("C-h C-d" . #'helpful-at-point)
   ("C-h F" . #'helpful-function)))


(use-package vertico ; 竖式展开小缓冲区
  :custom (verticle-cycle t)
  :config (vertico-mode))

(use-package marginalia ; 更多信息
  :config (marginalia-mode))

(use-package orderless ; 乱序补全
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; for restart-emacs
(use-package restart-emacs
  :init
  (setq restart-emacs-restore-frames t))

;; 放在最后一行，应用 emacs 自动设置的变量
(if (file-exists-p custom-file) (load-file custom-file))
