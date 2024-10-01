;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order
  '(elpaca
    :repo "https://github.com/progfolio/elpaca.git"
    :ref nil
    :depth 1
    :files (:defaults "elpaca-test.el" (:exclude "extensions"))
    :build (:not elpaca--activate-package)))
(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
   (build (expand-file-name "elpaca/" elpaca-builds-directory))
   (order (cdr elpaca-order))
   (default-directory repo))
  (add-to-list
   'load-path
   (if (file-exists-p build)
   build
     repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28)
  (require 'subr-x))
    (condition-case-unless-debug err
    (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
  	   ((zerop
  	     (apply #'call-process
  		    `("git" nil ,buffer t "clone" ,@
  		      (when-let ((depth (plist-get order :depth)))
  			(list
  			 (format "--depth=%d" depth)
  			 "--no-single-branch"))
  		      ,(plist-get order :repo) ,repo))))
  	   ((zerop
  	     (call-process "git"
  			   nil
  			   buffer
  			   t
  			   "checkout"
  			   (or (plist-get order :ref) "--"))))
  	   (emacs (concat invocation-directory invocation-name))
  	   ((zerop
  	     (call-process emacs
  			   nil
  			   buffer
  			   nil
  			   "-Q"
  			   "-L"
  			   "."
  			   "--batch"
  			   "--eval"
  			   "(byte-recompile-directory \".\" 0 'force)")))
  	   ((require 'elpaca))
  	   ((elpaca-generate-autoloads "elpaca" repo)))
      (progn
        (message "%s" (buffer-string))
        (kill-buffer buffer))
      (error
       "%s"
       (with-current-buffer buffer
         (buffer-string))))
  ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(when (eq system-type 'windows-nt)
  ; 当 windows 平台时，关闭软链接，同时限制 elpaca 的并发数目
  (setq elpaca-queue-limit 10)
  (elpaca-no-symlink-mode))

(elpaca
    elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(setq use-package-always-ensure t)

(defvar cabins--os-win (memq system-type '(ms-dos windows-nt cygwin)))
(defvar cabins--os-mac (eq system-type 'darwin))

(when (and cabins--os-win
  	       (boundp 'w32-get-true-file-attributes))
  (setq w32-get-true-file-attributes nil
  	    w32-pipe-read-delay 0
  	    w32-pipe-buffer-size (* 64 1024)))

(setq text-quoting-style 'straight)

;; 将lisp目录放到加载路径的前面以加快启动速度
(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir)))

;; 加载各模块化配置
;; 不要在`*message*'缓冲区显示加载模块化配置的信息
(with-temp-message ""
  (require 'init-ui) ; UI交互
  (require 'init-edit) ; 编辑行为
  (require 'init-md) ; markdown 支持
  (require 'init-org) ; org 相关设置
  (require 'init-completion) ; 补全系统
  (require 'init-dev) ; 版本管理
  (require 'init-tools) ; tools
  (require 'init-lsp) ; lsp 支持
  (require 'init-treesitter) ; treesitter 支持
  (require 'init-lang) ; lang 支持
  (require 'init-blog) ; blog 支持
  (require 'init-ai) ; ai 支持
  )

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
