;;; early-init.el --- Emacs pre-initialization config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; 设置垃圾回收参数
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; 不要在 gc 期间压缩字体
(setq inhibit-compacting-font-caches t)

;; 显示真实的路径，而不是符号链接
(setq find-file-visit-truename t)

;; 禁止启动的首页展示
(setq package-enable-at-startup nil)

;; 禁止自动缩放窗口先
(setq frame-inhibit-implied-resize t)

;; 禁止菜单栏、工具栏、滚动条模式，禁止启动屏幕和文件对话框
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-file-dialog nil)

;; 此阶段不编译
(setq comp-deferred-compilation nil)

; 不显示编译警告
(setq native-comp-async-report-warnings-errors nil)

(cd "~")

(setenv "LSP_USE_PLISTS" "true")

;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024))

; custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
