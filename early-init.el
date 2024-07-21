;;; early-init.el --- Emacs pre-initialization config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; 设置垃圾回收参数
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; 禁止启动的首页展示
(setq package-enable-at-startup nil)

;; 禁止自动缩放窗口先
(setq frame-inhibit-implied-resize t)

;; 禁止菜单栏、工具栏、滚动条模式，禁止启动屏幕和文件对话框
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)

;; 此阶段不编译
(setq comp-deferred-compilation nil)

;; 设置 utf8 编码
(prefer-coding-system 'utf-8)

; use y/n to replace yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

; 不显示编译警告
(setq native-comp-async-report-warnings-errors nil)

; 设置字体
(set-face-attribute 'default nil :height 140)

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
