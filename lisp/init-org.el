;;; init-org.el --- Org mode settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package org
  :ensure t
  :custom-face
  ;; 设置Org mode标题以及每级标题行的大小
  (org-document-title ((t (:height 1.3 :weight bold))))
  (org-level-1 ((t (:height 1.2 :weight bold))))
  (org-level-2 ((t (:height 1.15 :weight bold))))
  (org-level-3 ((t (:height 1.1 :weight bold))))
  (org-level-4 ((t (:height 1.05 :weight bold))))
  (org-level-5 ((t (:height 1.0 :weight bold))))
  (org-level-6 ((t (:height 1.0 :weight bold))))
  (org-level-7 ((t (:height 1.0 :weight bold))))
  (org-level-8 ((t (:height 1.0 :weight bold))))
  (org-level-9 ((t (:height 1.0 :weight bold))))
  ;; 设置代码块用上下边线包裹
  (org-block-begin-line ((t (:underline t :background unspecified))))
  (org-block-end-line ((t (:overline t :underline nil :background unspecified))))
  ;; 处理掉超链接默认的高亮
  (org-link ((t (:foreground "inherit" :underline t))))
  :custom
  (org-list-allow-alphabetical t)
  (org-fold-catch-invisible-edits 'smart)
  (org-image-actual-width nil)
  (word-wrap-by-category t)
  (org-blank-before-new-entry
   '((heading . t)
     (plain-list-item . auto)))
  ;; 设置Org mode的目录
  (org-directory "~/org")
  ;; 设置笔记的默认存储位置
  (org-default-notes-file (expand-file-name "capture.org" org-directory))
  ;; 启用一些子模块
  (org-modules '(ol-bibtex ol-gnus ol-info ol-eww org-habit org-protocol))
  ;; 标题行美化
  (org-fontify-whole-heading-line t)
  ;; 设置标题行折叠符号
  (org-ellipsis " ▾")
  ;; 在活动区域内的所有标题栏执行某些命令
  (org-loop-over-headlines-in-active-region t)
  ;; TODO标签美化
  (org-fontify-todo-headline t)
  ;; DONE标签美化
  (org-fontify-done-headline t)
  ;; 引用块美化
  (org-fontify-quote-and-verse-blocks t)
  ;; 隐藏宏标记
  (org-hide-macro-markers t)
  ;; 隐藏强调标签
  (org-hide-emphasis-markers t)
  ;; 高亮latex语法
  (org-highlight-latex-and-related '(native script entities))
  ;; 以UTF-8显示
  (org-pretty-entities t)
  ;; 当启用缩进模式时自动隐藏前置星号
  (org-indent-mode-turns-on-hiding-stars t)
  ;; 自动显示图片
  (org-startup-with-inline-images t)
  ;; 默认以Overview的模式展示标题行
  (org-startup-folded 'content)
  ;; 允许字母列表
  (org-list-allow-alphabetical t)
  ;; 列表的下一级设置
  (org-list-demote-modify-bullet
   '(("-"  . "+")
     ("+"  . "1.")
     ("1." . "a.")))
  ;; 编辑时检查是否在折叠的不可见区域
  (org-fold-catch-invisible-edits 'smart)
  ;; 在当前位置插入新标题行还是在当前标题行后插入，这里设置为当前位置
  (org-insert-heading-respect-content nil)
  ;; imenu的最大深度，默认为2
  (org-imenu-depth 4)
  ;; 回车要不要触发链接，这里设置不触发
  (org-return-follows-link nil)
  ;; 上标^下标_是否需要特殊字符包裹，这里设置需要用大括号包裹
  (org-use-sub-superscripts '{})
  ;; 复制粘贴标题行的时候删除id
  (org-clone-delete-id t)
  ;; 粘贴时调整标题行的级别
  (org-yank-adjusted-subtrees t)
  ;; 使用专家模式选择标题栏状态
  (org-use-fast-todo-selection 'expert)
  ;; 父子标题栏状态有依赖
  (org-enforce-todo-dependencies t)
  ;; 标题栏和任务复选框有依赖
  (org-enforce-todo-checkbox-dependencies t)
  ;; 优先级样式设置
  (org-priority-faces '((?A :foreground "red")
                        (?B :foreground "orange")
                        (?C :foreground "yellow")))
  :config
  (if (eq system-type 'windows-nt)
      (plist-put org-format-latex-options :scale 1.25)
    (plist-put org-format-latex-options :scale 2.5))
  )

(use-package ox
  :ensure nil
  :after org
  :custom
  (org-export-with-toc t)
  (org-export-with-drawers nil)
  (org-export-with-priority t)
  (org-export-with-footnotes t)
  (org-export-with-smart-quotes t)
  (org-export-with-section-numbers t)
  (org-export-with-sub-superscripts '{})
  (org-export-use-babel t)
  (org-export-headline-levels 9)
  (org-export-coding-system 'utf-8)
  (org-export-with-broken-links 'mark)
  (org-export-default-language "zh-CN") ; 默认是en
  (org-html-htmlize-output-type 'css)
  (org-html-head-include-default-style nil)
  :config
  ;; 很奇怪，这个变量通过 custom 设置无效，但是 setq 生效
  (setq org-export-exclude-tags '("TOC")))

;; export extra
(use-package ox-extra
  :ensure nil
  :after org
  :config
  (ox-extras-activate '(ignore-headlines)))

(use-package ox-html
  :ensure nil
  :after org
  :init
  ;; add support for video
  (defun org-video-link-export (path desc backend)
    (let ((ext (file-name-extension path)))
      (cond
       ((eq 'html backend)
        (format "<video width='800' preload='metadata' controls='controls'><source type='video/%s' src='%s' /></video>" ext path))
       ;; fall-through case for everything else
       (t
        path))))
  (org-link-set-parameters "video" :export 'org-video-link-export)
  :custom
  (org-html-doctype "html5")
  (org-html-html5-fancy t)
  (org-html-checkbox-type 'unicode)
  (org-html-validation-link nil))

(use-package htmlize
  :ensure t
  :after org
  :custom
  (htmlize-pre-style t)
  (htmlize-output-type 'css))

(use-package ol
  :ensure nil
  :defer t
  :custom
  (org-link-keep-stored-after-insertion t)
  (org-link-abbrev-alist '(("github"        . "https://github.com/")
                           ("gitlab"        . "https://gitlab.com/")
                           ("google"        . "https://google.com/search?q=")
                           ("wiki"          . "https://en.wikipedia.org/wiki/")
                           ("youtube"       . "https://youtube.com/watch?v=")
                           ("zhihu"         . "https://zhihu.com/question/"))))



(add-hook 'org-mode-hook 'org-indent-mode)

(use-package org-contrib :after org)

(use-package valign
  :hook ((org-mode . valign-mode)
         (markdown-mode . valign-mode)))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package ox-gfm :defer t)

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
