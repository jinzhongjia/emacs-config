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
  ;; 自动开启 indent mode
  (org-startup-indented t)
  ;; 允许字母列表
  (org-list-allow-alphabetical t)
  ;; 编辑时检查是否在折叠的不可见区域
  (org-fold-catch-invisible-edits 'smart)
  ;; 设置图片的最大宽度，如果有imagemagick支持将会改变图片实际宽度
  ;; 四种设置方法：(1080), 1080, t, nil
  (org-image-actual-width nil)
  ;; 处理中文的换行问题
  (word-wrap-by-category t)
  ;; 设置标题行之间总是有空格；列表之间根据情况自动加空格
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
  ;; (org-startup-folded 'content)
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

(use-package org-modern
  :after org
  :custom
  (org-modern-hide-stars 'leading)
  (line-spacing 0.1)
  ;; 由于字体问题，暂时禁用掉 org modern table
  (org-modern-table nil)
  :config
  (global-org-modern-mode))

(use-package org-modern-indent
  :ensure '(org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :after org-modern
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

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

(use-package ox-latex
  :ensure nil
  :after org
  :defer t
  :config
  (add-to-list 'org-latex-classes
               '("cn-article"
                 "\\documentclass[UTF8,a4paper]{article}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("cn-report"
                 "\\documentclass[11pt,a4paper]{report}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (setq org-latex-default-class "cn-article")
  (setq org-latex-image-default-height "0.9\\textheight"
        org-latex-image-default-width "\\linewidth")
  (setq org-latex-pdf-process
	    '("xelatex -interaction nonstopmode -output-directory %o %f"
	      "bibtex %b"
	      "xelatex -interaction nonstopmode -output-directory %o %f"
	      "xelatex -interaction nonstopmode -output-directory %o %f"
	      "rm -fr %b.out %b.log %b.tex %b.brf %b.bbl auto"
	      ))
  ;; 使用 Listings 宏包格式化源代码(只是把代码框用 listing 环境框起来，还需要额外的设置)
  (setq org-latex-listings t)
  ;; mapping jupyter-python to Python
  (add-to-list 'org-latex-listings-langs '(jupyter-python "Python"))
  ;; Options for \lset command（reference to listing Manual)

  )

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

(use-package org-contrib :after org)

(use-package valign
  :hook ((org-mode . valign-mode)
         (markdown-mode . valign-mode)))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package ox-gfm :defer t)

(use-package org-margin
  :ensure '(org-margin :type git :host github :repo "rougier/org-margin")
  :disabled t
  :hook (org-mode . org-margin-mode))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
