;;; init-blog.el --- blog settings -*- lexical-binding: t -*-
;;; Commentary: Useful tools to make Emacs efficient!

;;; Code:

(use-package ox-hugo
  :custom
  (org-hugo-auto-set-lastmod t "auto update latest time")
  )

(defun format-org-hugo-header (title author date base-dir section categories layout export-file-name)
  "Format an Org-mode header for ox-hugo with the given parameters."
  (format "#+TITLE: %s\n#+AUTHOR: %s\n#+DATE: %s\n#+HUGO_BASE_DIR: %s\n#+HUGO_SECTION: %s\n#+HUGO_CUSTOM_FRONT_MATTER: :math false\n#+HUGO_CATEGORIES: %s\n#+HUGO_LAYOUT: %s\n#+EXPORT_FILE_NAME: %s\n"
          title
          author
          date
          base-dir
          section
          categories
          layout
          export-file-name))

(defun blog-org()
  "Create a new blog post org in the org-hugo-base-dir."
  (interactive)
  (let* ((article-type (read-string "input article type:"))
         (article-name (read-string "input article name:"))
         (article-lang (completing-read "select language: " '("en" "cn"))) 
         (layout (completing-read "select layout: " '("docs" "blog" "default")))
         (base-dir "~/blog")
         (current-time (current-time))
         (year (format-time-string "%Y" current-time))
         (month (format-time-string "%m" current-time))
         (day (format-time-string "%d" current-time))
         (date (concat year "-" month "-" day))
         (section (concat article-type "/" year "/" month "/" day "/" article-name))
         (post-dir (expand-file-name (concat "content-org" "/" section) base-dir))
         (file-name (concat "index" (if (string= article-lang "en") "" ".zh-cn") ".org"))
         (index-file (expand-file-name file-name post-dir)))
    (make-directory post-dir t)
    (with-temp-file index-file
      (insert (format-org-hugo-header article-name "" date base-dir section "" layout file-name)))  ;; 确保这个函数存在并返回有效内容
    (find-file index-file)  ;; 在文件生成后打开
    (message "Create file: %s" index-file)))

(provide 'init-blog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-blog.el ends here
