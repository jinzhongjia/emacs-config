;; 手动添加配置加载目录，指定为lisp文件夹
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)) ; 设定源码加载路径

;; for basical config
(require 'basic)
;; for elpa package
(require 'init-elpa)
;; for keybind
(require 'keybind)
;; theme config
(require 'theme)
;; plugin import
(require 'plugin)

;; load custom file at last
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
