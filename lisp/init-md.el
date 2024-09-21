;;; init-md.el --- Org mode settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :defer t
  :init (setq markdown-command "pandoc"))

(use-package markdown-toc
  :hook (markdown-mode . markdown-toc-mode))

(provide 'init-md)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-md.el ends here
