;;; init-lang.el --- Tools settings -*- lexical-binding: t -*-
;;; Commentary: Useful tools to make Emacs efficient!

;;; Code:

(use-package
 zig-mode
 :defer t
 :custom (zig-format-on-save nil "disable zig format on save"))

(use-package highlight-defined :hook (elisp-mode . highlight-defined-mode))

(use-package nix-mode
  :if (not (eq system-type 'windows-nt))
  :mode "\\.nix\\'")

(provide 'init-lang)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lang.el ends here
