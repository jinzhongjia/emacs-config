;;; init-lang.el --- Tools settings -*- lexical-binding: t -*-
;;; Commentary: Useful tools to make Emacs efficient!

;;; Code:

(use-package web-mode
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))

(use-package vue-mode)

(use-package lua-mode)

(use-package go-mode)

(use-package
 zig-mode
 :defer t
 :custom (zig-format-on-save nil "disable zig format on save"))

(use-package highlight-defined :hook (elisp-mode . highlight-defined-mode))

(use-package nix-mode
  :if (not (eq system-type 'windows-nt))
  :mode "\\.nix\\'")

(use-package rust-mode
  :defer t
  :init
  (setq rust-mode-treesitter-derive t)
  :hook
  (rust-mode . (lambda () (prettify-symbols-mode))))

(use-package rustic
  :after (rust-mode)
  :config
  (setq rustic-format-on-save nil)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

(provide 'init-lang)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lang.el ends here
