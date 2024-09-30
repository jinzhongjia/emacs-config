;;; init-lsp.el --- Development settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  (lsp-idle-delay 0.500)
  ;; 这会禁用掉某些无用的东西
  (lsp-completion-provider :none)
  :hook ((prog-mode . (lambda ()
                        (unless (or (derived-mode-p 'lisp-mode 'emacs-lisp-mode))
                          (lsp-deferred))))))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

(use-package
  lsp-bridge
  :defer t
  :disabled t
  :ensure
  '(lsp-bridge
    :type git
    :host github
    :repo "manateelazycat/lsp-bridge"
    :files
    (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
    :build (:not compile))
  :init (global-lsp-bridge-mode))

(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
