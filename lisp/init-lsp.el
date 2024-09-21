;;; init-lsp.el --- Development settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package
  lsp-bridge
  :defer t
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
