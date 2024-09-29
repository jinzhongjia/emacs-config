;;; init-ai.el --- blog settings -*- lexical-binding: t -*-
;;; Commentary: Useful tools to make Emacs efficient!

;;; Code:

(use-package copilot
  :ensure (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :hook (prog-mode . copilot-mode)
  :bind
  (("C-c n" . copilot-next-completion)
   ("C-c p" . copilot-previous-completion)
   ("C-c w" . copilot-accept-completion-by-word)
   ("C-c l" . copilot-accept-completion-by-line)
   ("C-c RET" . rk/copilot-complete-or-accept))
  :config
  (defun rk/copilot-complete-or-accept ()
    "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
    (interactive)
    (if (copilot--overlay-visible)
        (progn
          (copilot-accept-completion)
          (open-line 1)
          (next-line))
      (copilot-complete)))
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

(use-package copilot-chat
  :ensure (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :custom
  (copilot-chat-curl-program "curl")
  :after (request org))

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
