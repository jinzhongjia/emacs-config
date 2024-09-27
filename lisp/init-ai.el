;;; init-ai.el --- blog settings -*- lexical-binding: t -*-
;;; Commentary: Useful tools to make Emacs efficient!

;;; Code:

(use-package copilot-chat
  :ensure (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :custom
  (copilot-chat-curl-program "curl")
  :after (request org))

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
