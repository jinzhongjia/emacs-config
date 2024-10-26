;;; init-dev.el --- Development settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package transient :defer t)

(use-package magit
  :after transient)

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package diff-hl :defer t :init (global-diff-hl-mode t))

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package magit-file-icons
  :ensure t
  :after magit
  :init
  (magit-file-icons-mode 1))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package emsg-blame
  :ensure
  '(emsg-blame
    :type git
    :host github
    :repo "ISouthRain/emsg-blame")
  :custom
  (emsg-blame-background nil "enable emsg blame background")
  (emsg-blame-display
   (lambda ()
     "Display git blame message, right-aligned with Magit-style faces.
If another message is already being displayed, display both messages unless they
do not both fit in the echo area."
     (let* ((message-log-max nil) ; prevent messages from being logged to *Messages*
            (cur-msg (or (current-message) ""))
	        (blm-msg (format "%s %s %s "
			                 emsg-blame--commit-summary
			                 (propertize emsg-blame--commit-author 'face 'error)
			                 (propertize emsg-blame--commit-date 'face 'warning)))
	        (available-width (max 0 (- (frame-width) (string-width cur-msg) 1)))
	        (blm-msg-width (string-width blm-msg))
	        (padding (max 0 (- available-width blm-msg-width)))
	        (rev-blm-msg (concat (make-string padding ?\s) blm-msg)))
       (if (> blm-msg-width available-width)
	       (message blm-msg)
         (message (concat cur-msg rev-blm-msg))))))
  :config (global-emsg-blame-mode))

(provide 'init-dev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dev.el ends here
