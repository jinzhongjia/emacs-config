(unse-package corfu-terminal
 :ensure (corfu-terminal
   :type git
   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
 :defer t
 :config (corfu-terminal-mode t))

(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

;; enable snippet
(use-package yasnippet
  :defer t
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package doom-snippets
  :ensure (doom-snippets :type git :host github :repo "doomemacs/snippets" :files ("*.el" "*"))
  :defer t
  :after yasnippet)

;; 再装一个通用模板库，省得没 template 用
(use-package yasnippet-snippets
  :after (yasnippet))

;; for check
(use-package flycheck
  :defer t
  :config
  (setq truncate-lines nil) ; 如果单行信息很长会自动换行
  :hook
  (prog-mode . flycheck-mode))

;; magit
(use-package magit
  :defer t)

;; search in bufer
(use-package ctrlf
  :defer t
  :config
  (ctrlf-mode t)
  (dirvish-side-follow-mode t)
  (setq delete-by-moving-to-trash t)
  :bind 
  (("C-c m" . dirvish-side)))

(use-package google-translate
  :defer t)

(use-package esup
  :defer t)

(provide 'init-enhance)
