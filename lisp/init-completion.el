;;; init-completion.el --- Completion settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package smartparens
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package
 vertico
 :defer t
 :custom
 (vertico-scroll-margin 0) ;; Different scroll margin
 (vertico-count 20) ;; Show more candidates
 (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
 (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
 :init (vertico-mode))

(use-package
 emacs
 :ensure nil
 :custom
 ;; Support opening new minibuffers from inside existing minibuffers.
 (enable-recursive-minibuffers t)
 ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
 ;; mode.  Vertico commands are hidden in normal buffers. This setting is
 ;; useful beyond Vertico.
 (read-extended-command-predicate #'command-completion-default-include-p)
 :init
 ;; Add prompt indicator to `completing-read-multiple'.
 ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
 (defun crm-indicator (args)
   (cons
    (format "[CRM%s] %s"
            (replace-regexp-in-string
             "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
            (car args))
    (cdr args)))
 (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

 ;; Do not allow the cursor in the minibuffer prompt
 (setq minibuffer-prompt-properties
       '(read-only t cursor-intangible t face minibuffer-prompt))
 (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; Configure directory extension.
(use-package
 vertico-directory
 :after vertico
 :ensure nil
 ;; More convenient directory navigation commands
 :bind
 (:map
  vertico-map
  ("RET" . vertico-directory-enter)
  ("DEL" . vertico-directory-delete-char)
  ("M-DEL" . vertico-directory-delete-word))
 ;; Tidy shadowed file names
 :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-mouse
  :after vertico
  :ensure nil
  :config (vertico-mouse-mode))

;; orderless 是一种哲学思想
(use-package
 orderless
 :defer t
 :custom
 ;; Configure a custom style dispatcher (see the Consult wiki)
 ;(orderless-style-dispatchers
 ; '(+orderless-consult-dispatch orderless-affix-dispatch))
 ;(orderless-component-separator #'orderless-escapable-split-on-space)
 (completion-styles '(orderless flex))
 (orderless-matching-styles
  '(orderless-regexp orderless-literal orderless-flex))
 (completion-category-defaults nil)
 (completion-category-overrides '((file (styles partial-completion)))))

(use-package
 marginalia
 :after vertico
 :config (marginalia-mode)
 :custom
 (marginalia-annotators
  '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package
  consult
  :bind
  ( ;; C-c bindings in `mode-specific-map'
   ("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ([remap Info-search] . consult-info)
   ([remap isearch-forward] . consult-line)
   ([remap isearch-backward] . consult-line)
   ;; C-x bindings in `ctl-x-map'
   ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
   ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
   ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop) ;; orig. yank-pop
   ;; M-g bindings in `goto-map'
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line) ;; orig. goto-line
   ("M-g M-g" . consult-goto-line) ;; orig. goto-line
   ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; M-s bindings in `search-map'
   ("M-s d" . consult-find) ;; Alternative: consult-fd
   ("M-s c" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map
   isearch-mode-map
   ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
   ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
   ;; Minibuffer history
   :map
   minibuffer-local-map
   ("M-s" . consult-history) ;; orig. next-matching-history-element
   ("M-r" . consult-history) ;; orig. previous-matching-history-element
   :map
   org-mode-map
   ("C-c C-j" . consult-org-heading))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq
   register-preview-delay 0.5
   register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq
   xref-show-xrefs-function #'consult-xref
   xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.4 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "M-.")
  ;; 让 consult 支持预览 org 时使用 org-modern 和 olivetti-mode
  (add-to-list 'consult-preview-allowed-hooks 'global-org-modern-mode)
  (add-to-list 'consult-preview-allowed-hooks 'olivetti-mode)
  )

(use-package consult-flycheck
  :after consult)

(use-package consult-todo
  :after consult)

(use-package consult-ls-git
  :after consult
  :bind
  (("C-c g f" . #'consult-ls-git)
   ("C-c g F" . #'consult-ls-git-other-window)))

(use-package consult-dir
  :after consult
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-yasnippet
  :after consult)

(use-package corfu
  :defer t
  :after savehist
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-separator ?\s)
  (corfu-preselect 'prompt)
  (corfu-scroll-margin 5)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package emacs
  :ensure nil
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  :ensure nil
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package
  cape
  :defer t
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword) ; programming language keyword
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol) ; elisp symbol
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;(add-to-list 'completion-at-point-functions #'cape-line)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  (add-hook 'completion-at-point-functions #'cape-tex)
  (add-hook 'completion-at-point-functions #'cape-history)
  :config
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package yasnippet-capf
  :after cape yasnippet
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package yasnippet :defer t
  :config
  (yas-global-mode 1))

(use-package
 doom-snippets
 :ensure
 (doom-snippets
  :type git
  :host github
  :repo "doomemacs/snippets"
  :files ("*.el" "*"))
 :after yasnippet)

;; 再装一个通用模板库，省得没 template 用
(use-package yasnippet-snippets :after yasnippet)

(provide 'init-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
