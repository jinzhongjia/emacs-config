;;; init-edit.el --- Editing settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(add-hook 'prog-mode-hook
          (lambda ()
            (unless (eq major-mode 'org-mode)
              (display-line-numbers-mode 1))))

(setq make-backup-files nil) ; 不自动备份
(setq auto-save-default nil) ; 不使用Emacs自带的自动保存

;; 解除不常用的快捷键定义
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "s-q") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "M-m") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key [mouse-2] nil)

(use-package
  autorevert
  :defer t
  :ensure nil
  :config (global-auto-revert-mode)
  :custom
  (auto-revert-interval 10)
  (auto-revert-avoid-polling t)
  (auto-revert-verbose nil)
  (auto-revert-remote-files t)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t))

(if (executable-find "aspell")
    (progn
      ;; 在这里执行你的指令
      (setq-default ispell-program-name "aspell")  ;; 或者使用 "hunspell"
      (setq ispell-dictionary "english")            ;; 设置字典为英语
      (add-hook 'text-mode-hook 'flyspell-mode)    ;; 在文本模式下启用拼写检查
      (add-hook 'prog-mode-hook 'flyspell-prog-mode) ;; 在编程模式下启用拼写检查

      )
  (message "Warning: Aspell is not installed."))

(use-package indent-bars
  :ensure (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode))

(use-package symbol-overlay :defer t :hook (prog-mode . symbol-overlay-mode))

(use-package flycheck :defer t :init (global-flycheck-mode))

(use-package
 rainbow-delimiters
 :hook (prog-mode . rainbow-delimiters-mode))

(use-package savehist :ensure nil :init (savehist-mode) :defer t)

;; 自动记住每个文件的最后一次访问的光标位置
(use-package saveplace :ensure nil :init (save-place-mode) :defer t)

(use-package
  recentf
  :defer t
  :ensure nil
  :init (recentf-mode)
  :custom
  (recentf-max-saved-items 300)
  (recentf-exclude
   '("treemacs-persist"
     ;; 排除 ~/.emacs.d/ 目录下除了 readme.org 和 config.org 之外的所有文件
     (lambda (file)
       (and (string-match-p "^~/.emacs.d/" file)
            (not (string-match-p "\\(?:[Rr][Ee][Aa][Dd][Mm][Ee]\\.org\\|config\\.org\\)$" file))))
     ".org-registry.el"
     )))

(use-package olivetti
  :hook ((org-mode . olivetti-mode)
         (markdown-mode . olivetti-mode))
  :custom (olivetti-body-width 0.75))

(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode))

(use-package rime
  :ensure '(rime :type git :host github :repo "DogLooksGood/emacs-rime" :ref "d8c0a99")
  :demand t
  :custom
  (default-input-method "rime")
  :bind
  (:map rime-mode-map
        ("C-`" . 'rime-send-keybinding)))

;; (message "init-base configuration: %.2fs"
;;          (float-time (time-subtract (current-time) my/init-base-start-time)))

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
