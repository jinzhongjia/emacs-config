;; config for melpa

;; disable signature checking

(setq package-check-signature nil) 

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless (bound-and-true-p package--initialized)
    (package-initialize))

;; refresh package archive

(unless package-archive-contents
    (package-refresh-contents))

;;;;;;;;;;;;;;;;;;;;

;; bootstrap for use-package, for manager package easily

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

;; config for use-package

(eval-when-compile
;; make use-package default behavior better
;; with `use-package-always-ensure' you won't need ":ensure t" all the time
;; with `use-package-always-defer' you won't need ":defer t" all the time
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-enable-imenu-support t
        ;; use-package-verbose t
        use-package-expand-minimally t)
  (require 'use-package))

;; for autoupdate package
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;;;;;;;;;;;;;;;;;;;;

(provide 'init-elpa)
