;;; init-tools.el --- Tools settings -*- lexical-binding: t -*-
;;; Commentary: Useful tools to make Emacs efficient!

;;; Code:

(when (string= system-type "gnu/linux")
  (defun detect-display-server-via-xdg ()
    "通过 $XDG_SESSION_TYPE 检测当前桌面环境: Wayland 或 Xorg，返回相应的值。"
    (let ((session-type (getenv "XDG_SESSION_TYPE")))
      (cond
       ((string= session-type "wayland")
        t)  ;; 返回 true
       ((string= session-type "x11")
        nil) ;; 返回 false
       (t
        nil)))) ;; 返回 nil
)

(if (or (eq system-type 'windows-nt)
          (eq system-type 'gnu/linux))
      (defun paste-img ()
        "paste image from clipboard"
        (interactive)
        (let* ((file-path (buffer-file-name))
               (directory (if file-path
                              (file-name-directory file-path)
                            (error "No associated file for the current buffer")))
               (image-directory (expand-file-name "image" directory))
               (timestamp (format-time-string "%Y%m%d_%H%M%S"))
               (format (completing-read "Select image form:" '("png" "jpg")))
               (image-name (format "image_%s.%s" timestamp format))
               (image-path (expand-file-name image-name image-directory))
               (image-format (if (string= format "jpg") "Jpeg" "Png"))
               (script (format "Add-Type -AssemblyName System.Windows.Forms; $clipboardImage = [System.Windows.Forms.Clipboard]::GetImage(); if ($clipboardImage -ne $null) { $clipboardImage.Save('%s', [System.Drawing.Imaging.ImageFormat]::%s); Write-Host 'Image saved'; } else { Write-Host 'No image in clipboard'; }" image-path image-format)))

          (unless (file-exists-p image-directory)
            (make-directory image-directory t))

(if (eq system-type 'gnu/linux)
(if (detect-display-server-via-xdg)
            ;; Wayland
            (progn
              (if (string= format "png")
                  (call-process "sh" nil nil nil "-c"  (format "wl-paste --type image/png > %s" image-path))
                (call-process "sh" nil nil nil "-c" (format "wl-paste --type image/png | convert - %s" image-path)))
              )
          ;; Xorg
          (progn
            (call-process "sh" nil nil nil "-c" (format "xclip -selection clipboard -t image/png -o | convert - %s" image-path))
            ))

          (call-process "pwsh" nil nil nil "-Command" script))

          (if (file-exists-p image-path)
              (progn
                (insert (format "[[file:%s]]" (concat "image/" image-name))) ; 插入正确的相对路径
                (message "Image successfully saved to: %s" image-path))
            (message "No image in clipboard or image not saved")))))

(use-package undo-fu
  :bind (("C-z" . undo-fu-only-undo)   ;; 绑定 C-z 为只撤销
         ("C-S-z" . undo-fu-only-redo))) ;; 绑定 C-S-z 为只重做

(use-package vundo
    :defer t
    :custom
    (vundo-glyph-alist vundo-unicode-symbols "beautify unicode for tree")
    :bind (("C-x u" . vundo)))

(when (eq system-type 'windows-nt)
  (if (executable-find "pwsh")
      (setq explicit-shell-file-name "pwsh")))

(use-package
   crux
   :bind
   (("C-x K" . crux-kill-other-buffers)
    ("C-k" . crux-smart-kill-line)
    ("C-c r" . crux-rename-file-and-buffer))
   :config
   (crux-with-region-or-buffer indent-region)
   (crux-with-region-or-buffer untabify)
   (crux-with-region-or-point-to-eol kill-ring-save)
   (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(use-package
 mwim
 :bind
 ("C-a" . mwim-beginning-of-code-or-line)
 ("C-e" . mwim-end-of-code-or-line))

(use-package ace-window :bind (("C-x o" . 'ace-window)))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key helpful-mode)
  :bind (([remap describe-command] . helpful-command)
         ("C-h f" . helpful-callable)
         ("C-h F" . helpful-function)
         ("C-h v" . helpful-variable)
         ("C-c C-d" . heloful-at-point)
         ("C-h x" . helpful-command)
         ([remap describe-key] . helpful-key))
  )

(provide 'init-tools)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tools.el ends here
