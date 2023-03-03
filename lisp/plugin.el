;; package benchmark
(use-package benchmark-init
             :init (benchmark-init/activate)
             :hook (after-init . benchmark-init/deactivate))

(provide 'plugin)