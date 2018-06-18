;;; init-r --- Jinyang's R config
;;; Commentary:
;;; Code:

(use-package ess-site
  :ensure ess
  :defer t
  :mode (("/R/.*\\.q\\'" . R-mode)
         ("\\.[rR]\\'" . R-mode))
  :config
  (setq ess-first-continued-statement-offset 2
        ess-continued-statement-offset 0
        ess-expression-offset 2
        ess-nuke-trailing-whitespace-p t
        ess-default-style 'DEFAULT))

(use-package ess-R-data-view
  :defer t
  :bind ("C-c v" . ess-R-dv-pprint))

(provide 'init-r)
;;; init-r.el ends here
