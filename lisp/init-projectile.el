;;; init-projectile --- Jinyang's Projectile config
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode t)
  (custom-set-variables
   '(projectile-switch-project-action (quote projectile-dired)))
  (setq projectile-completion-system 'ivy))

(provide 'init-projectile)
;;; init-projectile.el ends here
