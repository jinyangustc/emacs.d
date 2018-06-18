;;; init-projectile --- Jinyang's Projectile config
;;; Commentary:
;;; Code:

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-mode-line nil)
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching nil)
  (setq projectile-verbose nil)
  (setq projectile-switch-project-action
        (lambda ()
          (dired (projectile-project-root)))))

(provide 'init-projectile)
;;; init-projectile.el ends here
