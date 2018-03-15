;;; init-dired --- Jinyang's Dired-mode config
;;; Commentary:
;;; Code:

(use-package dired
  :bind ("C-x C-j" . dired-jump)
  :config
  (setq dired-listing-switches "-laGh"))

(provide 'init-dired)
;;; init-dired.el ends here
