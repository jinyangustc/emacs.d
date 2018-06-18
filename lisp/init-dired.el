;;; init-dired --- Jinyang's Dired-mode config
;;; Commentary:
;;; Code:

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(use-package dired
  :bind ("C-x C-j" . dired-jump)
  :config
  (setq dired-listing-switches "-laGh"))

(provide 'init-dired)
;;; init-dired.el ends here
