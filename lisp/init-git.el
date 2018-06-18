;;; init-git --- Jinyang's git config
;;; Commentary:
;;; Code:

(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :commands (diff-hl-mode diff-hl-dired-mode)
  :config
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'init-git)
;;; init-git.el ends here
