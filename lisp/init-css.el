;;; init-css --- Jinyang's CSS config
;;; Commentary:
;;; Code:

(use-package rainbow-mode
  :ensure t
  :init
  (rainbow-mode 1))

(use-package css-mode
  :mode (("\\.css\\'" . css-mode)
         ("\\.scss\\'" . css-mode)
         ("\\.sass\\'" . css-mode))
  :init
  (setq css-indent-offset 2))

(provide 'init-css)
;;; init-css.el ends here
