;;; init-html --- Jinyang's HTML config
;;; Commentary:
;;; Code:

(use-package emmet-mode
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (setq emmet-indentation 2))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.jsp\\'" . web-mode))
  :config
  (custom-set-variables
   '(web-mode-markup-indent-offset 2)
   '(web-mode-css-indent-offset 2)
   '(web-mode-code-indent-offset 2)
   '(web-mode-enable-auto-quoting nil)
   '(css-indent-offset 2)))

(provide 'init-html)
;;; init-html.el ends here
