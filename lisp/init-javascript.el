;;; init-javascript --- Jinyang's JavaScript config
;;; Commentary:
;;; Code:

(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :config
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (custom-set-variables
   '(js-indent-level 2)
   '(js2-basic-offset 2))
  (use-package js2-refactor
    :init
    (js2r-add-keybindings-with-prefix "C-c C-r")
    (add-hook 'js2-mode-hook #'js2-refactor-mode)))

(use-package xref-js2
  :after js2-mode
  :init
  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode)))

(use-package json-mode)

(use-package js-doc
  :defer t
  :bind (:map js2-refactor-mode-map
              ("C-c C-r i d" . js-doc-insert-function-doc)
              ("@" . js-doc-insert-tag)))

(provide 'init-javascript)
;;; init-javascript.el ends here
