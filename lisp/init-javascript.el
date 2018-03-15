;;; init-javascript --- Jinyang's JavaScript config
;;; Commentary:
;;; Code:

(use-package js2-mode
  :ensure t
  :defer t
  :config
  (custom-set-variables
   '(js-indent-level 2)
   '(js2-basic-offset 2))
  (use-package js2-refactor
    :ensure t
    :init
    (js2r-add-keybindings-with-prefix "C-c C-r")
    (add-hook 'js2-mode-hook 'js2-refactor-mode-hook)))

(use-package js-doc
  :ensure t
  :defer t
  :bind (:map js2-refactor-mode-map
              ("C-c C-r i d" . js-doc-insert-function-doc)
              ("@" . js-doc-insert-tag)))

(use-package tide
  :ensure t
  :defer t
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode 1)
    (setq flycheck-check-syntax-automatically '(save mode-enable))
    (eldoc-mode 1)
    (tide-hl-identifier-mode 1)
    (company-mode 1))
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (setq tide-format-options
        '(:indentSize 2 :tabSize 2)))

(provide 'init-javascript)
;;; init-javascript.el ends here
