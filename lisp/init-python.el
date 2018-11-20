;;; init-python --- Jinyang's Python config
;;; Commentary:
;;; Code:

(use-package elpy
  :defer t
  :bind (("M-." . elpy-goto-definition))
  :config
  (elpy-enable))

(use-package pyenv-mode :defer t)

(provide 'init-python)
;;; init-python ends here
