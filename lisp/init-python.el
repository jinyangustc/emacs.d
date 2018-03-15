;;; init-python --- Jinyang's Python config
;;; Commentary:
;;; Code:

(use-package elpy
  :ensure t
  :defer t
  :config
  (elpy-enable))

(use-package pyenv-mode
  :ensure t
  :defer t)

(provide 'init-python)
;;; init-python ends here
