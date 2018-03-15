;;; init-docer --- Jinyang's docker config
;;; Commentary:
;;; Code:
(use-package docker
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(provide 'init-docker)
;;; init-docker.el ends here
