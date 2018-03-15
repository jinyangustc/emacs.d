;;; init-compile --- Jinyang's compile config
;;; Commentary:
;;; Code:

(use-package compile
  :config
  (add-to-list 'compilation-error-regexp-alist 'node)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 2 3 4)))

(provide 'init-compile)
;;; init-compile.el ends here
