;;; init-racket --- Jinyang's Racket config
;;; Commentary:
;;; Code:

(use-package racket-mode
  :defer t
  :mode (("\\.rkt\\'" . racket-mode))
  :config
  (add-hook 'racket-mode-hook
            (lambda ()
              (progn
                (smartparens-strict-mode +1)
                (rainbow-delimiters-mode +1))))
  (add-hook 'racket-repl-mode-hook
            (lambda ()
              (progn
                (smartparens-strict-mode +1)
                (rainbow-delimiters-mode +1)))))

(provide 'init-racket)
;;; init-racket.el ends here
