;;; init-clojure --- Jinyang's Clojure config
;;; Commentary:
;;; Code:

(use-package clojure-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                 (progn
                                   (smartparens-strict-mode +1)
                                   (subword-mode +1)
                                   (rainbow-delimiters-mode +1)))))

(use-package cider
  :ensure t
  :defer t
  :config
  (setq nrepl-log-message t)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook (lambda ()
                                    (progn
                                      (run-hooks 'prelude-lisp-coding-hook)
                                      (subword-mode +1)))))

(provide init-clojure)
;;; init-clojure.el ends here
