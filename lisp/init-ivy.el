;;; init-ivy --- Jinyang's Ivy config
;;; Commentary:
;;; Code:

(use-package ivy
  :diminish ivy-mode
  :bind (("C-s" . counsel-grep-or-swiper)
         ("M-x" . counsel-M-x)
	 ("C-r" . swiper)
         ("C-s" . counsel-grep-or-swiper)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("C-c k" . counsel-rg)
         ("C-c m" . counsel-imenu)
         ("C-c o" . ivy-occur))
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  ;; (setq ivy-re-builders-alist
  ;;       '((swiper . ivy--regex-plus)
  ;;         (t . ivy--regex-fuzzy)))
  (setq ivy-format-function 'ivy-format-function-arrow)
  )

(use-package ivy-bibtex
  :defer t
  :init
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil))

(use-package hydra :demand t)

(use-package ivy-hydra :after hydra :demand t)

(provide 'init-ivy)
;;; init-ivy.el ends here
