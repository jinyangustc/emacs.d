;;; init-ocaml --- Jinyang's OCaml config
;;; Commentary:
;;; Code:

(use-package tuareg
  :defer t
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode)))

(use-package merlin
  :defer t
  :init
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (add-hook 'merlin-mode-hook #'company-mode)
  (setq merlin-error-after-save nil))

(use-package ocp-indent
  :defer t
  :init
  (add-hook 'tuareg-mode-hook 'ocp-indent-caml-mode-setup))

(use-package utop
  :defer t
  :init
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  :config
  (if (executable-find "opam")
      (setq utop-command "opam config exec -- utop -emacs")
    (message "Cannot find opam executable.")))

(use-package flycheck-ocaml
  :defer t
  :init
  (flycheck-ocaml-setup))

(provide 'init-ocaml)
;;; init-ocaml.el ends here
