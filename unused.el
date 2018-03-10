(use-package docker
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package tuareg
  :ensure t
  :defer t
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode)))

(use-package merlin
  :ensure t
  :defer t
  :init
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (add-hook 'merlin-mode-hook #'company-mode)
  (setq merlin-error-after-save nil))

(use-package ocp-indent
  :ensure t
  :defer t
  :init
  (add-hook 'tuareg-mode-hook 'ocp-indent-caml-mode-setup))

(use-package utop
  :ensure t
  :defer t
  :init
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  :config
  (if (executable-find "opam")
      (setq utop-command "opam config exec -- utop -emacs")
    (message "Cannot find opam executable.")))

(use-package flycheck-ocaml
  :ensure t
  :defer t
  :init
  (flycheck-ocaml-setup))

(use-package company-cabal
  :ensure t
  :defer t
  :init
  (add-to-list 'company-backends 'company-cabal))

(use-package company-ghc
  :ensure t
  :defer t)

(use-package intero
  :ensure t
  :defer t)

(use-package flycheck-haskell
  :ensure t
  :defer t
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-haskell-configure))

(use-package haskell-mode
  :ensure t
  :defer t
  :init
  (setq haskell-interactive-popup-errors nil
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-stylish-on-save nil)
  :config
  (with-eval-after-load 'align
    (add-to-list 'align-rules-list
                 '(haskell-types
                   (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                   (modes . haskell-modes)))
    (add-to-list 'align-rules-list
                 '(haskell-assignment
                   (regexp . "\\(\\s-+\\)=\\s-+")
                   (modes . haskell-modes)))
    (add-to-list 'align-rules-list
                 '(haskell-arrows
                   (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                   (modes . haskell-modes)))
    (add-to-list 'align-rules-list
                 '(haskell-left-arrows
                   (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                   (modes . haskell-modes)))))

(use-package hindent
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package hlint-refactor
  :ensure t
  :defer t)
