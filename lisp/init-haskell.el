;;; init-haskell --- Jinyang's Haskell Config
;;; Commentary:
;;; Code:

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

(provide 'init-haskell)
;;; init-haskell ends here
