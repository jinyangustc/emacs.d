;;; init-rust --- Jinyang's Rust config
;;; Commentary:
;;; Code:

(use-package cargo
  :ensure t
  :defer t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package rust-mode
  :ensure t
  :defer t
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c <tab>") #'rust-format-buffer))))

(use-package racer
  :ensure t
  :defer t
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package company-racer
  :ensure t
  :defer t)

(use-package flycheck-rust
  :ensure t
  :defer t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'init-rust)
;;; init-rust.el ends here
