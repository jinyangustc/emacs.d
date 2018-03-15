;;; init-latex --- Jinyang's LaTex config
;;; Commentary:
;;; Code:

(use-package auctex
  :defer t
  :ensure t
  :init
  (setq reftex-plug-into-AUCTex t)
  (add-hook 'LaTex-mode-hook 'turn-on-reftex)
  :config
  (setq-default Tex-master nil)
  (setq Tex-auto-save t)
  (setq Tex-parse-self t)
  (setq Tex-save-query nil)
  (setq Tex-source-correlate-mode t)
  (setq Tex-PDF-mode t))

(use-package company-auctex
  :requires company
  :ensure t
  :defer t
  :config
  (company-auctex-init))

(use-package auctex-latexmk
  :ensure t
  :defer t
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (auctex-latexmk-setup))

(provide 'init-latex)
;;; init-latex.el ends here
