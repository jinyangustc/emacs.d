;;; init-latex --- Jinyang's LaTex config
;;; Commentary:
;;; Code:

(use-package auctex
  :defer t
  :ensure t
  :init
  (require 'reftex)
  (setq reftex-plug-into-AUCTex t)
  (add-hook 'LaTex-mode-hook 'turn-on-reftex)
  (setq TeX-view-program-selection '((output-pdf "Skim")))
  (setq TeX-view-program-list
        '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t)
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

(provide 'init-latex)
;;; init-latex.el ends here
