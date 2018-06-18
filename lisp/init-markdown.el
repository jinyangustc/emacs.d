;;; init-markdown --- Jinyang's Markdown config
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :commands (markdonw-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'init-markdown)
;;; init-markdown.el ends here
