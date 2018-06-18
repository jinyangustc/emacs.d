;;; init-flyspell --- Jinyang's Flyspell config
;;; Commentary:
;;; Code:

(use-package flyspell
  :commands (flyspell-prog-mode flyspell-mode)
  :diminish flyspell-mode
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (use-package ispell
    :config
    (setq ispell-personal-dictionary "~/.aspell.en.pws"))
  :config
  (unbind-key "C-;" flyspell-mode-map)
  ;; (set-face-attribute 'flyspell-duplicate nil
  ;;                     :foreground "white"
  ;;                     :background "#d54e53")
  ;; (set-face-attribute 'flyspell-incorrect nil
  ;;                     :foreground "white"
  ;;                     :background "#d54e53")
  )

(use-package flyspell-correct
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic)
  :after flyspell)

(use-package flyspell-correct-ivy
  :demand t
  :bind (:map flyspell-mode-map
              ("<f8>" . flyspell-correct-next-word-generic)))

(provide 'init-flyspell)
;;; init-flyspell.el ends here
