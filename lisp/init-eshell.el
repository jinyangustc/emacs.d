(global-set-key (kbd "C-c e") 'eshell)

;; Visual commands are commands which require a proper terminal.
;; eshell will run them in a term buffer when you invoke them.
(setq eshell-visual-commands
      '("less" "tmux" "htop" "top" "bash" "zsh" "fish"))
(setq eshell-visual-subcommands
      '(("git" "log" "l" "diff" "show")))

;; Define a pretty prompt.
(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'robbyrussell))

(add-hook 'eshell-mode-hook
          (lambda ()
            (company-mode 0)))

;; esh-autosuggest provides fish shell like autosuggestion from history.
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(provide 'init-eshell)
