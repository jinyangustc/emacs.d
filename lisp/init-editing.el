;;; init-editing --- Jinyang's editing config
;;; Commentary:
;;; Code:

;; don't use tabs to indent
(setq-default indent-tabs-mode nil)

;; when you have a selection, typing text repalce it all
(delete-selection-mode t)

;; but maintain correct appearance
(setq-default tab-width 8)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; always end files with new line
(setq require-final-newline t)

;; don't highlight current line
(global-hl-line-mode -1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; disable lockfiles
(setq create-lockfiles nil)

;; backup and autosave files go into the tmp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; nice scrolling
;; (setq scroll-margin 0
;;       scroll-conservatively 100000
;;       scroll-preserve-screen-position 1)

;; undo visual tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))

;; setup flycheck to show on the right side of the buffer
(use-package flycheck
  :defer t
  :init
  (setq flycheck-indication-mode 'right-fringe)
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; whitespace
(use-package whitespace
  :config
  (whitespace-mode -1))

;; remove trailing whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(winner-mode 1)

;; show matching paren
(use-package paren
  :init
  (setq show-paren-style 'parenthesis)
  (show-paren-mode 1))

(remove-hook 'minibuffer-setup-hook 'winner-save-unconditionally)
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(prefer-coding-system 'utf-8)
(setq delete-by-moving-to-trash t)

;; revert buffers automatically when underlying files are changed
;; externally
(use-package autorevert
  :diminish auto-revert-mode
  :init
  (global-auto-revert-mode 1)
  (setq auto-revert-verbose nil))

(use-package abbrev
  :diminish abbrev-mode
  :init
  (setq save-abbrevs nil)
  (setq-default abbrev-mode t))

;; warn when opening file bigger than 100MB
(setq large-file-warning-threshold 100000000)

(use-package phi-search
  :bind (("C-c s" . phi-search)
         ("C-c r" . phi-search-backward)))

(use-package anzu
  :defer t
  :init
  (add-hook 'after-init-hook 'global-anzu-mode)
  (setq anzu-mode-lighter ""))

(use-package crux
  :bind (("S-RET" . crux-smart-open-line)
         ("C-k" . crux-smart-kill-line)
	 ("C-c O" . crux-open-with)
	 ("C-c I" . crux-find-user-init-file)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region))
  :config
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line))

(use-package rg :defer t)
(setq-default grep-highlight-matches t
	      grep-scroll-output t)
(setq-default localte-command "mdfind")

(use-package ace-window
  :bind ("C-x o" . 'ace-window))

(use-package avy
  :bind ("C-:" . 'avy-goto-char-timer))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  (define-key mc/keymap (kbd "<return>") nil))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :config
  (yas-global-mode t))

;; (use-package key-chord
;;   :config
;;   (key-chord-mode 1)
;;   ;; save buffer
;;   (key-chord-define-global "fs" 'save-buffer)
;;   ;; go to word
;;   (key-chord-define-global "jj" 'avy-goto-char-timer)
;;   ;; go to line
;;   (key-chord-define-global "jl" 'avy-goto-line)
;;   ;; go back to previous buffer
;;   (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
;;   ;;  undo
;;   (key-chord-define-global "uu" 'undo-tree-visualize)
;;   ;; shortcut for M-x is nice
;;   (key-chord-define-global "xx" 'counsel-M-x))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5)
  (which-key-setup-side-window-bottom))

(use-package youdao-dictionary
  :bind ("C-c y" . youdao-dictionary-search-at-point)
  :init
  (setq url-automatic-caching t)
  (setq youdao-dictionary-search-history-file "~/.emacs.d/.youdao"))

(defun join-lines (n)
  "Join N lines."
  (interactive "p")
    (if (use-region-p)
      (let ((fill-column (point-max)))
        (fill-region (region-beginning) (region-end)))
      (dotimes (_ (abs n))
        (delete-indentation (natnump n)))))

(use-package wgrep
  :defer t
  :bind (("C-x C-q" . wgrep-change-to-wgrep-mode)
         ("C-c C-c" . wgrep-finish-edit)))

(use-package smartparens
  :diminish smartparens-mode
  :bind (:map smartparens-mode-map
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-k" . sp-kill-hybrid-sexp)
              ("C-M-w" . sp-copy-sexp))
  :init
  (show-smartparens-global-mode +1)
  :config
  (use-package smartparens-config)
  (smartparens-global-mode 1)
  (sp-local-pair 'latex-mode "\\[" "\\]")
  (sp-local-pair 'org-mode "\\[" "\\]")
  (setq sp-based-key-bingdings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (smartparens-strict-mode +1)
                                    (rainbow-delimiters-mode +1))))

(use-package youdao-dictionary
  :bind ("C-c y" . youdao-dictionary-search-at-point)
  :init
  (setq url-automatic-caching t)
  (setq youdao-dictionary-search-history-file "~/.emacs.d/.youdao"))

(defun join-lines (n)
  "Join N lines."
  (interactive "p")
    (if (use-region-p)
      (let ((fill-column (point-max)))
        (fill-region (region-beginning) (region-end)))
      (dotimes (_ (abs n))
        (delete-indentation (natnump n)))))

(use-package wgrep
  :defer t
  :bind (("C-x C-q" . wgrep-change-to-wgrep-mode)
         ("C-c C-c" . wgrep-finish-edit)))

(use-package smartparens
  :diminish smartparens-mode
  :bind (:map smartparens-mode-map
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-k" . sp-kill-hybrid-sexp)
              ("C-M-w" . sp-copy-sexp))
  :init
  (show-smartparens-global-mode +1)
  :config
  (use-package smartparens-config)
  (smartparens-global-mode 1)
  (sp-local-pair 'latex-mode "\\[" "\\]")
  (sp-local-pair 'org-mode "\\[" "\\]")
  (setq sp-based-key-bingdings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (smartparens-strict-mode +1)
                                    (rainbow-delimiters-mode +1))))

(use-package lispy
  :diminish lispy-mode
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1))))

(use-package volatile-highlights
  :commands volatile-highlights-mode
  :config
  (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)

;; (use-package cnfonts
;;   :init
;;   (cnfonts-enable))

(provide 'init-editing)
;;; init-editing.el ends here
