;;; package --- Summary
;;; Commentary:
;;; Code:

;;
;; * System
;;

;; system dependent flag
(defconst *is-a-mac* (eq system-type 'darwin))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; custome file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;
;; * Setup Packages
;;
(require' package)
(setq package-enable-at-startup nil)

;; Install into separate package dirs for each Emacs version, to
;; prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))

(setq package-archives '(("org" . "http://elpa.emacs-china.org/org/")
			 ("melpa" . "http://elpa.emacs-china.org/melpa/")
			 ("gnu" . "http://elpa.emacs-china.org/gnu/")))

(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(use-package diminish :ensure t)
(use-package bind-key :ensure t)

;;
;; * UI and keys
;;
(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq-default default-input-method "MacOSX"))
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
;; (when (fboundp 'menu-bar-mode)
;;   (menu-bar-mode -1))
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(add-hook 'term-mode-hook (lambda () (setq line-spacing 0)))
(setq column-number-mode t)

(use-package mode-line-bell
  :ensure t
  :init (mode-line-bell-mode))

;; fringe width (in pixels)
(fringe-mode '(8 . 8))

;;
;; * Font and themes
;;
;; (use-package color-theme-sanityinc-tomorrow :ensure t)
;; (setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))
(use-package default-text-scale
  :ensure t
  :init
  (add-hook 'after-init-hook 'default-text-scale-mode))
(when (member "Triplicate T4c" (font-family-list))
  (set-face-attribute 'default nil :font "Triplicate T4c-14"))

;;
;; * Editing
;;

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
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; undo visual tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))

;; setup flycheck to show on the right side of the buffer
(use-package flycheck
  :init
  (setq flycheck-indication-mode 'right-fringe))

;; whitespace
(use-package whitespace
  :config
  (whitespace-mode -1))

;; remove trailing whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; autosave when switching buffers, windows or frames.
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

(add-hook 'focus-out-hook
          (lambda () (when buffer-file-name (save-buffer))))

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
(when (eq system-type 'darwin)
  (use-package osx-trash
    :ensure t
    :config
    (osx-trash-setup)))

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

;;
;; * Completion
;;

(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0.4
	company-minium-prefix-length 3
	company-show-numbers t)
  (setq company-frontends
	'(company-pseudo-tooltip-unless-just-one-frontend
	  company-preview-if-just-one-frontend))
  (setq company-backends
	'(company-elisp
	  company-capf
	  (company-dabbrev-code company-keywords)
	  company-files
	  company-dabbrev))
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (defun ora-company-number ()
    "Forward to `company-complete-number'.
Unless the number is potentially part of the candidate.
In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
	   (re (concat "^" company-prefix k)))
      (if (cl-find-if (lambda (s) (string-match re s))
		      company-candidates)
	  (self-insert-command 1)
	(company-complete-number
	 (if (equal k "0")
	     10
	   (string-to-number k))))))

  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x) 'ora-company-number))
	  (number-sequence 0 9))
    (define-key map " " (lambda ()
			  (interactive)
			  (company-abort)
			  (self-insert-command 1)))
    (define-key map (kbd "<return>") nil)))

(use-package company-math
  :ensure t
  :config
  (add-to-list 'company-backends 'company-math-symbols-latex)
  ;; (add-to-list 'company-backends 'company-math-symbols-unicode)
  (setq company-math-allow-latex-symbols-in-faces t))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-s" . counsel-grep-or-swiper)
	 ("C-r" . swiper)
         ("C-s" . counsel-grep-or-swiper)
         ("C-x C-r" . counsel-recentf)
         ("C-c k" . counsel-rg)
         ("C-c i" . counsel-imenu)
         ("C-c o" . ivy-occur))
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  ;; (setq ivy-re-builders-alist
  ;;       '((swiper . ivy--regex-plus)
  ;;         (t . ivy--regex-fuzzy)))
  (setq ivy-format-function 'ivy-format-function-arrow)
  )

(use-package phi-search
  :ensure t
  :bind (("C-c s" . phi-search)
         ("C-c r" . phi-search-backward)))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(use-package flx :ensure t)

(use-package smex :ensure t)

;;
;; * Packages
;;
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package dired
  :bind ("C-x C-j" . dired-jump)
  :config
  (setq dired-listing-switches "-laGh"))

(use-package anzu
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-anzu-mode)
  (setq anzu-mode-lighter ""))

(use-package rg
  :ensure t)
(setq-default grep-highlight-matches t
	      grep-scroll-output t)
(setq-default localte-command "mdfind")

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package crux
  :ensure t
  :bind (("S-RET" . crux-smart-open-line)
         ("C-k" . crux-smart-kill-line)
	 ("C-c O" . crux-open-with)
	 ("C-c I" . crux-find-user-init-file)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region))
  :config
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line))

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package pyenv-mode
  :ensure t)

(use-package magit
  :ensure t
  :commands magit-status
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode diff-hl-dired-mode)
  :config
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode)
  (diff-hl-dired-mode)
  (setq diff-hl-draw-borders nil)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package avy
  :ensure t
  :bind ("C-:" . 'avy-goto-char-timer))

(use-package lispy
  :ensure t
  :diminish lispy-mode
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))

(use-package ace-window
  :ensure t
  :bind ("C-x o" . 'ace-window))

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

;; (use-package iedit
;;   :ensure t
;;   :commands iedit-mode
;;   :bind ("C-;" . iedit-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this))
  :config
  (define-key mc/keymap (kbd "<return>") nil))

(use-package docker
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode t))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode t)
  (custom-set-variables
   '(projectile-switch-project-action (quote projectile-dired)))
  (setq projectile-completion-system 'ivy))

(use-package windmove
  :ensure t
  :config
  (windmove-default-keybindings 'super))

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  ;; save buffer
  (key-chord-define-global "fs" 'save-buffer)
  ;; go to word
  (key-chord-define-global "jj" 'avy-goto-char-timer)
  ;; go to line
  (key-chord-define-global "jl" 'avy-goto-line)
  ;; go back to previous buffer
  (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
  ;;  undo
  (key-chord-define-global "uu" 'undo-tree-visualize)
  ;; shortcut for M-x is nice
  (key-chord-define-global "xx" 'counsel-M-x))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5)
  (which-key-setup-side-window-bottom))

(use-package zoom-frm
  :ensure t)

(use-package emmet-mode
  :ensure t
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (setq emmet-indentation 2))

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.jsp\\'" . web-mode))
  :config
  (custom-set-variables
   '(web-mode-markup-indent-offset 2)
   '(web-mode-css-indent-offset 2)
   '(web-mode-code-indent-offset 2)
   '(web-mode-enable-auto-quoting nil)
   '(css-indent-offset 2)))

(use-package js2-mode
  :ensure t
  :config
  (custom-set-variables
   '(js-indent-level 2)
   '(js2-basic-offset 2)))

(use-package js2-refactor
  :ensure t
  :init
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (add-hook 'js2-mode-hook 'js2-refactor-mode-hook))

(use-package js-doc
  :ensure t
  :bind (:map js2-refactor-mode-map
              ("C-c C-r i d" . js-doc-insert-function-doc)
              ("@" . js-doc-insert-tag)))

(use-package tide
  :ensure t
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode 1)
    (setq flycheck-check-syntax-automatically '(save mode-enable))
    (eldoc-mode 1)
    (tide-hl-identifier-mode 1)
    (company-mode 1))
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (setq tide-format-options
        '(:indentSize 2 :tabSize 2)))

(use-package compile
  :init
  (add-to-list 'compilation-error-regexp-alist 'node)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 2 3 4)))

(use-package rainbow-mode
  :ensure t
  :init
  (rainbow-mode 1))

(use-package css-mode
  :mode (("\\.css\\'" . css-mode)
         ("\\.scss\\'" . css-mode)
         ("\\.sass\\'" . css-mode))
  :init
  (setq css-indent-offset 2))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))

(use-package youdao-dictionary
  :ensure t
  :bind ("C-c y" . youdao-dictionary-search-at-point)
  :init
  (setq url-automatic-caching t)
  (setq youdao-dictionary-search-history-file "~/.emacs.d/.youdao"))

(use-package org-download
  :ensure t)

(defun join-lines (n)
  "Join N lines."
  (interactive "p")
    (if (use-region-p)
      (let ((fill-column (point-max)))
        (fill-region (region-beginning) (region-end)))
      (dotimes (_ (abs n))
        (delete-indentation (natnump n)))))

(use-package wgrep
  :ensure t
  :bind (("C-x C-q" . wgrep-change-to-wgrep-mode)
         ("C-c C-c" . wgrep-finish-edit)))

(use-package org-plus-contrib
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-highlight-latex-and-related '(latex))
  (setq org-pretty-entities t))

(provide 'init)
;;; init.el ends here
