;;; package --- Summary
;;; Commentary:
;;; Code:

;; system dependent flag
(defconst *is-a-mac* (eq system-type 'darwin))

;; adjust garbage collection thresholds during startup, and thereafter
;; (let ((normal-gc-cons-threshold (* 20 1024 1024))
;;       (init-gc-cons-threshold (* 128 1024 1024)))
;;   (setq gc-cons-threshold init-gc-cons-threshold)
;;   (add-hook 'after-init-hook
;;             (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

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
  (setq mac-option-modifier 'none)
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

(defalias 'yes-or-no-p 'y-or-n-p)
(setq indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(winner-mode 1)
(show-paren-mode 1)
(remove-hook 'minibuffer-setup-hook 'winner-save-unconditionally)
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq create-lockfiles nil)
(prefer-coding-system 'utf-8)
(setq delete-by-moving-to-trash t)
(setq save-abbrevs nil)
(setq-default abbrev-mode t)

;;
;; * Company
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

(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode diff-hl-dired-mode))

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

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-s" . counsel-grep-or-swiper)
	 ("C-r" . swiper))
  :init
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind ("C-s" . counsel-grep-or-swiper)
  :bind ("C-x C-r" . counsel-recentf)
  :bind ("C-c k" . counsel-rg))

(use-package crux
  :ensure t
  :bind (("C-k" . crux-smart-kill-line)
	 ("C-c o" . crux-open-with)
	 ("C-c I" . crux-find-user-init-file))
  :config
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line))

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package magit
  :ensure t
  :commands magit-status
  :bind ("C-x g" . magit-status))

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

(use-package iedit
  :ensure t
  :commands iedit-mode
  :bind ("C-;" . iedit-mode))

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
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))
(provide 'init)
;;; init.el ends here
