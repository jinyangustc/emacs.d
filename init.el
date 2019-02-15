(defconst *is-a-mac* (eq system-type 'darwin))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; custome file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; emacs-mac specific
(setq mac-pass-control-to-system nil)

;; ==================================================
;; ELPA, Packages and `use-package'
;; ==================================================
(require 'package)
;; Install into separate package dirs for each Emacs version, to
;; prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))

;; package archives URLs
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")))
;; turn off automatic package installation at startup
(setq package-enable-at-startup nil)

;; initialize packages
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; always ensure package
(setq use-package-always-ensure nil)

(use-package diminish)
(use-package bind-key)

;; ==================================================
;; UI configuration
;; ==================================================
(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq-default default-input-method "MacOSX"))

;; turn off dialog box
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; no startup screen
(setq inhibit-startup-screen t)

;; no initiali scratch message
(setq initial-scratch-message "")

;; no tool bar
(tool-bar-mode -1)

;; no scroll bar
(set-scroll-bar-mode nil)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(add-hook 'term-mode-hook (lambda () (setq line-spacing 0)))

;; show column number
(setq column-number-mode t)

;; flush mode line for ring
(use-package mode-line-bell
  :init (mode-line-bell-mode))

;; package to adjust text size
(use-package default-text-scale
  :init
  (add-hook 'after-init-hook 'default-text-scale-mode))

;; color theme
(setq custom-safe-themes t)
(use-package solarized-theme
  :config (load-theme 'solarized-light))

;; ==================================================
;; Editing
;; ==================================================

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
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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
(prefer-coding-system 'utf-8)
(setq delete-by-moving-to-trash t)

;; revert buffers automatically when underlying files are changed
;; externally
(use-package autorevert
  :diminish auto-revert-mode
  :init
  (global-auto-revert-mode 1)
  (setq auto-revert-verbose nil))

;; warn when opening file bigger than 100MB
(setq large-file-warning-threshold 100000000)

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

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :config
  (yas-global-mode t))

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

;; ==================================================
;; Completion
;; ==================================================

(use-package company
  :diminish company-mode
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
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
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
  :config
  (add-to-list 'company-backends 'company-math-symbols-latex)
  (setq company-math-allow-latex-symbols-in-faces t))

(use-package counsel-projectile
  :bind ("C-c p" . projectile-command-map)
  :config
  (counsel-projectile-mode))

(use-package flx)

(use-package smex)

;; ==================================================
;; Git
;; ==================================================
(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :commands (diff-hl-mode diff-hl-dired-mode)
  :config
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; ==================================================
;; Dired
;; ==================================================
(use-package dired
  :bind ("C-x C-j" . dired-jump)
  :config
  (setq dired-listing-switches "-laGh")
  (when *is-a-mac*
    (setq dired-use-ls-dired nil)))

;; ==================================================
;; Projectile
;; ==================================================
(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-mode-line nil)
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching nil)
  (setq projectile-verbose nil)
  (setq projectile-switch-project-action
        (lambda ()
          (dired (projectile-project-root)))))

;; ==================================================
;; Ivy
;; ==================================================
(use-package ivy
  :diminish ivy-mode
  :bind (("C-s" . counsel-grep-or-swiper)
         ("M-x" . counsel-M-x)
	 ("C-r" . swiper)
         ("C-s" . counsel-grep-or-swiper)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("C-c k" . counsel-rg)
         ("C-c m" . counsel-imenu)
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

(use-package ivy-bibtex
  :defer t
  :init
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil))

(use-package hydra :demand t)

(use-package ivy-hydra :after hydra :demand t)

;; ==================================================
;; Flyspell
;; ==================================================
(use-package flyspell
  :commands (flyspell-prog-mode flyspell-mode)
  :diminish flyspell-mode
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (use-package ispell
    :config
    (setq ispell-personal-dictionary "~/.aspell.en.pws"))
  :config
  (unbind-key "C-;" flyspell-mode-map))

(use-package flyspell-correct
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic)
  :after flyspell)

(use-package flyspell-correct-ivy
  :demand t
  :bind (:map flyspell-mode-map
              ("<f8>" . flyspell-correct-next-word-generic)))

;; ==================================================
;; Compile
;; ==================================================
(use-package compile
  :config
  (add-to-list 'compilation-error-regexp-alist 'node)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 2 3 4)))

;; ==================================================
;; Ledger
;; ==================================================
(use-package ledger-mode)
(use-package flycheck-ledger
  :after flycheck)

;; ==================================================
;; Python
;; ==================================================
;; (use-package elpy
;;   :defer t
;;   :bind (("M-." . elpy-goto-definition))
;;   :config
;;   (elpy-enable))

;; (use-package pyenv-mode :defer t)

;; ==================================================
;; Latex
;; ==================================================
(use-package auctex
  :defer t
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
  :defer t
  :config
  (company-auctex-init))

;; ==================================================
;; Rust
;; ==================================================
(use-package cargo
  :defer t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package rust-mode
  :defer t
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c <tab>") #'rust-format-buffer))))

(use-package racer
  :defer t
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package company-racer :defer t)

(use-package flycheck-rust
  :defer t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

(use-package markdown-mode
  :commands (markdonw-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package ess-site
  :ensure ess
  :defer t
  :mode (("/R/.*\\.q\\'" . R-mode)
         ("\\.[rR]\\'" . R-mode))
  :config
  (setq ess-first-continued-statement-offset 2
        ess-continued-statement-offset 0
        ess-expression-offset 2
        ess-nuke-trailing-whitespace-p t
        ess-default-style 'DEFAULT))

(use-package ess-R-data-view
  :defer t
  :bind ("C-c v" . ess-R-dv-pprint))

(use-package ibuffer :bind ("C-x C-b" . ibuffer))
(use-package rainbow-delimiters)
(use-package esup :defer t)
(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-c C-d" . helpful-at-point)))
(use-package deadgrep
  :bind (("C-c s" . deadgrep)))
(use-package counsel-gtags)
(use-package cc-mode
  :bind
  (("M-." . counsel-gtags-find-definition)
   ("M-*" . counsel-gtags-find-reference)
   ("M-," . counsel-gtags-go-backward))
  :init
  (setq c-default-style "bsd"
        c-basic-offset 4)
  (counsel-gtags-mode 1))

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
