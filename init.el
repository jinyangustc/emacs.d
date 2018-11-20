;;; init --- Jinyang's Emacs init file
;;; Commentary:
;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defconst *is-a-mac* (eq system-type 'darwin))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; custome file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; emacs-mac specific
(setq mac-pass-control-to-system nil)

(require 'init-elpa)
(require 'init-ui)
(require 'init-editing)
(require 'init-completion)
(require 'init-git)
(require 'init-dired)
(require 'init-projectile)
(require 'init-ivy)
(require 'init-flyspell)
(require 'init-compile)
(require 'init-ledger)

(require 'init-eshell)
;; (require 'init-org)
(require 'init-python)
(require 'init-clojure)
(require 'init-rust)
(require 'init-latex)
(require 'init-r)
(require 'init-yaml)
(require 'init-markdown)
;; (require 'init-racket)
;; (require 'init-javascript)

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
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
