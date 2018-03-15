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

(require 'init-python)
(require 'init-rust)
(require 'init-latex)
(require 'init-r)
(require 'init-yaml)
(require 'init-markdown)
(require 'init-racket)

(use-package ibuffer :bind ("C-x C-b" . ibuffer))
(use-package rainbow-delimiters :ensure t)
(use-package esup :ensure t :defer t)

(provide 'init)
;;; init.el ends here
