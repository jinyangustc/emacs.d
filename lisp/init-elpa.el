;;; init-elpa --- Jinyang's elpa config
;;; Commentary:
;;; Code:

(require 'package)

;; Install into separate package dirs for each Emacs version, to
;; prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))

(setq package-archives '(("org" . "http://elpa.emacs-china.org/org/")
			 ("melpa" . "http://elpa.emacs-china.org/melpa/")
			 ("gnu" . "http://elpa.emacs-china.org/gnu/")))

(setq package-enable-at-startup nil)
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

(use-package diminish)
(use-package bind-key)


(provide 'init-elpa)
;;; init-elpa.el ends here
