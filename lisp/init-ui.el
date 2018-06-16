;;; init-ui --- Jinyang's UI config
;;; Commentary:
;;; Code:

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
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
;; (menu-bar-mode -1)
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
(use-package default-text-scale
  :ensure t
  :init
  (add-hook 'after-init-hook 'default-text-scale-mode))
;; (when (member "qTriplicate T4c" (font-family-list))
;;   (set-face-attribute 'default nil :font "Triplicate T4c-14"))

(when (display-graphic-p)
  ;; Auto generated by cnfonts
  ;; <https://github.com/tumashu/cnfonts>
  (set-face-attribute
   'default nil
   :font (font-spec :name "-*-Triplicate T4c-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
                    :weight 'normal
                    :slant 'normal
                    :size 14.0))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :name "-*-KaiTi-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
                :weight 'normal
                :slant 'normal
                :size 16.5))))

(setq custom-safe-themes t)

(use-package doom-themes
  :ensure t)

(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

(load-theme 'doom-one-light t)
(doom-themes-visual-bell-config)

;; (use-package gruvbox-theme :ensure t)
;; (load-theme 'gruvbox-dark-medium)

;; (use-package color-theme-sanityinc-tomorrow :ensure t)
;; (load-theme 'sanityinc-tomorrow-day)


(provide 'init-ui)
;;; init-ui.el ends here
