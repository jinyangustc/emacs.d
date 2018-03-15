;;; init-completion --- Jinyang's completion config
;;; Commentary:
;;; Code:

;;
;; * Completion
;;

(use-package company
  :ensure t
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

(use-package counsel-projectile
  :ensure t
  :defer t
  :config
  (counsel-projectile-mode))

(use-package flx :ensure t)

(use-package smex :ensure t)

(provide 'init-completion)
;;; init-completion.el ends here
