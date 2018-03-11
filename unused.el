(use-package docker
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package tuareg
  :ensure t
  :defer t
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode)))

(use-package merlin
  :ensure t
  :defer t
  :init
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (add-hook 'merlin-mode-hook #'company-mode)
  (setq merlin-error-after-save nil))

(use-package ocp-indent
  :ensure t
  :defer t
  :init
  (add-hook 'tuareg-mode-hook 'ocp-indent-caml-mode-setup))

(use-package utop
  :ensure t
  :defer t
  :init
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  :config
  (if (executable-find "opam")
      (setq utop-command "opam config exec -- utop -emacs")
    (message "Cannot find opam executable.")))

(use-package flycheck-ocaml
  :ensure t
  :defer t
  :init
  (flycheck-ocaml-setup))

(use-package company-cabal
  :ensure t
  :defer t
  :init
  (add-to-list 'company-backends 'company-cabal))

(use-package company-ghc
  :ensure t
  :defer t)

(use-package intero
  :ensure t
  :defer t)

(use-package flycheck-haskell
  :ensure t
  :defer t
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-haskell-configure))

(use-package haskell-mode
  :ensure t
  :defer t
  :init
  (setq haskell-interactive-popup-errors nil
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-stylish-on-save nil)
  :config
  (with-eval-after-load 'align
    (add-to-list 'align-rules-list
                 '(haskell-types
                   (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                   (modes . haskell-modes)))
    (add-to-list 'align-rules-list
                 '(haskell-assignment
                   (regexp . "\\(\\s-+\\)=\\s-+")
                   (modes . haskell-modes)))
    (add-to-list 'align-rules-list
                 '(haskell-arrows
                   (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                   (modes . haskell-modes)))
    (add-to-list 'align-rules-list
                 '(haskell-left-arrows
                   (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                   (modes . haskell-modes)))))

(use-package hindent
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package hlint-refactor
  :ensure t
  :defer t)

(use-package org-download
  :ensure t)

(use-package org-plus-contrib
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . counsel-org-capture))
  :init
  ;; (setq org-latex-pdf-process (list "latexmk -f -pdf %f"))
  (setq org-clock-persist t)
  (setq org-highlight-latex-and-related '(latex script entities))
  (setq org-pretty-entities nil)
  (setq org-startup-with-latex-preview t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.0))
  ;; basic settings
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "red" :weight bold))
          ("NEXT" . (:foreground "blue" :weight bold))
          ("DONE" . (:foreground "forest green" :weight bold))
          ("WAITING" . (:foreground "orange" :weight bold))
          ("HOLD" . (:foreground "magenta" :weight bold))
          ("CANCELLED" . (:foreground "forest green" :weight bold))
          ("MEETING" . (:foreground "forest green" :weight bold))
          ("PHONE" . (:foreground "forest green" : :weight bold))))
  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("HOLD" ("WAITING") ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
  (setq org-startup-indented t)
  (setq org-startup-folded nil)
  (setq org-return-follows-link t)
  (setq org-src-preserve-indentation t)
  (setq org-src-fontify-natively t)
  (setq org-agenda-files '("~/Onedrive - Jinyang Li/org/"))
  (setq org-default-notes-file "~/Onedrive - Jinyang Li/org/notes.org")
  (setq org-capture-templates
        '(("t" "todo" entry (file "")
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("r" "respond" entry (file "")
           "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
          ("n" "note" entry (file "")
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("j" "journal" entry (file+datetree "~/journal.org")
           "* %?\n%U\n" :clock-in t :clock-resume t)
          ("m" "meeting" entry (file "")
           "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
          ("p" "Phone call" entry (file "")
           "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)))
  (add-to-list 'org-structure-template-alist
               '("eq" "#+BEGIN_EXPORT latex\n\\begin{equation}\n?\n\\end{equation}\n#+END_EXPORT")))
