(use-package org-download)

(use-package htmlize)

(use-package org-plus-contrib
  ;; :ensure tcalendar, abbrev,
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
  (setq org-agenda-files '("~/git/org/"))
  (setq org-default-notes-file "~/git/org/notes.org")
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
               '("eq" "#+BEGIN_EXPORT latex\n\\begin{equation}\n?\n\\end{equation}\n#+END_EXPORT"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (python . t)
     (shell . t)
     (C . t)))
  (setq org-confirm-babel-evaluate nil))

(use-package deft
  :bind ("<f8>" . deft)
  :commands (deft)
  :config
  (setq deft-directory "~/OneDrive - Jinyang Li/notes"
        deft-extensions '("md" "org" "txt" "tex")
        deft-recursive t
        deft-use-filename-as-title nil))

(use-package org-ref
  :config
  (setq reftex-default-bibliography '("~/OneDrive - Jinyang Li/bibliography/references.bib"))
  (setq org-ref-bibliography-notes "~/OneDrive - Jinyang Li/bibliography/notes.org"
        org-ref-default-bibliography '("~/OneDrive - Jinyang Li/bibliography/references.bib")
        org-ref-pdf-directory "~/OneDrive - Jinyang Li/bibliography/pdfs/"))

(provide 'init-org)
