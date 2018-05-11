(defun jinyang-dired/pdfs->jpgs ()
  "Convert pdf to jpg."
  (interactive)
  (let ((pdfs (dired-get-marked-files)))
    (mapcar `jinyang-dired/pdf->jpg pdfs)))

(defun jinyang-dired/pdf->jpg (pdf)
  "Convert a pdf to a 300 dpi no compression jpg."
  (let ((jpg (concat (file-name-sans-extension pdf) ".jpg")))
    (start-process-shell-command "imagemagick" nil "convert" "-density" "300" pdf "-flatten" "-quality" "100" jpg)))

(provide 'init-funcs)
