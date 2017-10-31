(require 'sh-script)

(defconst +mb-echo-indent+ 29) ;'ls --help' style

(defun in-echo-paragraph-p ()
  (string-match "^[\t ]*echo[\t ]*\".*\"$" (line-string)))
;;(in-echo-paragraph-p)

(cl-defun move-beginning-of-echo-paragraph (&optional (point (point)))
  "Move to the beginning of the current echo paragraph."
  (re-search-backward "^[\t ]*echo[\t ]*\" \\{2,5\\}[^ ].*\"$")
  (bol))

(cl-defun move-end-of-echo-paragraph (&optional (point (point)))
  "Move to the end of the current echo paragraph."
  (eol)
  (let ((max-pos (save-excursion
		   (move-end-of-echo-section)
		   (point))))
    (if (re-search-forward "^[\t ]*echo[\t ]*\" \\{2,5\\}[^ ].*\"$" max-pos t)
      (bol :offset 1)
      (goto-char max-pos)
      (bol))))
;;(move-end-of-echo-paragraph)

(cl-defun move-end-of-echo-section (&optional (point (point)))
  "Move to the end of the current echo paragraph."
  (while (in-echo-paragraph-p)
    (next-line)))

(cl-defun boep (&optional (point (point)))
  (move-beginning-of-echo-paragraph point)
  (point))
(cl-defun eoep (&optional (point (point)))
  (move-end-of-echo-paragraph point)
  (point))
(cl-defun boep* (&optional (point (point)))
  (save-excursion (boep point)))
(cl-defun eoep* (&optional (point (point)))
  (save-excursion (eoep point)))

(cl-defun echo-paragraph-region (&optional (point (point)))
  (list (boep*) (eoep*)))

(cl-defun echo-paragraph-prefix (&optional (point (point)))
  (when (in-echo-paragraph-p)
    (string-match* "^[\t ]*echo[\t ]*\""
      (line-string :point  point))))
;;(echo-paragraph-prefix)

(cl-defun sh-indent-echo-paragraph (&optional (point (point)))
  (interactive)
  (let ((prefix (echo-paragraph-prefix point)))
    (save-restriction
      (apply #'narrow-to-region (echo-paragraph-region)))))

(provide 'mb-sh)
