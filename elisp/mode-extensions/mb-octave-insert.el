;;;; Insert snippets for Octave code
(cl-defun mb-octave-insert-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'mb-octave-insert-defun)
    (define-key map "D" #'mb-octave-insert-doc)
    (define-key map "i" #'mb-octave-insert-if)
    (define-key map "c" #'mb-octave-insert-case)
    (define-key map "f" #'mb-octave-insert-for)
    map))

(cl-defun mb-octave-insert-defun ()
  (interactive)
  (octave-insert-defun))

(cl-defun mb-octave-insert-if ()
  (interactive)
  (newline)
  (let ((point (point)))
    (insert "if ()\nelse\nendif\n\n")
    (backward-char 13)
    (evil-indent point (+ point 17))))

(cl-defun mb-octave-insert-for ()
  (interactive)
  (newline)
  (let ((point (point)))
    (insert "for \nendfor\n\n")
    (evil-indent point (+ point 8))i
    (forward-char 4)))

(cl-defun mb-octave-insert-case ()
  (message "Not implemented!"))

(cl-defun mb-octave-insert-doc ()
  "Insert template for the initial documention in an Octave file."
  (interactive)
  (bob)
  (insert (string-replace-map
	      (file-string (expand-file-name
			    "mode-extensions/mb-octave-snippets.txt"
			    +mb-lisp-dir+))
	    `(("%thisyear%" ,(number-to-string (etime-year (now))))
	      ("%thisname%" ,(octave-defun-name))
	      ("%thisisodate%" ,(iso-date))))))

(provide 'mb-octave-insert)
