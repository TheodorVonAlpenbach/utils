;;;; Insert snippets for Octave code
(defun mb-octave-insert-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'mb-octave-insert-defun)
    (define-key map "i" #'mb-octave-insert-if)
    (define-key map "c" #'mb-octave-insert-case)
    (define-key map "f" #'mb-octave-insert-for)
    map))

(defun mb-octave-insert-defun ()
  (message "Not implemented!"))

(defun mb-octave-insert-if ()
  (interactive)
  (newline)
  (let ((point (point)))
    (insert "if ()\nelse\nendif\n\n")
    (backward-char 13)
    (evil-indent point (+ point 17))))

(defun mb-octave-insert-for ()
  (interactive)
  (newline)
  (let ((point (point)))
    (insert "for \nendfor\n\n")
    (evil-indent point (+ point 8))i
    (forward-char 4)))

(defun mb-octave-insert-case ()
  (message "Not implemented!"))

(provide 'mb-octave-insert)