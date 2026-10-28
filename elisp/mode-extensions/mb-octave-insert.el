;;; mb-octave-insert.el --- Insert Octave code snippets -*- lexical-binding: t; -*-

;;; Code:

(require 'octave)
(require 'subr-x)

(defconst mb-octave--snippet-file
  (expand-file-name "mb-octave-snippets.txt"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "File containing the Octave documentation template.")

(defun mb-octave-insert-map ()
  "Return the keymap for Octave insertion commands."
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'mb-octave-insert-defun)
    (define-key map "D" #'mb-octave-insert-doc)
    (define-key map "i" #'mb-octave-insert-if)
    (define-key map "c" #'mb-octave-insert-case)
    (define-key map "f" #'mb-octave-insert-for)
    map))

(defun mb-octave-insert-defun ()
  "Insert an Octave function skeleton."
  (interactive)
  (octave-insert-defun))

(defun mb-octave--insert-block (text point-offset)
  "Insert TEXT on a new line and leave point at POINT-OFFSET.
POINT-OFFSET is measured from the beginning of TEXT."
  (newline)
  (let ((start (point)))
    (insert text)
    (let ((target (copy-marker (+ start point-offset))))
      (unwind-protect
          (progn
            (indent-region start (point))
            (goto-char target))
        (set-marker target nil)))))

(defun mb-octave-insert-if ()
  "Insert an Octave if/else skeleton and leave point in the condition."
  (interactive)
  (mb-octave--insert-block "if ()\nelse\nendif\n\n" 4))

(defun mb-octave-insert-for ()
  "Insert an Octave for-loop skeleton and leave point after `for '."
  (interactive)
  (mb-octave--insert-block "for \nendfor\n\n" 4))

(defun mb-octave-insert-case ()
  "Report that case insertion has not been implemented."
  (interactive)
  (user-error "Octave case insertion is not implemented"))

(defun mb-octave--template-name ()
  "Return the Octave function name implied by the visited filename."
  (unless buffer-file-name
    (user-error "Buffer is not visiting an Octave function file"))
  (file-name-base buffer-file-name))

(defun mb-octave-insert-doc ()
  "Insert the expanded Octave documentation template at buffer start."
  (interactive)
  (let* ((name (mb-octave--template-name))
         (template
          (with-temp-buffer
            (insert-file-contents mb-octave--snippet-file)
            (buffer-string)))
         (expanded
          (string-replace
           "%thisisodate%" (format-time-string "%Y-%m-%d")
           (string-replace
            "%thisname%" name
            (string-replace
             "%thisyear%" (format-time-string "%Y") template)))))
    (goto-char (point-min))
    (insert expanded)))

(provide 'mb-octave-insert)

;;; mb-octave-insert.el ends here
