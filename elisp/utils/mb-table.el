;;; mb-table.el --- Align tables of Emacs Lisp expressions -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'lisp-mode)

;;; Code:

(defun mb-table--goto-line (line)
  "Move to one-based LINE and return point."
  (unless (and (integerp line) (cl-plusp line))
    (error "Line must be a positive integer: %S" line))
  (goto-char (point-min))
  (forward-line (1- line))
  (point))

(defun mb-table--line-items (line)
  "Return the Emacs Lisp expressions on one-based LINE as strings."
  (save-excursion
    (mb-table--goto-line line)
    (let ((end (line-end-position))
          items)
      (while (progn
               (skip-chars-forward " \t" end)
               (< (point) end))
        (let* ((start (point))
               (finish
                (condition-case nil
                    (scan-sexps start 1)
                  (scan-error nil))))
          (unless (and finish (<= finish end))
            (error "Invalid expression on line %d at column %d"
                   line (current-column)))
          (push (buffer-substring-no-properties start finish) items)
          (goto-char finish)))
      (nreverse items))))

(defun mb-table--column-widths (rows)
  "Return maximum column widths for ROWS.
Signal an error unless every row has the same number of columns."
  (when rows
    (let ((columns (length (cl-first rows))))
      (dolist (row (cl-rest rows))
        (unless (= (length row) columns)
          (error "Table rows have different numbers of columns")))
      (cl-loop for column below columns
               collect
               (cl-loop for row in rows
                        maximize (length (nth column row)))))))

(defun mb-table--offsets (widths)
  "Return zero-based column starts for WIDTHS with two spaces between."
  (cl-loop with offset = 0
           for width in widths
           collect offset
           do (cl-incf offset (+ width 2))))

(defun mb-table--format-row (items offsets)
  "Return ITEMS aligned at OFFSETS."
  (unless (= (length items) (length offsets))
    (error "Number of items does not match offsets"))
  (with-temp-buffer
    (setq indent-tabs-mode nil)
    (cl-loop for item in items
             for offset in offsets
             do (indent-to offset)
             do (insert item))
    (buffer-string)))

(cl-defun table-clean (beg end)
  "Align a table of Emacs Lisp expressions between BEG and END.
Rows are lines and columns are expressions.  Separate columns by at least two
spaces and preserve each expression's source text."
  (interactive "r")
  (let* ((beg-line (line-number-at-pos beg))
         (end-line (line-number-at-pos (max beg (1- end))))
         (rows
          (with-syntax-table emacs-lisp-mode-syntax-table
            (cl-loop for line from beg-line to end-line
                     collect (mb-table--line-items line))))
         (offsets (mb-table--offsets (mb-table--column-widths rows))))
    (save-excursion
      (cl-loop for line from beg-line to end-line
               for row in rows
               do (mb-table--goto-line line)
               do (delete-region (line-beginning-position)
                                 (line-end-position))
               do (insert (mb-table--format-row row offsets))))))

(cl-defun table-clean-paragraph ()
  "Align the table in the paragraph at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'paragraph)))
    (unless bounds
      (user-error "Point is not in a paragraph"))
    (table-clean (car bounds) (cdr bounds))))

(cl-defun goto-begin-sexp (&optional (n 1))
  "Move to the beginning of the Nth following expression and return point."
  (forward-sexp n)
  (forward-sexp -1)
  (point))

(cl-defun table-clean-line (line offsets)
  "Align expressions on one-based LINE at zero-based OFFSETS."
  (let ((items
         (with-syntax-table emacs-lisp-mode-syntax-table
           (mb-table--line-items line))))
    (save-excursion
      (mb-table--goto-line line)
      (delete-region (line-beginning-position) (line-end-position))
      (insert (mb-table--format-row items offsets)))))

(cl-defun point-2-line (&optional (point (point)))
  "Return the one-based line number containing POINT."
  (interactive "d")
  (line-number-at-pos point))

(cl-defun table-collect-sexp-lengths (start-line end-line)
  "Return expression lengths for each line from START-LINE through END-LINE."
  (unless (<= start-line end-line)
    (error "Start line must not follow end line"))
  (with-syntax-table emacs-lisp-mode-syntax-table
    (cl-loop for line from start-line to end-line
             collect (mapcar #'length (mb-table--line-items line)))))

(cl-defun table-item-lengths-line (line)
  "Return expression lengths on one-based LINE."
  (cl-first (table-collect-sexp-lengths line line)))

(cl-defun table-item-lengths-line* ()
  "Display expression lengths on the current line."
  (interactive)
  (message "%S" (table-item-lengths-line (point-2-line))))

(provide 'mb-table)

;;; mb-table.el ends here
