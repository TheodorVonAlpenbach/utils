;;; mb-sexp.el --- S-expression and defining-form helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Navigation, counting, lifting, and definition discovery helpers.  Modern
;; implementations use an mb-sexp- prefix; compatibility aliases retain the
;; original public names used by existing configuration.

;;; Code:

(require 'cl-lib)

(defvaralias '+defun-symbols+ 'mb-sexp-defining-forms)
(defvaralias '+defun-regexp+ 'mb-sexp-defining-form-regexp)

(defconst mb-sexp-defining-forms
  '(defun cl-defun
    defmacro cl-defmacro
    defparameter defconstant
    ert-deftest)
  "Forms whose names are recognized by `mb-sexp-defun-symbol'.")

(defconst mb-sexp-defining-form-regexp
  (regexp-opt (mapcar #'symbol-name mb-sexp-defining-forms) 'symbols)
  "Regexp matching a form in `mb-sexp-defining-forms'.")

(defun mb-sexp-goto-first ()
  "Move to the beginning of the first sexp at point's current level.
Return point."
  (interactive)
  (let (moved)
    (while
        (progn
          (setq moved (point))
          (and (condition-case nil
                   (progn (forward-sexp -1) t)
                 (scan-error nil))
               (< (point) moved)))))
  (point))

(defalias 'goto-first-sexp #'mb-sexp-goto-first)

(defun mb-sexp-count-current-level ()
  "Return the number of sexps at point's current level."
  (interactive)
  (let ((count
         (save-excursion
           (mb-sexp-goto-first)
           (let ((n 0))
             (while (condition-case nil
                        (progn (forward-sexp 1) t)
                      (scan-error nil))
               (cl-incf n))
             n))))
    (when (called-interactively-p 'interactive)
      (message "%d" count))
    count))

(defalias 'count-sexps-current-level #'mb-sexp-count-current-level)

(defun mb-sexp-count-region (start end)
  "Return the number of complete sexps between START and END.
Stop counting at the first malformed or incomplete sexp."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let ((count 0)
            next)
        (while
            (progn
              (skip-syntax-forward " >")
              (and (< (point) (point-max))
                   (setq next
                         (condition-case nil
                             (scan-sexps (point) 1)
                           (scan-error nil)))))
          (goto-char next)
          (cl-incf count))
        count))))

(defalias 'count-sexps-region #'mb-sexp-count-region)

(defun count-sexps-region* (start end)
  "Display and return the number of sexps between START and END."
  (interactive "r")
  (let ((count (mb-sexp-count-region start end)))
    (message "%d" count)
    count))

(defun mb-sexp--splice-parent ()
  "Remove the delimiters around the list containing point."
  (save-excursion
    (backward-up-list 1)
    (let ((opening (point))
          (closing (scan-sexps (point) 1)))
      (goto-char (1- closing))
      (delete-char 1)
      (goto-char opening)
      (delete-char 1))))

(defun mb-sexp-lift (&optional count)
  "Raise COUNT sexps one level, defaulting to one.
Positive COUNT delegates to the standard `raise-sexp'.  A zero COUNT
retains the original command's special behavior of removing only the
surrounding delimiters."
  (interactive "p")
  (setq count (or count 1))
  (cond
   ((< count 0)
    (user-error "Count must be non-negative"))
   ((zerop count)
    (mb-sexp--splice-parent))
   (t
    (raise-sexp count))))

(defalias 'lift-sexp #'mb-sexp-lift)

(defun mb-sexp--definition-at (position defining-forms)
  "Return the defining form and name enclosing POSITION.
DEFINING-FORMS is the list of accepted form symbols.  Return nil when
POSITION is not inside one of those forms."
  (save-excursion
    (goto-char position)
    (when (looking-at-p "\\s(")
      (forward-char 1))
    (condition-case nil
        (progn
          (beginning-of-defun)
          (let* ((start (point))
                 (end (scan-sexps start 1)))
            (when (and end (<= start position) (<= position end))
              (forward-char 1)
              (skip-syntax-forward " ")
              (let ((form (read (current-buffer))))
                (when (memq form defining-forms)
                  (skip-syntax-forward " ")
                  (list form (read (current-buffer))))))))
      (error nil))))

(defun mb-sexp-defun-symbol
    (&optional position defining-forms)
  "Return the name of the defining form enclosing POSITION.
POSITION defaults to point and DEFINING-FORMS defaults to
`mb-sexp-defining-forms'."
  (cadr (mb-sexp--definition-at
         (or position (point))
         (or defining-forms mb-sexp-defining-forms))))

(defalias 'defun-symbol #'mb-sexp-defun-symbol)

(defun mb-sexp-in-defun-p
    (&optional position defining-form-regexp)
  "Return non-nil when POSITION is inside a recognized defining form.
DEFINING-FORM-REGEXP can restrict the recognized form names."
  (let* ((definition
          (mb-sexp--definition-at
           (or position (point)) mb-sexp-defining-forms))
         (form (car definition)))
    (and form
         (or (null defining-form-regexp)
             (string-match-p defining-form-regexp
                             (symbol-name form))))))

(defalias 'in-defun-p #'mb-sexp-in-defun-p)

(defun mb-sexp-defun-symbols (&optional buffer)
  "Return the names of recognized top-level definitions in BUFFER.
BUFFER defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let (symbols)
        (while (re-search-forward
                (concat "^\\s-*(" mb-sexp-defining-form-regexp)
                nil t)
          (let ((position (match-beginning 0)))
            (unless (nth 8 (save-excursion (syntax-ppss position)))
              (let ((symbol (mb-sexp-defun-symbol position)))
                (when symbol
                  (push symbol symbols))))))
        (nreverse symbols)))))

(defalias 'defun-symbols #'mb-sexp-defun-symbols)

(defun defun-symbol* ()
  "Display and return the recognized definition name at point."
  (interactive)
  (let ((symbol (mb-sexp-defun-symbol)))
    (message "%S" symbol)
    symbol))

(defun defun-symbols* (&optional buffer)
  "Display and return recognized definition names in BUFFER."
  (interactive)
  (let ((symbols (mb-sexp-defun-symbols buffer)))
    (message "%S" symbols)
    symbols))

(defun insert-defun-symbols (&optional buffer)
  "Insert the printed definition-name list for BUFFER at point."
  (interactive)
  (insert (format "%S" (mb-sexp-defun-symbols buffer))))

(provide 'mb-sexp)

;;; mb-sexp.el ends here
