;;; ld-identifier.el --- List database identifiers -*- lexical-binding: t; -*-

;;; Commentary:

;; Identifiers have the form
;; (DATABASE-KEYWORD &optional TABLE-KEYWORD COLUMN-KEYWORD).
;; Missing leading components are represented by nil.

;;; Code:

(require 'cl-lib)

(declare-function ld-column-identifier "ld-column")
(declare-function ld-schema "ld-global")
(declare-function ld-schema-column-definitions "ld-schema")
(declare-function ld-schema-identifier "ld-schema")

(defun ld-keyword-or-null-p (value)
  "Return non-nil when VALUE is a keyword or nil."
  (or (keywordp value) (null value)))

(defalias 'keywordp-or-null #'ld-keyword-or-null-p)

(defun ld-identifier-p (form &optional type)
  "Return non-nil when FORM is a valid identifier of TYPE.
TYPE may be `:database', `:table', `:schema', or `:column'.
Identifier components may be omitted only as a leading nil
prefix; nil cannot follow the first keyword."
  (and
   (consp form)
   (pcase type
     (:database (= (length form) 1))
     ((or :table :schema) (= (length form) 2))
     (:column (= (length form) 3))
     (_ t))
   (cl-every #'ld-keyword-or-null-p form)
   (let ((keyword-tail (cl-member-if #'keywordp form)))
     (or (null keyword-tail)
         (cl-every #'keywordp keyword-tail)))))

(defun ld-identifier-keyword (identifier &optional type)
  "Return the keyword component of IDENTIFIER selected by TYPE.
Without TYPE, return the last component."
  (pcase type
    (:database (car identifier))
    (:table (cadr identifier))
    (:column (cl-third identifier))
    (_ (car (last identifier)))))

(defun ld-column-identifier-p (column-designator schema-designator)
  "Return the matching column definition, or nil.
COLUMN-DESIGNATOR is interpreted relative to SCHEMA-DESIGNATOR."
  (let* ((schema (ld-schema schema-designator))
         (identifier
          (ld-make-column-identifier column-designator schema)))
    (cl-find identifier
             (ld-schema-column-definitions schema)
             :key #'ld-column-identifier
             :test #'equal)))

(defun ld-make-column-identifier
    (column-designator schema-designator)
  "Return a global column identifier for COLUMN-DESIGNATOR.
Interpret COLUMN-DESIGNATOR relative to SCHEMA-DESIGNATOR.  It
may be a column keyword or a one-, two-, or three-component
identifier.  Return nil when an explicitly named table does not
match the schema."
  (if (ld-identifier-p column-designator :column)
      column-designator
    (let ((table-identifier
           (ld-schema-identifier (ld-schema schema-designator))))
      (cond
       ((keywordp column-designator)
        (append table-identifier (list column-designator)))
       ((ld-identifier-p column-designator :table)
        (when (eq (cadr table-identifier)
                  (car column-designator))
          (cons (car table-identifier) column-designator)))
       ((ld-identifier-p column-designator :database)
        (append table-identifier column-designator))))))

(defun ld-identifier-type-p (identifier)
  "Return the type keyword for IDENTIFIER, or nil."
  (when (ld-identifier-p identifier)
    (pcase (length identifier)
      (1 :database)
      (2 :table)
      (3 :column))))

(provide 'ld-identifier)

;;; ld-identifier.el ends here
