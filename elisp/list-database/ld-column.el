;;; ld-column.el --- List database column definitions -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ld-identifier)

(declare-function ld-schema-column-definitions "ld-schema")

(defun ld-make-column (identifier &rest properties)
  "Create a column definition for IDENTIFIER.
PROPERTIES accepts `:name', `:type', and `:properties'."
  (let ((name (plist-get properties :name))
        (type (plist-get properties :type))
        (column-properties
         (append
          (copy-sequence (plist-get properties :properties))
          (when (plist-get properties :primary-key)
            '(:primary-key))
          (when (plist-get properties :unique)
            '(:unique)))))
    (list :column
          identifier
          (or name
              (substring
               (symbol-name
                (ld-identifier-keyword identifier :column))
               1))
          type
          column-properties)))

(defalias 'ld-column-tag #'car)
(defalias 'ld-column-identifier #'cadr)
(defalias 'ld-column-name #'cl-third)
(defalias 'ld-column-type #'cl-fourth)
(defalias 'ld-column-properties #'cl-fifth)

(defun ld-column-property-p (property column-definition)
  "Return non-nil when COLUMN-DEFINITION has PROPERTY."
  (memq property (ld-column-properties column-definition)))

(defun ld-column-primary-key-p (column-definition)
  "Return non-nil when COLUMN-DEFINITION is a primary key."
  (ld-column-property-p :primary-key column-definition))

(defun ld-column-p (object)
  "Return non-nil when OBJECT is a column definition."
  (and (consp object) (eq (car object) :column)))

(defun ld-column-keyword (column-definition)
  "Return COLUMN-DEFINITION's column keyword."
  (ld-identifier-keyword
   (ld-column-identifier column-definition)
   :column))

(defun ld-primary-key (schema &optional as-column-definition-p)
  "Return SCHEMA's primary key.
Return its column definition when AS-COLUMN-DEFINITION-P is
non-nil; otherwise return its identifier.  Return nil when SCHEMA
has no primary key."
  (let ((column
         (cl-find-if
          #'ld-column-primary-key-p
          (ld-schema-column-definitions schema))))
    (if as-column-definition-p
        column
      (and column (ld-column-identifier column)))))

(defun ld-column-comparator (column-definition)
  "Return the equality predicate for COLUMN-DEFINITION's type."
  (pcase (ld-column-type column-definition)
    ('string #'string=)
    ('listp #'equal)
    (_ #'eql)))

(provide 'ld-column)

;;; ld-column.el ends here
