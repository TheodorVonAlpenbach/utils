;;; ld-general.el --- General list-database object resolution -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ld-global)

(defun ld-warning (format-string &rest arguments)
  "Display and return a list-database warning.
FORMAT-STRING and ARGUMENTS follow `message'."
  (apply #'message
         (concat "Warning: " format-string)
         arguments))

(defun ld-type (object)
  "Return the list-database type of OBJECT.
Signal a `user-error' when OBJECT is not a recognized entity or
identifier."
  (cond
   ((ld-identifier-p object) 'identifier)
   ((ld-database-p object) 'database)
   ((ld-table-p object) 'table)
   ((ld-schema-p object) 'schema)
   ((ld-column-p object) 'column)
   (t (user-error "Not a list-database entity: %S" object))))

(defun ld-identifier (object)
  "Return OBJECT's identifier."
  (pcase (ld-type object)
    ('identifier object)
    ('database (ld-database-identifier object))
    ('table (ld-table-identifier object))
    ('schema (ld-schema-identifier object))
    ('column (ld-column-identifier object))))

(defun ld-keyword (object)
  "Return OBJECT's final identifier keyword.
Return nil when OBJECT is nil."
  (cond
   ((null object) nil)
   ((keywordp object) object)
   (t (ld-identifier-keyword (ld-identifier object)))))

(defun ld-database-id (object)
  "Return OBJECT's database identifier, or nil when unqualified."
  (let ((keyword (car (ld-identifier object))))
    (and keyword (list keyword))))

(defun ld-database (object)
  "Resolve OBJECT's database in `*current-database*'.
OBJECT may be a database entity, another model entity, or an
identifier.  Return nil when its database component does not
identify the current database."
  (if (ld-database-p object)
      object
    (let ((identifier (ld-database-id object)))
      (when (and identifier
                 *current-database*
                 (equal identifier
                        (ld-database-identifier *current-database*)))
        *current-database*))))

(defun ld-parent-id (object)
  "Return OBJECT's parent identifier, or nil at database level."
  (butlast (ld-identifier object)))

(defun ld-parent (object)
  "Resolve and return OBJECT's parent entity."
  (when-let ((identifier (ld-parent-id object)))
    (ld-object identifier)))

(defun ld-object (identifier)
  "Resolve IDENTIFIER in `*current-database*'.
Return nil when IDENTIFIER is valid but no matching object exists."
  (unless (ld-identifier-p identifier)
    (user-error "Not a list-database identifier: %S" identifier))
  (pcase (length identifier)
    (1 (ld-database identifier))
    (2 (ld-table identifier))
    (3
     (when-let ((table (ld-object (butlast identifier))))
       (cl-find identifier
                (ld-table-column-definitions table)
                :key #'ld-column-identifier
                :test #'equal)))))

(provide 'ld-general)

;;; ld-general.el ends here
