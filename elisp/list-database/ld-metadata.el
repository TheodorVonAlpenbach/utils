;;; ld-metadata.el --- Row metadata for list databases -*- lexical-binding: t; -*-

;;; Code:

(defun ld-row-metadata (row)
  "Return ROW's trailing metadata object."
  (car (last row)))

(defun ld-metadata (_metadata-designator)
  "Resolve a metadata designator.
This operation is not implemented."
  (user-error "Metadata designators are not implemented"))

(defun ld-metadata-p (object)
  "Return non-nil when OBJECT is a metadata object."
  (and (consp object) (eq (car object) :metadata)))

(defun ld-get-metadatum (property metadata)
  "Return PROPERTY from METADATA."
  (plist-get (cdr metadata) property))
;;(ld-select :users :column ::created)

(defun ld-set-metadatum (value property metadata)
  "Set PROPERTY to VALUE in METADATA and return VALUE."
  (setcdr metadata (plist-put (cdr metadata) property value))
  value)

(provide 'ld-metadata)

;;; ld-metadata.el ends here
