(cl-defun ld-row-metadata (row)
  (last-elt row))

(cl-defun ld-metadata (metadata-designator)
  (error "Not implemented"))

(cl-defun ld-metadata-p (x)
  (and (consp x) (eql (first x) :metadata)))

(cl-defun ld-get-metadatum (property metadata)
  (getf (rest metadata) property))
;;(ld-select :users :column ::created)

(cl-defun ld-set-metadatum (value property metadata)
  (setf (getf (rest metadata) property) value))

(provide 'ld-metadata)
