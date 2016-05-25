(defun ld-row-metadata (row)
  (last-elt row))

(defun ld-metadata (metadata-designator)
  (error "Not implemented"))

(defun ld-metadata-p (x)
  (and (consp x) (eql (first x) :metadata)))

(defun ld-get-metadatum (property metadata)
  (getf (rest metadata) property))
;;(ld-select :users :column ::created)

(defun ld-set-metadatum (value property metadata)
  (setf (getf (rest metadata) property) value))

(provide 'ld-metadata)
