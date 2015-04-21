;;;; New approach:
;;;; Handle LaTeX terminal in a more smooth way
;;;; Properties should be set with a gp-let macro:

;;(defmacro gp-let (bindings))

;;;; so that e.g. the expression

;; (gp-let ((lmargin 1) (rmargin 2))
;;   expressions)

;;;; expands into
"
set lmargin 1
set rmargin 2
gp-expressions
unset rmargin
unset rmargin
"

;;;; Dispatch property values with generic methods:
(defun property-type (symbol)
  "Maybe it is clearer to do the dispatching directly in the defmethods."
  (case symbol
    ((lmargin rmargin tmargin bmargin) 'margin)
    (t symbol)))

(defmethod write-value ((type (eql 'margin)) value out &key)
  "Not sure about the &key here..."
  (write value :stream out))

(defmethod gp-set ((property-name (eql 'margins)) value out)
  "Default method"
  (format out "~(~a~) ")
  (write-value value out))

(defmethod gp-set ((property-name (eql 'margins)) value out)
  "Specialized on 'margins"
  (loop for k in '(lmargin rmargin tmargin bmargin)
	for v in (dispatch-margins value)
	do (gp-set k v out)))

;;;; Label handling. In gnuplot you need to keep track on each label
;;;; by assigning it a positive integer when it is initially set. The the 
