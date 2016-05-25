;;;; generation
(defun dot-node-base (identifier properties)
  "Creates a dot node definition string. PROPERTIES is an alist
where each element defines a dot node property \(name . value)"
  (format "%s [%s];" identifier
	  (concat* properties :in "," :key #'(lambda (p) 
					       (format "%s=\"%s\"" (car p) (cdr p))))))
;;(dot-node-base 'n '((label . "nodeName") (fillcolor . yellow) (style . filled)))

(defconst *dot-dir* (concat *local-data-dir* "dot/"))
(defun* dot-node (identifier &optional name (color "white") (style "filled"))
  (dot-node-base identifier (list (cons 'label name)
				  (cons 'fillcolor color)
				  (cons 'style style))))
;;(dot-node 'n "nodeName" 'yellow)

(defun dot-relation (parent-identifier child-identifier)
  (format "%s -> %s;" parent-identifier child-identifier))
;;(dot-relation "A" "B")

(defun* dot-statements-from-tree (tree &optional (node-address ()))
  (let* ((identifier (concat* node-address :in "" :key #'char-to-string))
	 (parent-identifier (concat* (butlast node-address) :in "" :key #'char-to-string))
	 (node (if node-address (dot-node identifier (sstring tree))))
	 (relation (if (butlast node-address) (dot-relation parent-identifier identifier)))
	 (subnodes (if (listp tree)
		     (loop for node in tree
			   for i from ?A
			   collect (dot-statements-from-tree node (append node-address (list i)))))))
    (delete nil (flatten (list node relation subnodes)))))
;;(dot-statements-from-tree '((c f) g c))

(defun dot-string (dot-statements)
  (concat* dot-statements :pre "digraph g {\n" :in "\n" :suf "\n}"))
;;(dot-string (dot-statements-from-tree '((c f) g c)))

;;;; printing
(defun dot-tmp-path (dot-string)
  (concat temporary-file-directory (md5 dot-string)))

(defun* dot-to-png (dot-string &optional (tmp-path (dot-tmp-path dot-string)))
  "Returns path to generated PNG file"
  (string-to-file dot-string tmp-path)
  (let ((res (call-process "dot.exe" nil "*qwe*" nil tmp-path "-Tpng" "-O")))
    (if (zerop res)
      (concat tmp-path ".png")
      (error "Couldn't compile .dot file %s. See *qwe* for reason." tmp-path))))

(defun* dot-view (dot-string &optional (tmp-path (dot-tmp-path dot-string)))
  (png-view (dot-to-png dot-string tmp-path)))

(defun png-view (filename)
  (browse-url filename))

;;;; this could be moved to lilypond-<something>
(defun pdf-view (filename)
  (browse-url filename))

(provide 'dot)
