;;;; This module depends on the Graphviz package, which, in particular,
;;;; contains the dot utility used here. You can install the package like this
;;;; $ apt-get install graphviz

;;; Customization
(defgroup Graphviz nil
  "Customizations for Elisp code based on Graphviz utilities."
  :tag "Graphviz"
  :link '(url-link :tag "Home Page" "http://www.graphviz.org/")
  :group 'mb-elisp)

(defgroup dot nil
  "Customizations for Elisp code based on the dot utility from the Graphviz package."
  :tag "dot"
  :group 'Graphviz)

(defcustom dot-program "dot"
       "Command to run the dot utility from the Graphviz package."
       :type 'string
       :group 'dot)


;;; Generation
(defun dot-node-base (identifier properties)
  "Creates a dot node definition string. PROPERTIES is an alist
where each element defines a dot node property \(name . value)"
  (format "%s [%s];" identifier
	  (concat* properties :in "," :key #'(lambda (p) 
					       (format "%s=\"%s\"" (car p) (cdr p))))))
;;(dot-node-base 'n '((label . "nodeName") (fillcolor . yellow) (style . filled)))

(defconst *dot-dir* (concat *local-data-dir* "dot/"))
(cl-defun dot-node (identifier &optional name (color "white") (style "filled"))
  (dot-node-base identifier (list (cons 'label name)
				  (cons 'fillcolor color)
				  (cons 'style style))))
;;(dot-node 'n "nodeName" 'yellow)

(defun dot-relation (parent-identifier child-identifier)
  (format "%s -> %s;" parent-identifier child-identifier))
;;(dot-relation "A" "B")

(cl-defun dot-statements-from-tree (tree &optional (node-address ()))
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


;;; Printing
(defun dot-tmp-path (dot-string)
  (concat temporary-file-directory (md5 dot-string)))
;;(dot-tmp-path "qwe")

(cl-defun dot-to-png (dot-string &key (path (dot-tmp-path dot-string)))
  "Returns path to generated PNG file.
The function uses `dot-program' to convert the DOT-STRING to a PNG image."
  (string-to-file dot-string path)
  (let ((res (call-process dot-program nil "*qwe*" nil path "-Tpng" "-O")))
    (if (zerop res)
      (concat path ".png")
      (error "Couldn't compile .dot file %s. See *qwe* for reason." path))))
;;(dot-to-png (dot-string (dot-statements-from-tree '((c f) g c))))

(defun dot-view-file (path)
  (png-view (dot-to-png (file-string path))))

(cl-defun dot-view (dot-string &key (path (dot-tmp-path dot-string)))
  (png-view (dot-to-png dot-string :path path)))
;;(dot-view (dot-string (dot-statements-from-tree '(((c f) g c)))))
;;(dot-view (file-string "~/projects/dot/CHESS-process.gv"))

(defun png-view (filename)
  (auto-image-file-mode 1)
  (find-file filename))

;;;; this could be moved to lilypond-<something>
(defun pdf-view (filename)
  (browse-url filename))

(provide 'dot)
