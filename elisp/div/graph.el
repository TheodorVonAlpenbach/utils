(defun graph-find-node (nodes node-id)
  (cl-find node-id nodes :key #'car :test #'equal))
;;(graph-find-node (make-graphs '(("a" "b") ("a" "c"))) "a")

(defun node-insert-child (parent-node child-node)
  (setf (cdr parent-node) (cons child-node (cdr parent-node))))
;;(let ((pn '(1))) (node-insert-child pn '(2 (3))) pn)

(defun relation-nodes (nodes relation)
  (mapcar (bind #'graph-find-node nodes 1) relation))
;;(relation-nodes (make-graphs all-relations-19) (nth 200 all-relations-19))

(defun graph-insert-relation (nodes relation)
  (apply #'node-insert-child (relation-nodes nodes relation)))
;;(graph-insert-relation (make-graphs all-relations-19) (nth 200 all-relations-19))

(defun make-graphs (relations)
  (let ((nodes (mapcar #'list
		 (cl-remove-duplicates (flatten relations) :test #'equal))))
    (cl-loop for r in relations do (graph-insert-relation nodes r))
    nodes))
;;(make-graphs '((a b) (a c) (d e)))
;;(make-graphs all-relations-19)
;;(nth 200 all-relations-19)

(defun graph-parent-child-p (nodes parent child)
  (cl-find child (cdr (graph-find-node nodes parent)) :key #'car :test #'equal))
;;(graph-parent-child-p (make-graphs '(("a" "b") ("a" "c") ("d" "e"))) "a" "b")

(defun graph-visited-p (graph nodes-visited)
  (cl-find (car graph) nodes-visited :test #'equal))
;;(graph-visited-p '(a) '(a))

(cl-defun graph-prune-non-ancestors-1 (graph node-value nodes-visited)
  (unless (graph-visited-p graph nodes-visited)
    (push (car graph) nodes-visited)
    (when (equal (car graph) node-value)
      (print "node found!")
      (print node-value))
    (aif (cl-loop for subgraph in (cdr graph)
		  if (graph-prune-non-ancestors-1
		      subgraph node-value nodes-visited)
		  collect it)
      (cons (car graph) it)
      (when (equal (car graph) node-value)
	(print "target found!")
	(list (car graph))))))
;;(prune-non-ancestor-relations-pu (append (gateway-relations 4) rel-16968) 16968 4)

(cl-defun graph-prune-non-ancestors (graph node-value &optional nodes-visited)
  (cl-remove-duplicates (graph-prune-non-ancestors-1 graph node-value '())
    :test #'equal))
;;(graph-prune-non-ancestors (first (make-graphs '((a 1) (a 2) (1 3)))) 3)
;;(graph-prune-non-ancestors (first (make-graphs '(("a" "b") ("b" "a") ("b" "c")))) "c")

(cl-defun graph-relations-1 (graph &optional (nodes-visited ()))
  (unless (graph-visited-p graph nodes-visited)
    (push (car graph) nodes-visited)
    (cl-loop for child in (cdr graph)
	     collect (list (car graph) (car child))
	     append (graph-relations-1 child nodes-visited))))

(cl-defun graph-relations (graph &optional (nodes-visited ()))
  (cl-remove-duplicates (graph-relations-1 graph '()) :test #'equal))
;;(graph-relations (first (make-graphs '((a 1) (a 2) (1 3)))))

(provide 'graph)
