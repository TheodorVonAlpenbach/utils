;;; mb-utils-tree.el --- Tree relation and traversal helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Conversion between parent-child relations and cons-based forests, plus
;; subtree search, pruning, copying, and leaf traversal.

;;; Code:

(require 'cl-lib)

(defun mb-tree--relation-edge (relation reverse-p)
  "Return RELATION as (PARENT CHILD), respecting REVERSE-P."
  (unless (and (proper-list-p relation) (= (length relation) 2))
    (error "Each relation must contain exactly two nodes: %S" relation))
  (cl-destructuring-bind (first second) relation
    (let ((edge (if reverse-p
                    (list second first)
                  (list first second))))
      (when (memq nil edge)
        (error "Relation nodes must be non-nil: %S" relation))
      edge)))

(defun mb-tree--same-edge-p (left right test)
  "Return non-nil when LEFT and RIGHT are equal edges under TEST."
  (and (funcall test (car left) (car right))
       (funcall test (cadr left) (cadr right))))

(defun mb-tree--ancestor-p (candidate node relations test)
  "Return non-nil when CANDIDATE is an ancestor of NODE in RELATIONS.
Compare node values with TEST."
  (let ((parent-relation (cl-find node relations :key #'cadr :test test)))
    (and parent-relation
         (or (funcall test candidate (car parent-relation))
             (mb-tree--ancestor-p
              candidate (car parent-relation) relations test)))))

(defun mb-tree--validate-forest (relations test)
  "Validate that RELATIONS describe a forest under TEST."
  (cl-loop for (parent child) in relations
           for parents =
           (cl-loop for (other-parent other-child) in relations
                    when (funcall test child other-child)
                    collect other-parent)
           unless (cl-loop for other-parent in parents
                           always (funcall test parent other-parent))
           do (error "Node %S has multiple parents" child)
           when (mb-tree--ancestor-p child parent relations test)
           do (error "Relation introduces a cycle at %S" child)))

(defun mb-tree--validated-relations (relations test reverse-p)
  "Return validated, deduplicated RELATIONS using TEST and REVERSE-P."
  (unless (proper-list-p relations)
    (error "RELATIONS must be a proper list: %S" relations))
  (let ((normalized
         (cl-remove-duplicates
          (cl-loop for relation in relations
                   collect (mb-tree--relation-edge relation reverse-p))
           :test (lambda (left right)
                   (mb-tree--same-edge-p left right test))
           :from-end t)))
    (mb-tree--validate-forest normalized test)
    normalized))

(cl-defun relations->tree (relations &key (test #'eql) reverse-p)
  "Convert parent-child RELATIONS into a list of cons-based trees.
Each relation must be a two-element list (PARENT CHILD).  Duplicate
relations are ignored.  Signal an error for nil nodes, cycles, or children
with multiple distinct parents.  Compare nodes with TEST.  When REVERSE-P
is non-nil, interpret each relation as (CHILD PARENT)."
  (let* ((relations
          (mb-tree--validated-relations relations test reverse-p))
         (nodes
          (mapcar #'list
            (cl-remove-duplicates
                (apply #'append relations) :test test :from-end t))))
    (let ((non-roots
           (cl-loop for (parent child) in relations
                 for parent-node =
                 (cl-find parent nodes :key #'car :test test)
                 for child-node =
                 (cl-find child nodes :key #'car :test test)
                 do (push child-node (cdr parent-node))
                 collect child-node)))
      (cl-set-difference nodes non-roots))))
;;(relations->tree '(("a" "b") ("a" "c")) :test #'string=)
;;(relations->tree '((a b) (a c) (c d) (b e)))
;;(relations->tree '((a b) (b c)) :reverse-p t)
;;(relations->tree '((a b) (a c) (c d) (b e) (f g)))

(defun tree->relations (tree)
  "Return the parent-child relations represented by TREE."
  (when tree
    (cl-loop for sn in (cdr tree)
	  append (cons (list (car tree) (car sn)) (tree->relations sn)))))
;;(tree->relations (car (relations->tree '((a b) (a c) (c d) (b e)))))

(cl-defun tree-member (x tree &key (test #'eql) key from-end)
  "Return the first subtree in TREE whose top node matches X.
Compare with TEST after applying KEY when non-nil.  Search children from
right to left when FROM-END is non-nil."
  (if (funcall test x (if key (funcall key (car tree)) (car tree)))
    tree
    (cl-loop for cn in (if from-end (reverse (cdr tree)) (cdr tree))
	  if (tree-member x cn :test test :key key :from-end from-end)
	  return it)))
;;(tree-member 'b '(a (b (c)) (b (d))) :from-end t :key nil)

(cl-defun prune-tree-to-target (x tree &key (test #'eql) key from-end with-subtree)
  "Remove all nodes in TREE not having X as successor.
If :WITH-SUBTREE is not nil it returns all nodes that have X as
either its successor or predecessor.  If :FROM-END is nil it
traverses TREE from left to right otherwise in the other
direction."
  (if (funcall test x (if key (funcall key (car tree)) (car tree)))
    (if with-subtree tree (list (car tree)))
    (cl-loop for sn in (if from-end (reverse (cdr tree)) (cdr tree))
	  if (prune-tree-to-target
	      x sn :test test :key key :with-subtree with-subtree)
	  collect (list (car tree) it))))
;;(prune-tree-to-target 'b '(a (b (c)) (c) (d (b)) (f)) :with-subtree t)

(cl-defun mb-tree--copy-subtree
    (x trees copy-p filter test key from-end tree-from-end)
  "Find X in TREES for `copy-subtree' or `find-subtree'.
COPY-P selects all matches rather than the first.  FILTER, TEST, KEY,
FROM-END, and TREE-FROM-END implement their public keyword arguments."
  (cl-loop for ft in (if from-end (reverse trees) trees)
	for res = (cl-case filter
		    (:down (tree-member
			    x ft :test test :key key :from-end tree-from-end))
		    (:up (prune-tree-to-target
			  x ft :test test :key key
			  :from-end tree-from-end :with-subtree nil))
		    (:prune (prune-tree-to-target
			     x ft :test test :key key
			     :from-end tree-from-end :with-subtree t))
		    (otherwise
		     (error "Keyword :FILTER must be :UP, :DOWN, or :PRUNE")))
	if res if copy-p collect res else return res))

(cl-defun find-subtree (x trees &key (filter :down) (test #'eql) key
				  from-end tree-from-end)
  "Find the first subtree in TREES with X as top node.
FILTER controls whether the result contains descendants, ancestors,
or both."
  (mb-tree--copy-subtree
   x trees nil filter test key from-end tree-from-end))
;;(find-subtree 'f '((a (b (f (d)))) (e (f (g (h))))) :filter :prune)

(cl-defun copy-subtree (x trees &key (filter :down) (test #'eql) key
				  from-end tree-from-end)
  "Find all subtrees in TREES with X as top node.
FILTER controls whether results contain descendants, ancestors,
or both."
  (mb-tree--copy-subtree
   x trees t filter test key from-end tree-from-end))
;;(copy-subtree 'f '((a (b (f (d)))) (e (f (g (h))))) :filter :up)

(defun tree-leaves (tree)
  "Return values of every leaf in TREE from left to right."
  (when tree
    (let ((children (cdr tree)))
      (if children
          (cl-loop for child in children append (tree-leaves child))
        (list (car tree))))))
;;(tree-leaves '(a))
;;(tree-leaves '(a))
;;(tree-leaves '(a (c (d (f))) (b (e))))

(provide 'mb-utils-tree)

;;; mb-utils-tree.el ends here
