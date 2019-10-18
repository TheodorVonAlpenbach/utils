(defun copy-tree (tree)
  "Return a clone of TREE which has no shared list structures with it."
  (if (consp tree)
    (cons (copy-tree (car tree)) (copy-tree (cdr tree)))
    tree))
;;(copy-tree nil)

(cl-defun relations->tree (relations &key (test #'eql) reverse-p)
  "Given list RELATIONS, return a list of family trees.
A RELATION is a pair (X Y), meaning that X is the parent of the
child Y. The resulting FAMILY-TREE is a CONS based tree where
each subnode SN of a node N is a relation in list RELATIONS. If
instead optional parameter REVERSE-P is not nil then Y is
interpreted as the parent of X."
  (let ((nodes (mapcar #'list
		 (cl-remove-duplicates (flatten relations) :test test))))
    ;; the loop builds child node lists and returns non-top nodes
    (let ((x (loop for (p c) in (if reverse-p
				  (mapcar #'reverse relations) relations)
		   for pn = (cl-find p nodes :key #'car :test test)
		   for cn = (cl-find c nodes :key #'car :test test)
		   do (push cn (cdr pn))
		   collect cn)))
      ;; remove non-top nodes from NODES, and we are home
      (cl-set-difference nodes x))))
;;(relations->tree '(("a" "b") ("a" "c")) :test #'string=)
;;(relations->tree '((a b) (a c) (c d) (b e)))
;;(relations->tree '((a b) (a c) (c d) (b e)) :reverse-p t)
;;(relations->tree '((a b) (a c) (c d) (b e) (f g)))

(cl-defun tree->relations (tree)
  (when tree
    (loop for sn in (cdr tree)
	  append (cons (list (car tree) (car sn)) (tree->relations sn)))))
;;(tree->relations (car (relations->tree '((a b) (a c) (c d) (b e)))))

(cl-defun tree-member (x tree &key (test #'eql) key from-end)
  "Return first subtree in TREE having X has its top node"
  (if (funcall test x (if key (funcall key (car tree)) (car tree)))
    tree
    (loop for cn in (if from-end (reverse (cdr tree)) (cdr tree))
	  if (tree-member x cn :test test :key key :from-end from-end)
	  return it)))
;;(tree-member 'b '(a (b (c)) (b (d))) :from-end t :key nil)

(cl-defun prune-tree-to-target (x tree &key (test #'eql) key from-end with-subtree)
  "Remove all nodes in TREE not having X as successor.
If :WITH-SUBTREE is not nil it returns all nodes that have X as
either its successor or predecessor. If :FROM-END is nil it
traverses TREE from left to right otherwise in the other
direction."
  (if (funcall test x (if key (funcall key (car tree)) (car tree)))
    (if with-subtree tree (list (car tree)))
    (loop for sn in (if from-end (reverse (cdr tree)) (cdr tree))
	  if (prune-tree-to-target
	      x sn :test test :key key :with-subtree with-subtree)
	  collect (list (car tree) it))))
;;(prune-tree-to-target 'b '(a (b (c)) (c) (d (b)) (f)) :with-subtree t)

(cl-defun copy-subtree-1 (x trees copy-p filter test key from-end tree-from-end)
  "Helper for `copy-subtree' and `find-subtree'."
  (loop for ft in (if from-end (reverse trees) trees)
	for res = (case filter
		    (:down (tree-member
			    x ft :test test :key key :from-end tree-from-end))
		    (:up (prune-tree-to-target
			  x ft :test test :key key
			  :from-end tree-from-end :with-subtree nil))
		    (:prune (prune-tree-to-target
			     x ft :test test :key key
			     :from-end tree-from-end :with-subtree t))
		    (otherwise
		     (error "Keyword :FILTER must be :UP, :DOWN, or :PRUNE.")))
	if res if copy-p collect res else return res))
;;(copy-subtree-1 'f '((a (b (f (d)))) (e (f (g (h))))) nil :up #'eql nil nil nil)

(cl-defun find-subtree (x trees &key (filter :down) (test #'eql) key
				  from-end tree-from-end)
  "Find the first subtree in TREES with X as top node.
If REVERSE-P is not nil, return the minimal ancestor tree
containing X."
  (copy-subtree-1 x trees nil filter test key from-end tree-from-end))
;;(find-subtree 'f '((a (b (f (d)))) (e (f (g (h))))) :filter :prune)

(cl-defun copy-subtree (x trees &key (filter :down) (test #'eql) key
				  from-end tree-from-end)
  "Find all subtrees in TREES with X as top node.
If REVERSE-P is not nil, return the minimal ancestor tree
containing X."
  (copy-subtree-1 x trees t filter test key from-end tree-from-end))
;;(copy-subtree 'f '((a (b (f (d)))) (e (f (g (h))))) :filter :up)

(defun tree-leaves (tree)
  (when tree
    (aif (cdr tree)
      (loop for x in it append (tree-leaves x))
      (list (car tree)))))
;;(tree-leaves '(a))
;;(tree-leaves '(a))
;;(tree-leaves '(a (c (d (f))) (b (e))))

(provide 'mb-utils-tree)
