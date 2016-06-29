(defun copy-tree (tree)
  "Return a clone of TREE which has no shared list structures with it."
  (if (consp tree)
    (cons (copy-tree (car tree)) (copy-tree (cdr tree)))
    tree))
;;(copy-tree nil)

(provide 'mb-utils-tree)
