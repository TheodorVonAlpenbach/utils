(in-package :topology)

(defmethod generalize ((x path) y)
  "Generalize path X to an N points long path."
  (generalize (points x) y))

(defmethod generalize ((x sequence) (n integer))
  "Generalize point sequence X to an N points long sequence."
  (apply #'melt x (generalize-order x 0 (1- (length x)) n)))
;;(generalize #(1 2 3 4 5) .5)

(defmethod generalize ((x sequence) (factor real))
  "Generalize point sequence X to an N points long sequence."
  (generalize x (round (* factor (length x)))))
;;(generalize #(1 2 3 4 5) .5)

(defun generalize-tree (points a b)
  (when (< 1 (- b a))
    (let ((s (make-segment (elt points a) (elt points b))))
      (multiple-value-bind (x i v)
	  (maximum points :key (bind #'distance2 s) :start (1+ a) :end b)
	(declare (ignore x v))
	(list i
	      (generalize-tree points a i)
	      (generalize-tree points i b))))))
;;(untrace generalize-tree)

(defun generalize-order (x a b n)
  (let ((order (subseq (bfs-order (generalize-tree x a b)) 0 (- n 2))))
    (print order)
    (nflank a (sort order #'<) b)))

(defun bfs-order (tree)
  "Return a list of TREE's elements in BFS order.
See https://en.wikipedia.org/wiki/Breadth-first_search for a
definition of BFS."
  (let ((q (fifo::make (list tree))))
    (loop while (fifo::not-empty-p q)
	  for (value . subtrees) = (fifo::pop q)
	  collect value
	  do (fifo::mpush (remove nil subtrees) q))))
;;(bfs-order '(3 (1 (2 (7))) (5 (4) (6))))
;;(bfs-order '(3 (1 nil (2 nil (7 nil nil))) (5 (4 nil nil) (6 nil nil))))
