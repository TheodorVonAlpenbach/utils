(defun interval:within (interval1 interval2)
  "Returns non-nil iff INTERVAL1 within INTERVAL2."
  (and (>= (car interval1) (car interval2))
       (<= (cdr interval1) (cdr interval2))))
;;(interval:within '(-1 . 7) '(0 . 7))

(defun tree:insert-element (tree x pred)
  (if (not tree)
    (list x)
    (if (funcall pred x (car tree))

      ;; x should be a child of tree
      (or (loop for i below (length (cdr tree))
		for res = (tree:insert-element (nth i (cdr tree)) x pred)
		if res do (setf (nth i (cdr tree)) res)
		          and return tree)
	  
	  ;; x could not be inserted in any of the children, so a new child is created
	  (append tree (list (list x))))

      ;; else tree could be a child of x
      (if (funcall pred (car tree) x)
	(list x tree)
	nil))))

(do ((i 0 (1+ i)))
	((>= i (match:count match)))
      (set! res (cons (match:start-end match i) res)))

(do ((i 0 (1+ i))) for i below (length (cdr tree))
		for res = (tree:insert-element (nth i (cdr tree)) x pred)
		if res do (setf (nth i (cdr tree)) res)
		and return tree)



;;(setq tree '() (tree:insert-element '() '(0 . 7) #'interval:within))
;;(setq tree (tree:insert-element tree '(0 . 2) #'interval:within))
;;(setq tree (tree:insert-element tree 2 #'<))

(defun hatree (list)
  "Destroys list!"
  (when list
    (loop for tree = (hatree))))

;(loop for i below 10 if (= i 3) return 2)
