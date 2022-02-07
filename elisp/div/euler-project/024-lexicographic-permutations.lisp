(require 'cl-utils "~/git/utils/elisp/div/euler-project/cl-utils.lisp")
(require 'factor-table "~/git/utils/elisp/div/euler-project/factor-table.lisp")

(defun position-first-symbol (p n)
  (if (< n 2)
    0
    (let ((n-1! (! (1- n))))
     (if (< p n-1!)
       0 (1+ (position-first-symbol (mod p n-1!) (1- n)))))))
;;(loop with n = 4 for p below (! n) collect (position-first-symbol p n))

(defun position-kth-symbol (p k n)
  (let ((diff (- (floor p (! (1- n))) k)))
    (case (signum diff) 
      (-1 (1+ (position-kth-symbol (mod p (! (1- n))) (1- k) (1- n))))
      (0 0)
      (t  (1+ (position-kth-symbol (mod p (! (1- n))) k (1- n)))))))
;;(position-kth-symbol 3 1 3)

(defun nth-permutation-positions (p length)
  (loop for k below length collect (position-kth-symbol p k length)))
;;(nth-permutation-positions 3 3)
;;(loop for p below 6 collect (nth-permutation-positions p 3))

(defun project-list (list positions)
  (loop for p in positions collect (nth p list)))
;;(project-list '(a b c) (project-list (nth-permutation-positions 3 3) (nth-permutation-positions 3 3)))

(defun permutation-from-positions (permutation)
  (let* ((n (length permutation))
	 (v (make-array n)))
    (loop for i below n do (setf (svref v (elt permutation i)) i))
    (coerce v 'list)))
;;(permutation-from-positions '(2 0 1))

(defun nth-permutation (p length)
  (permutation-from-positions (nth-permutation-positions p length)))
;;(loop for p below 6 collect (nth-permutation p 3))

(defun permute (list &optional (p 1))
  (project-list list (nth-permutation p (length list))))
;;(loop with l = '(a b c d) for p below (! (length l)) collect (permute l p))

(defun 024-solution (&optional (nth-permutation 1000000)
		       (symbols '(0 1 2 3 4 5 6 7 8 9)))
  "This is really fast"
  (apply #'concatenate 'string
	 (mapcar #'write-to-string (permute symbols nth-permutation))))
;;(time (024-solution))
;; => "2783915604"
