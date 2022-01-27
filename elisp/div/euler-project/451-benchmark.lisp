(require '451-modular-inverses "~/git/utils/elisp/div/euler-project/451-modular-inverses.lisp")

(defun modular-inverse-p (i n) (= (mod (* i i) n) 1))
;;(modular-inverse-p 11 15)

(defun min-modular-inverse-brute (n)
  (or (loop for i from 2 below (/ n 2)
	    if (modular-inverse-p i n) return i)
      1))

(defun min-modular-inverses-brute (n)
  (if (< n 2)
    (make-list n :initial-element 1)
    (append '(1 1 1)
	    (loop for i from 3 to n collect (min-modular-inverse-brute i)))))
;;(min-modular-inverses-brute 8)

(defun max-modular-inverse-brute (n)
  (let ((i (min-modular-inverse-brute n)))
    (if (> i 1) (- n i) i)))
;;(max-modular-inverse-brute 100)

(defun 451-solution-brute (n)
  (loop for i from 3 to n sum (max-modular-inverse-brute i)))
;;(451-solution-brute (round 2E4))

(defun 451-solutions-brute (b &optional (a 3))
  (loop for n from a to b collect (list n (451-solution-brute n))))
;;(451-solutions-brute 1000 1000)

(defun 451-solutions (b &optional (a 3))
  (loop for n from a to b collect (list n (min-modular-inverses n))))
;;(451-solutions 1000 1000)
;;(min-modular-inverses 8)

(defun 451-benchmark (b &optional (a 3))
  (equal (451-solutions b a)
	 (451-solutions-brute b a)))
;;(time (451-benchmark 107013 107013))
;;(time (451-benchmark 107 107))

(provide 451-benchmark)
