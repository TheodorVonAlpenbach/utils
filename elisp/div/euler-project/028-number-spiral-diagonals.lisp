(require 'cl-utils "~/git/utils/elisp/div/euler-project/cl-utils.lisp")

;;;; move to utils
(defun isum1 (n)
  (/ (* n (1+ n)) 2))
;;(isum1 10)

(defun isum2 (n)
  (/ (* n (1+ n) (1+ (* 2 n))) 6))
;;(isum2 10)

(defun isum3 (n)
  (sq (isum1 n)))
;;(isum3 3)

(defun 028-solution (s+1)
  "
7 8 9
6 1 2
5 4 3

We define 1 as the 0th square, the figure as the 1st square on so on.
We denote the corner numbers as ne, nw, sw and se, respectively. By
inspection, we observe that those corners are s(n)= 2n numbers aparat
for the n-th square, which, hence, has a border length of 4 * 2n = 8n.
By construction, ne(n) is 1 + 1*8 + 2*8 + ··· + n*8 = 1 + 8S_1(n).
Furhtermore, nw(n) = nw(n) - s(n), sw(n) = nw(n) - s(n) = ne(n) =
2s(n), se(n) = ne(n) - 3s(n). So the sum of all numbers in the n-th
that are part of the diagonals, is

d(n) = ne(n) + nw(n) + sw(n) + se(n) = 4 + 32S_1(n) - 12n
     = 4 + 16n(n+1) - 12n
     = 16n^2 + 16n - 12n + 4
     = 16n^2 + 4n + 4.

The sum D(n) of d for i = 1..n is 

D(n) = 16S_2(n) + 4S_1(n) + 4S_0(n)

And the solution to the problem is D(n) + 1.
"
  (let ((n (/ (1- s+1) 2)))
    (+ 1 (* 16 (isum2 n)) (* 4 (isum1 n)) (* 4 n))))
;;(time (028-solution 1001))
;; => 669171001
