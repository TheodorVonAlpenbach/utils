(require 'euler-utils "~/git/utils/elisp/div/euler-project/euler-utils.lisp")

(defun number-letter-count-1 (n)
  (length (remove #\Space (remove #\- (format nil "~R" n)))))

(defun number-letter-count (n &optional (with-and-p t))
  "Only works up to 100 999, in general. It does not (yet) handle 101
000 (one hundred [and] one thousand), for instance."
  (+ (if (and with-and-p (mod (floor n 100) 10)) 3 0) (number-letter-count-1 n)))
;;(mapcar #'number-letter-count '(115 342))

(defun 017-solution (&optional (n 1000) (start 1) (with-and-p t))
  "Bad problem. It is not clear how 'and' is used for greater numbers.
Here, we assume 'and' is always inserted after 'hundred', except for
100, 200, ..., 900."
  (loop for i from start to n sum (number-letter-count i with-and-p)))
;;(017-solution)
;; => 21451
