;;;; This package provide an implementation of the Main-Lorentz
;;;; algorithm for finding all the repetitions in a sequence in
;;;; O(n*log(n)) time. The original article can be found here: http://ac.els-cdn.com/019667748490021X/1-s2.0-019667748490021X-main.pdf?_tid=38549f4a-b35a-11e3-979e-00000aacb35d&acdnat=1395668762_ccb5f48c480ab56f55b3430585339110

;;;; This implementaion generalizes the original algorithm in a couple
;;;; of ways, and also employs a zero based indexing for both input
;;;; sequences and results.

;;;; The main function is repetitions, see bottom of file. It takes
;;;; the standard keyword arguments for sequences, key and test, in
;;;; addition to min-length. The min-length ensures that only
;;;; repetitions of minimum min-length is returned.

(defun* lptext (pattern text &key (lppattern nil) (test #'eql) (key #'identity))
  "Returns LPTEXT, a vector [LPTEXT-0 LPTEXT-1 ... LPTEXT-N]
where N is the length of TEXT. LPTEXT-I is the length of the
longest prefix of the subsequence of TEXT starting at position I
which is also a prefix of PATTERN. See also `lppattern'."
  ;; first find LPPATTERN-1
  (let* ((m (length pattern))
	 (n (length text))
	 (lptext (make-vector (1+ n) 0))
	 (k 0)
	 (lp (or lppattern (lppattern pattern :test test :key key))))
    (loop for j below (min m n)
	  while (funcall test (funcall key (aref pattern j)) (funcall key (aref text j)))
	  finally (setf (aref lptext k) j))
    (loop for i from 1 below n
	  for i-k = (- i k)
	  for length = (- (aref lptext k) i-k)
	  do (if (and (< i-k m) 
		      (< (aref lp i-k) length))
	       (setf (aref lptext i) (aref lp i-k))
	       (loop for j from (if (>= i (+ k (aref lptext k))) 0 length) below (min m (- n i))
		     while (funcall test (funcall key (aref pattern j)) (funcall key (aref text (+ j i))))
		     finally (setf (aref lptext i) j)
		             (setf k i))))
    lptext))
;;(lptext "ab" "cabab")
;;(lptext "cabab" "cababc")
;;(lppattern "cabab")

(defun* lppattern (pattern &key (test #'eql) (key #'identity))
  "Returns the equivalent of (LPTEXT pattern pattern). See
`lptext' for further details. See also comment in the foreword."
  (let* ((m (length pattern))
	 (lppattern (make-vector (1+ m) 0))
	 (k 1))
    (setf (aref lppattern 0) m) ;; first entry must necessarily be the length of the pattern itself
    (setf (aref lppattern k) (1- (aif (position (aref pattern 0) pattern :test (complement test)) it m)))
    (loop for i from 2 below m
	  for i-k = (- i k)
	  for length = (- (aref lppattern k) i-k)
	  do (if (< (aref lppattern i-k) length)
	       (setf (aref lppattern i) (aref lppattern i-k))
	       (loop for j from (if (>= i (+ k (aref lppattern k))) 0 length) below (- m i)
		     while (funcall test (funcall key (aref pattern j)) (funcall key (aref pattern (+ j i))))
		     finally (setf (aref lppattern i) j)
		             (setf k i))))
    lppattern))
;;(lppattern (buffer-string-no-properties))

;;; Two mirror functions of the former
(defun* lspattern (pattern &key (test #'eql) (key #'identity))
  "Returns LSPATTERN, a vector [LSPATTERN-0 LSPATTERN-1 ...
LSPATTERN-M], where M is the length of PATTERN. LSPATTERN-I is
the length of the longest subsequence of PATTERN which ends at
position I and is a suffix of PATTERN. Note that the first entry
in LSPATTERN corresponds to the subsequence ending at index zero.
Hence, this subsequence is the empty sequence and so this value
is always zero. Also, note that the last entry corresponds to the
prefix ending at index M. Thus, this prefix is the pattern
itself, so the last entry is always M."
  (cl-nreverse (lppattern (cl-reverse pattern) :test test :key key)))
;;(lspattern "abcabc")

(defun* lstext (pattern text &key (lspattern nil) (test #'eql) (key #'identity))
  "Returns LSTEXT, a vector [LSTEXT-0 LSTEXT-1 ... LSTEXT-N],
where LSTEXT-I is the length of the longest subsequence of TEXT
which ends at position I and is a suffix of PATTERN. See also
`lspattern' and `lptext'."
  (cl-nreverse (lptext (cl-reverse pattern) (cl-reverse text) 
		       :lppattern (and lspattern (cl-reverse lspattern))
		       :test test
		       :key key)))
;;(lstext "abq" "cababq")

(defun right-repetitions (u v v-offset min-length test key &optional with-center-repetitions-p)
  (let ((ls-u-in-v (lstext u v :test test :key key))
	(lp-v-in-v (lppattern v :test test :key key))
	(max-min-center (if with-center-repetitions-p 0 1)))
    (loop for l from min-length to (length v)
	  for min-center = (max max-min-center (- l (aref ls-u-in-v l)))
	  for max-center = (min (1- l) (aref lp-v-in-v l))
	  if (<= min-center max-center) collect (list l (list (+ v-offset min-center) (+ v-offset max-center))))))
;;(right-repetitions "aaa" "aaaa" 0)
;;(right-repetitions "a" "a" 0 t)

(defun offset-center-postitions (repetitions offset-function)
  "OFFSET-FUNCTION takes the old position as the only argument"
  (loop for repetition in repetitions
	collect (list (first repetition) (mapcar offset-function (second repetition)))))

(defun left-repetitions (u v u-offset min-length test key &optional with-center-repetitions-p)
  "The positions returned by right-repetitions must be reversed.
With offset 0, each position should be inverted on the length of
U. Finally the U-OFFSET must be added."
  (let ((m (length u)))
    (offset-center-postitions (right-repetitions (cl-reverse v) (cl-reverse u) 0 min-length test key with-center-repetitions-p)
			      #'(lambda (x) (- m x)))))
;;(left-repetitions "aa" "aa" 0 1 #'= #'identity t)

(defun repetitions-1 (sequence offset min-length test key)
  (let ((n (length sequence))) 
    (unless (< n (* 2 min-length))
      (let* ((mid (/ n 2))
	     (u (subseq sequence 0 mid))
	     (v (subseq sequence mid))
	     (rr (append (repetitions-1 u 0 min-length test key)
			 (repetitions-1 v mid min-length test key)
			 (left-repetitions u v 0 min-length test key t)
			 (right-repetitions u v mid min-length test key nil))))
	(offset-center-postitions rr (bind #'+ offset))))))
;;(repetitions-1 "aaa" 0 1 #'= #'identity)

(defun* repetitions (sequence &key (min-length 1) (test #'eql) (key #'identity))
  (repetitions-1 sequence 0 min-length test key))
;;(length (repetitions (buffer-string-no-properties) :min-length 7))

;(byte-compile-file "/cygdrive/c/Users/eier/Google Drive/site-lisp/mb-lisp/utils/repetitions.el")

(provide 'repetitions)
