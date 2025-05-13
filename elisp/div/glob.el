;; 20190308--20190310
;; ->
;; 201903 + 08--10
;; ->
;; 201903 + {08,09,10}

;; 20190308--20190310
;; ->
;; 201903 + 08--10
;; ->
;; 201903 + {08,09,10}
;; DevN_20181116_180000.bin
;; NB! RawCAM2_20181116_131319.bin Check result

;; 20181116_180000
;; 20181116_1[9]*
;; 20181116_[2]*
;; 2018111[7-9]*
;; 201811[2-3]*
;; 20181[2]*
;; 2018[1]*
;; 201[9]*
;; 20[2-9]*
;; etc

;; RawCAM2_20181116_131319
;; RawCAM2_20181116_13*
;; RawCAM2_20181116_1[4-9]*
;; RawCAM2_20181116_2*
;; RawCAM2_20181117*

00000000
20181117
99999999

00000000
20190228
99999999

2018  20181117
2019  2018


;; On hmon
;; scp -v /home/data/RawStr/*20181116_180000* 20181116_1[9]* *20181116_[2]* *2018111[7-9]* *201811[2-3]* *20181[2]* *2018[1]* *201[9]* mbe@ssh.lightstructures.no:/ls/platinum/data1/17_010_Osstrupen/RawStr

(cl-defun glob-up-digit (d &optional (d-range (i-make-interval 0 9)))
  "Return a glob for ls-date digit D, given its valid range D-RANGE."
  (when (i-within-p d d-range)
    (if (= d (i-right d-range))
      (format "%d" d)
      (format "[%d-%d]" d (i-right d-range)))))

(cl-defun glob-ranges (ranges)
  "Return a glob for RANGES."
  (concat* (cl-mapcar #'glob-up-digit (mapcar #'car ranges) ranges)))
;;(glob-ranges '((0 8) (0 9)))

(cl-defun glob-up-digits-1 (digits ranges)
  "Return a glob for DIGITS given RANGES."
  (when (i-interval-p ranges)
    (setf ranges (make-list (length digits) ranges)))
  (when digits
    (if (cdr digits)
      (let ((g1 (glob-up-digit (1+ (car digits)) (car ranges)))
	    (grest (cl-loop for s in (glob-up-digits-1 (cdr digits) (cdr ranges))
			 if s collect (format "%d%s" (car digits) s))))
	(if g1
	  (cons (format "%s%s" g1 (glob-ranges (cdr ranges))) grest)
	  grest))
      (list (glob-up-digit (car digits) (car ranges))))))

(cl-defun glob-up-digits (digits ranges)
  "Return a glob for DIGITS given RANGES."
  (nreverse (glob-up-digits-1 digits ranges)))
;;(glob-up-digits '(2 0 1 8 0 7 0 5) '((0 9) (0 9) (0 9) (0 9) (0 1) (0 9) (0 3) (0 9)))
;;(glob-up-digits '(1 2 3) '(0 9))

(cl-defun glob-up-lsdate-date (lsd dranges)
  (glob-up-digits (mapcar #'string-to-number (split-string lsd "" t)) dranges))

(cl-defun glob-up-lsdate-time (lst tranges)
  (glob-up-digits (mapcar #'string-to-number (split-string lst "" t)) tranges))

(cl-defun glob-up-lsdate (lsdate)
  (let ((r9 (i-make-interval 0 9))
	(r5 (i-make-interval 0 5))
	(r3 (i-make-interval 0 3))
	(r2 (i-make-interval 0 2))
	(r1 (i-make-interval 0 1)))
    (let ((dranges (list r9 r9 r9 r9 r1 r9 r3 r9))
	  (tranges (list r2 r9 r5 r9 r5 r9)))
      (cl-destructuring-bind (lsd lst) (split-string lsdate "_")
	(list (glob-up-lsdate-date lsd dranges)
	      (glob-up-lsdate-time lst tranges))))))
;;(glob-up-lsdate "20181117_000000")

(provide 'glob)
