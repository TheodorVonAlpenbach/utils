;;; mb-utils-economy.el --- Small financial calculations -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun to-double (x)
  "Return X converted to floating point."
  (float x))

(defun grew (increase sum)
  "Return the growth ratio represented by INCREASE in final SUM."
  (/ (float increase) (- sum increase)))

(cl-defun CFaR (values &key (from 19) (to 29))
  "Return the difference between the mean of VALUES and a selected mean.
FROM and TO delimit the selected subsequence."
  (let ((selection (cl-subseq values from to)))
    (- (/ (cl-reduce #'+ values) (float (length values)))
       (/ (cl-reduce #'+ selection) (float (length selection))))))

(defun mb-economy--geometric-series (ratio end)
  "Return the sum of powers of RATIO from zero below END."
  (cond
   ((<= end 0) 0)
   ((= ratio 1) end)
   (t (/ (- 1.0 (expt ratio end))
         (- 1 ratio)))))

(cl-defun growth (n &optional (f 1.0) (c 1.0) (d 0.0))
  "Accumulate C for N years with growth factor F and yearly deposit D.
N = 0 is the amount at the beginning of year 0, which is C.
N = 1 is the amount at the beginning of year 1, which is (C+D)*F."
  (+ (* c (expt f n))
     (* d f (mb-economy--geometric-series f n))))

(cl-defun deposit (n &optional (f 1.0) (c 1.0))
  "Return the yearly deposit that reduces initial amount C to zero.
N is the number of years and F is the yearly growth factor."
  (- (/ (* c (expt f n))
        (* f (mb-economy--geometric-series f n)))))

(cl-defun withdraw-to-zero (n &optional (f 1.0) (d 1.0))
  "Return the initial balance needed for N yearly withdrawals of D.
F is the yearly growth factor.  Withdrawals happen at the
beginning of each year."
  (/ (* d f (mb-economy--geometric-series f n))
     (expt f n)))

(cl-defun profit-to-interest (profit &optional (years 1.0))
  "Return the yearly growth factor for PROFIT accumulated over YEARS."
  (expt profit (/ 1.0 years)))

(provide 'mb-utils-economy)

;;; mb-utils-economy.el ends here
