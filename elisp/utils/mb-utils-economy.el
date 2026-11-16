;;; mb-utils-economy.el --- Small financial calculations -*- lexical-binding: t; -*-

;;; Commentary:

;; Helpers for growth, deposits, withdrawals, annualized returns, and selected
;; average comparisons.

;;; Code:

(require 'cl-lib)
(require 'mb-utils-math)

(defun mb-economy--check-number (value name)
  "Signal an error unless VALUE is a number named NAME."
  (unless (numberp value)
    (error "%s must be a number: %S" name value)))

(defun mb-economy--check-periods (periods &optional positive)
  "Signal an error unless PERIODS is an integer in the required domain.
Require a positive value when POSITIVE is non-nil, otherwise allow zero."
  (unless (and (integerp periods)
	       (if positive (cl-plusp periods) (>= periods 0)))
    (error "Period count must be a %s integer: %S"
	   (if positive "positive" "non-negative")
	   periods)))

(defun mb-economy--check-factor (factor &optional positive)
  "Signal an error unless FACTOR is in the required numeric domain.
Require a positive value when POSITIVE is non-nil, otherwise allow zero."
  (unless (and (numberp factor)
	       (if positive (cl-plusp factor) (>= factor 0)))
    (error "Growth factor must be %s: %S"
	   (if positive "positive" "non-negative")
	   factor)))

(defun grew (increase sum)
  "Return the growth ratio represented by INCREASE in final SUM."
  (mb-economy--check-number increase "Increase")
  (mb-economy--check-number sum "Final sum")
  (let ((initial (- sum increase)))
    (when (zerop initial)
      (error "Initial sum must be nonzero"))
    (/ (float increase) initial)))

(cl-defun CFaR (values start &optional end)
  "Return the difference between the mean of VALUES and a selected mean.
START and END delimit a selected subsequence using zero-based, half-open
indexes.  The subsequence must not be empty; `cl-subseq' validates its
remaining bounds."
  (unless (and (proper-list-p values) values (cl-every #'numberp values))
    (error "VALUES must be a nonempty list of numbers"))
  (let ((selection (cl-subseq values start end)))
    (when (null selection)
      (error "Selected subsequence [%S, %S) is empty" start end))
    (- (average values) (average selection))))

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
  (mb-economy--check-periods n)
  (mb-economy--check-factor f)
  (mb-economy--check-number c "Initial amount")
  (mb-economy--check-number d "Yearly deposit")
  (+ (* c (expt f n))
     (* d f (mb-economy--geometric-series f n))))

(cl-defun deposit (n &optional (f 1.0) (c 1.0))
  "Return the yearly deposit that reduces initial amount C to zero.
N is the number of years and F is the yearly growth factor."
  (mb-economy--check-periods n t)
  (mb-economy--check-factor f t)
  (mb-economy--check-number c "Initial amount")
  (- (/ (* c (expt f n))
        (* f (mb-economy--geometric-series f n)))))

(cl-defun withdraw-to-zero (n &optional (f 1.0) (d 1.0))
  "Return the initial balance needed for N yearly withdrawals of D.
F is the yearly growth factor.  Withdrawals happen at the
beginning of each year."
  (mb-economy--check-periods n)
  (mb-economy--check-factor f t)
  (mb-economy--check-number d "Withdrawal")
  (if (zerop n)
      0.0
    (/ (* d f (mb-economy--geometric-series f n))
       (expt f n))))

(cl-defun profit-to-interest (profit &optional (years 1.0))
  "Return the yearly growth factor for PROFIT accumulated over YEARS."
  (unless (and (numberp profit) (cl-plusp profit))
    (error "Accumulated growth factor must be positive: %S" profit))
  (unless (and (numberp years) (cl-plusp years))
    (error "Years must be positive: %S" years))
  (expt profit (/ 1.0 years)))

(provide 'mb-utils-economy)

;;; mb-utils-economy.el ends here
