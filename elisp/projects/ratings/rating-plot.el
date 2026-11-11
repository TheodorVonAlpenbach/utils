;;; rating-plot.el --- Plot rating histories with Gnuplot -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'mb-polynomials)
(require 'mb-utils-time)

(defun rating-plot--rating-value (rating)
  "Return the estimated rating represented by RATING."
  (if (numberp rating)
      rating
    (car rating)))

(cl-defun rating-plot-write-data
    (path-prefix ratings &optional (time-unit :hour))
  "Write RATINGS to PATH-PREFIX.dat and return its path.
Each rating is a pair of rating value and timestamp.  Rating
values may be numbers or Glicko rating/deviation pairs."
  (let ((data-path (concat path-prefix ".dat"))
        (current-time (now)))
    (with-temp-file data-path
      (dolist (entry (reverse ratings))
        (pcase-let ((`(,rating ,timestamp) entry))
          (insert
           (format
            "%g %g\n"
            (time- timestamp current-time time-unit)
            (rating-plot--rating-value rating))))))
    data-path))

(defun rating-plot--quoted-path (path)
  "Return PATH as a quoted Gnuplot string."
  (prin1-to-string (expand-file-name path)))

(defun rating-plot-write-history-script (path-prefix)
  "Write a Gnuplot rating script for PATH-PREFIX and return its path."
  (let ((script-path (concat path-prefix ".gp"))
        (data-path (concat path-prefix ".dat"))
        (pdf-path (concat path-prefix ".pdf")))
    (with-temp-file script-path
      (insert "set terminal pdfcairo\n")
      (insert
       (format "set output %s\n"
               (rating-plot--quoted-path pdf-path)))
      (insert
       (format "plot %s with lines\n"
               (rating-plot--quoted-path data-path))))
    script-path))

(defun rating-plot-write-expression-script (expression path-prefix)
  "Write a Gnuplot script plotting EXPRESSION for PATH-PREFIX."
  (let ((script-path (concat path-prefix ".gp"))
        (pdf-path (concat path-prefix ".pdf")))
    (with-temp-file script-path
      (insert "set terminal pdfcairo\n")
      (insert
       (format "set output %s\n"
               (rating-plot--quoted-path pdf-path)))
      (insert (format "plot [0:2] %s\n" expression)))
    script-path))

(defun rating-plot--temporary-prefix (prefix)
  "Create a temporary plot directory and return its file prefix.
Use PREFIX when naming the directory."
  (let ((directory
         (make-temp-file
          (format "%s-" (file-name-nondirectory prefix))
          t)))
    (expand-file-name "plot" directory)))

(defun rating-plot--run-script (script-path pdf-path)
  "Run Gnuplot with SCRIPT-PATH and visit PDF-PATH.
Signal a user error when Gnuplot is unavailable, exits
unsuccessfully, or does not create the expected PDF."
  (unless (executable-find "gnuplot")
    (user-error "Gnuplot executable was not found"))
  (let ((status
         (call-process "gnuplot" nil nil nil script-path)))
    (unless (and (integerp status) (zerop status))
      (user-error "Gnuplot failed with status %S" status))
    (unless (file-exists-p pdf-path)
      (user-error "Gnuplot did not create %s" pdf-path))
    (find-file pdf-path)))

(cl-defun rating-plot-expression
    (&key polynomial gnuplot-expression (prefix "gnuplot"))
  "Plot POLYNOMIAL or GNUPLOT-EXPRESSION and visit the resulting PDF."
  (let ((expression
         (or
          (and polynomial
               (pfp polynomial
                    :multiplication "*"
                    :exponent "**"))
          gnuplot-expression)))
    (unless expression
      (user-error
       "Specify either :polynomial or :gnuplot-expression"))
    (let* ((path-prefix
            (rating-plot--temporary-prefix prefix))
           (script-path
            (rating-plot-write-expression-script
             expression path-prefix))
           (pdf-path (concat path-prefix ".pdf")))
      (rating-plot--run-script script-path pdf-path))))

(define-obsolete-function-alias
  'maths-gnuplot #'rating-plot-expression "2026-11-11")

(define-obsolete-function-alias
  'gnuplot #'rating-plot-expression "2026-11-11")

(cl-defun rating-plot-history
    (ratings &optional (time-unit :hour) (prefix "gnuplot"))
  "Plot RATINGS against time and visit the resulting PDF.
Each element is a pair of rating value and timestamp."
  (unless ratings
    (user-error "No ratings are available to plot"))
  (let* ((path-prefix
          (rating-plot--temporary-prefix prefix))
         (_data-path
          (rating-plot-write-data
           path-prefix ratings time-unit))
         (script-path
          (rating-plot-write-history-script path-prefix))
         (pdf-path (concat path-prefix ".pdf")))
    (rating-plot--run-script script-path pdf-path)))

(define-obsolete-function-alias
  'gnuplot-ratings #'rating-plot-history "2026-11-11")

(provide 'rating-plot)

;;; rating-plot.el ends here
