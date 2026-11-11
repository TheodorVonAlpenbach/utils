;;; test-maths-plot.el --- Tests for Maths plotting -*- lexical-binding: t; -*-

(require 'ert)
(require 'maths-plot)

(ert-deftest maths-plot-writes-rating-data-without-mutation ()
  (let* ((directory (make-temp-file "maths-plot-test-" t))
         (prefix (expand-file-name "ratings" directory))
         (ratings
          (copy-tree
           '((1500 "2026-01-02")
             ((1400 100) "2026-01-01"))))
         (original (copy-tree ratings)))
    (unwind-protect
        (cl-letf (((symbol-function 'now)
                   (lambda () "2026-01-03"))
                  ((symbol-function 'time-)
                   (lambda (timestamp current unit)
                     (should (equal current "2026-01-03"))
                     (should (eq unit :day))
                     (if (equal timestamp "2026-01-01") -2 -1))))
          (let ((path
                 (maths-plot-write-data
                  prefix ratings :day)))
            (should (equal ratings original))
            (should
             (equal
              (with-temp-buffer
                (insert-file-contents path)
                (buffer-string))
              "-2 1400\n-1 1500\n"))))
      (delete-directory directory t))))

(ert-deftest maths-plot-writes-ratings-script ()
  (let* ((directory (make-temp-file "maths-plot-test-" t))
         (prefix (expand-file-name "ratings file" directory)))
    (unwind-protect
        (let ((path
               (maths-plot-write-ratings-script prefix)))
          (with-temp-buffer
            (insert-file-contents path)
            (let ((script (buffer-string)))
              (should
               (string-match-p
                "set terminal pdfcairo" script))
              (should
               (string-match-p
                "ratings file\\.pdf" script))
              (should
               (string-match-p
                "ratings file\\.dat\" with lines" script)))))
      (delete-directory directory t))))

(ert-deftest maths-plot-requires-an-expression ()
  (should-error (maths-gnuplot) :type 'user-error))

(ert-deftest maths-plot-reports-missing-gnuplot ()
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_program) nil)))
    (should-error
     (maths-plot--run-script "plot.gp" "plot.pdf")
     :type 'user-error)))

(ert-deftest maths-plot-checks-process-output ()
  (let ((pdf-path (make-temp-file "maths-plot-test-" nil ".pdf")))
    (unwind-protect
        (progn
          (delete-file pdf-path)
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (_program) "/usr/bin/gnuplot"))
                    ((symbol-function 'call-process)
                     (lambda (&rest _) 1)))
            (should-error
             (maths-plot--run-script "plot.gp" pdf-path)
             :type 'user-error))
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (_program) "/usr/bin/gnuplot"))
                    ((symbol-function 'call-process)
                     (lambda (&rest _) 0)))
            (should-error
             (maths-plot--run-script "plot.gp" pdf-path)
             :type 'user-error)))
      (when (file-exists-p pdf-path)
        (delete-file pdf-path)))))

(ert-deftest maths-plot-visits-successful-output ()
  (let ((pdf-path (make-temp-file "maths-plot-test-" nil ".pdf"))
        visited)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (_program) "/usr/bin/gnuplot"))
                  ((symbol-function 'call-process)
                   (lambda (&rest _) 0))
                  ((symbol-function 'find-file)
                   (lambda (path) (setq visited path))))
          (maths-plot--run-script "plot.gp" pdf-path)
          (should (equal visited pdf-path)))
      (delete-file pdf-path))))

(provide 'test-maths-plot)

;;; test-maths-plot.el ends here
