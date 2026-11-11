;;; test-rating-plot.el --- Tests for rating plots -*- lexical-binding: t; -*-

(require 'ert)
(require 'rating-plot)

(ert-deftest rating-plot-writes-data-without-mutation ()
  (let* ((directory (make-temp-file "rating-plot-test-" t))
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
                 (rating-plot-write-data
                  prefix ratings :day)))
            (should (equal ratings original))
            (should
             (equal
              (with-temp-buffer
                (insert-file-contents path)
                (buffer-string))
              "-2 1400\n-1 1500\n"))))
      (delete-directory directory t))))

(ert-deftest rating-plot-writes-history-script ()
  (let* ((directory (make-temp-file "rating-plot-test-" t))
         (prefix (expand-file-name "ratings file" directory)))
    (unwind-protect
        (let ((path
               (rating-plot-write-history-script prefix)))
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

(ert-deftest rating-plot-requires-an-expression ()
  (should-error (rating-plot-expression) :type 'user-error))

(ert-deftest rating-plot-reports-missing-gnuplot ()
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_program) nil)))
    (should-error
     (rating-plot--run-script "plot.gp" "plot.pdf")
     :type 'user-error)))

(ert-deftest rating-plot-checks-process-output ()
  (let ((pdf-path (make-temp-file "rating-plot-test-" nil ".pdf")))
    (unwind-protect
        (progn
          (delete-file pdf-path)
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (_program) "/usr/bin/gnuplot"))
                    ((symbol-function 'call-process)
                     (lambda (&rest _) 1)))
            (should-error
             (rating-plot--run-script "plot.gp" pdf-path)
             :type 'user-error))
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (_program) "/usr/bin/gnuplot"))
                    ((symbol-function 'call-process)
                     (lambda (&rest _) 0)))
            (should-error
             (rating-plot--run-script "plot.gp" pdf-path)
             :type 'user-error)))
      (when (file-exists-p pdf-path)
        (delete-file pdf-path)))))

(ert-deftest rating-plot-visits-successful-output ()
  (let ((pdf-path (make-temp-file "rating-plot-test-" nil ".pdf"))
        visited)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (_program) "/usr/bin/gnuplot"))
                  ((symbol-function 'call-process)
                   (lambda (&rest _) 0))
                  ((symbol-function 'find-file)
                   (lambda (path) (setq visited path))))
          (rating-plot--run-script "plot.gp" pdf-path)
          (should (equal visited pdf-path)))
      (delete-file pdf-path))))

(provide 'test-rating-plot)

;;; test-rating-plot.el ends here
