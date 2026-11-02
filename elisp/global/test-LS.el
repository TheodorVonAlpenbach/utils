;;; test-LS.el --- Tests for LS work-log helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'LS)

(ert-deftest ls-time-within-uses-half-open-interval ()
  (should
   (ls-time-within-p "2026-11-02T09:00"
                     '("2026-11-02T09:00" "2026-11-02T10:00")))
  (should-not
   (ls-time-within-p "2026-11-02T10:00"
                     '("2026-11-02T09:00" "2026-11-02T10:00"))))

(ert-deftest ls-parses-days-within-range ()
  (let ((log
         (concat
          "* Logg\n"
          "** 2026-11-01\n08:00 Start\n"
          "** 2026-11-02\n09:00 Start\n")))
    (should
     (equal
      (mapcar #'car
              (ls-log-days log
                           "2026-11-02T00:00"
                           "2026-11-03T00:00"))
      '("2026-11-02")))))

(ert-deftest ls-parses-items-within-range ()
  (let ((day-log
         '("2026-11-02"
           "08:00 Ankomst\n12:00 Lunch\n16:00 Hjem\n")))
    (should
     (equal
      (mapcar #'car
              (ls-log-items day-log
                            "2026-11-02T09:00"
                            "2026-11-02T16:01"))
      '("12:00" "16:00")))))

(ert-deftest ls-log-string-extracts-log-section ()
  (with-temp-buffer
    (insert "* Notes\ntext\n* Logg\n** 2026-11-02\n08:00 Start\n")
    (should
     (equal (ls-log-string (current-buffer))
            "* Logg\n** 2026-11-02\n08:00 Start\n"))))

(ert-deftest ls-log-string-requires-heading ()
  (with-temp-buffer
    (insert "* Notes\n")
    (should-error
     (ls-log-string (current-buffer))
     :type 'user-error)))

(ert-deftest ls-hours-calculates-rounded-workday ()
  (cl-letf (((symbol-function 'ls-log-string)
             (lambda (&optional _buffer) "log"))
            ((symbol-function 'ls-log-days)
             (lambda (&rest _)
               '(("2026-11-02" "items"))))
            ((symbol-function 'ls-log-items)
             (lambda (&rest _)
               '(("08:00" "Start") ("16:10" "End")))))
    (should
     (equal (ls-hours
             "2026-11-02T00:00" "2026-11-03T00:00" 0.5)
            '(("2026-11-02" 7.75))))))

(ert-deftest ls-estimate-applies-payment-factors ()
  (cl-letf (((symbol-function 'ls-hours)
             (lambda (&rest _)
               '(("one" 2.0) ("two" 3.0)))))
    (should (= (ls-estimate nil nil 100 0.8 0.5)
               200.0))))

(ert-deftest ls-insertion-commands-require-work-log-buffer ()
  (with-temp-buffer
    (should-error (ls-insert-arrival-time) :type 'user-error)
    (should-error (ls-insert-departure-time) :type 'user-error)))

(ert-deftest ls-inserts-arrival-entry ()
  (let ((ls-notes-buffer-name "work-log")
        (buffer (generate-new-buffer "work-log")))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (insert "* Logg\n")
          (cl-letf (((symbol-function 'insert-date)
                     (lambda (_prefix) (insert "2026-11-02")))
                    ((symbol-function 'insert-time)
                     (lambda (_prefix) (insert "08:15"))))
            (ls-insert-arrival-time))
          (should
           (string-suffix-p
            "** 2026-11-02()\n08:15 Ankomst\n"
            (buffer-string))))
      (kill-buffer buffer))))

(ert-deftest ls-inserts-departure-entry ()
  (let ((ls-notes-buffer-name "work-log")
        (buffer (generate-new-buffer "work-log")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "* Logg\n")
          (cl-letf (((symbol-function 'insert-time)
                     (lambda (_prefix) (insert "16:30"))))
            (ls-insert-departure-time))
          (should
           (string-suffix-p "16:30 Hjem\n" (buffer-string))))
      (kill-buffer buffer))))

(ert-deftest ls-project-number-and-directory-lookup ()
  (let ((directory (make-temp-file "ls-projects-" t)))
    (unwind-protect
        (let ((ls-project-directory directory))
          (make-directory
           (expand-file-name "15_010-example" directory))
          (should (equal (ls-project-number->tag 15010) "15_010"))
          (should
           (equal (ls-project-name 15010) "15_010-example"))
          (should-not (ls-project-name 99999)))
      (delete-directory directory t))))

(ert-deftest ls-compatibility-aliases ()
  (dolist (pair
           '((LS-fontify . ls-fontify)
             (LS-buffer . ls-buffer)
             (LS-log-day-separator . ls-log-day-separator)
             (LS-log-item-separator . ls-log-item-separator)
             (time-within . ls-time-within-p)
             (LS-estimate . ls-estimate)
             (LS-log-items . ls-log-items)
             (LS-log-days . ls-log-days)
             (LS-log-string . ls-log-string)
             (LS-hours . ls-hours)
             (ls-insert-depature-time . ls-insert-departure-time)))
    (should
     (eq (indirect-function (car pair))
         (indirect-function (cdr pair))))))

(provide 'test-LS)

;;; test-LS.el ends here
