;;; test-quiz-summary.el --- Tests for quiz summaries -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'quiz-summary)

(ert-deftest quiz-summary-adds-calendar-days ()
  (should
   (equal
    (quiz-date-add-days "2024-02-28")
    "2024-02-29"))
  (should
   (equal
    (quiz-date-add-days "2024-02-28" 2)
    "2024-03-01"))
  (should
   (equal
    (quiz-date-add-days "2025-01-01" -1)
    "2024-12-31")))

(ert-deftest quiz-summary-validates-range-and-items ()
  (should-error
   (quiz-compile-summary
    :iso-from "2026-11-11"
    :iso-to "2026-11-11"
    :display nil)
   :type 'user-error)
  (should-error
   (quiz-compile-summary
    :iso-from "2026-11-11"
    :iso-to "2026-11-12"
    :items ""
    :display nil)
   :type 'user-error)
  (should-error
   (quiz-compile-summary
    :iso-from "2026-11-11"
    :iso-to "2026-11-12"
    :items "QZ"
    :display nil)
   :type 'user-error))

(ert-deftest quiz-summary-aggregates-and-filters-files ()
  (let ((quiz-directory "/quiz/")
        generated-buffer)
    (cl-letf
        (((symbol-function
           'quiz-summary--file-exists-p)
          (lambda (path)
            (not
             (string-suffix-p
              "mq-2026-11-12.qz" path))))
         ((symbol-function
           'quiz-summary--insert-file-contents)
          (lambda (path &rest _arguments)
            (insert
             (if (string-suffix-p
                  "mq-2026-11-11.qz" path)
                 (concat
                  "Q1: First?\n"
                  "A1: First answer\n"
                  "S1: First source\n")
               (concat
                "Q1: Third?\n"
                "A1: Third answer\n"
                "S1: Third source\n"))))))
      (setq generated-buffer
            (quiz-compile-summary
             :iso-from "2026-11-11"
             :iso-to "2026-11-14"
             :items "QA"
             :display nil)))
    (unwind-protect
        (with-current-buffer generated-buffer
          (should
           (string-match-p
            "mq-2026-11-11"
            (buffer-string)))
          (should-not
           (string-match-p
            "mq-2026-11-12"
            (buffer-string)))
          (should
           (string-match-p
            "mq-2026-11-13"
            (buffer-string)))
          (should-not
           (string-match-p
            "^S1:"
            (buffer-string))))
      (kill-buffer generated-buffer))))

(ert-deftest quiz-summary-builds-requested-day-range ()
  (let (arguments)
    (cl-letf
        (((symbol-function 'quiz-today)
          (lambda () "2026-11-11"))
         ((symbol-function 'quiz-compile-summary)
          (lambda (&rest args)
            (setq arguments args))))
      (quiz-summary 7 "QS"))
    (should
     (equal
      arguments
      '(:iso-from "2026-11-05"
        :iso-to "2026-11-12"
        :items "QS")))))

(ert-deftest quiz-summary-handles-raw-prefix-arguments ()
  (let (arguments)
    (cl-letf
        (((symbol-function 'quiz-summary)
          (lambda (&rest args)
            (setq arguments args))))
      (quiz-summary-print '(4)))
    (should
     (equal arguments '(4 "QA")))))

(ert-deftest quiz-summary-finds-source-question ()
  (with-temp-buffer
    (insert
     "mq-2026-11-10\n"
     "=============\n\n"
     "Q3: Question\n"
     "A3: Answer\n")
    (goto-char (point-max))
    (let (arguments)
      (cl-letf
          (((symbol-function
             'quiz-summary-edit-question-1)
            (lambda (&rest args)
              (setq arguments args))))
        (quiz-summary-edit-question
         "/quiz/"))
      (should
       (equal
        arguments
        '(3 "2026-11-10" "/quiz/"))))))

(ert-deftest quiz-summary-reports-missing-heading ()
  (with-temp-buffer
    (insert "Q1: Question\nA1: Answer\n")
    (goto-char (point-max))
    (should-error
     (quiz-summary-edit-question)
     :type 'user-error)))

(provide 'test-quiz-summary)

;;; test-quiz-summary.el ends here
