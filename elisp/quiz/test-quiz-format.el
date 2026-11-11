;;; test-quiz-format.el --- Tests for quiz item formatting -*- lexical-binding: t; -*-

(require 'ert)
(require 'quiz-format)

(ert-deftest quiz-format-defines-item-vocabulary ()
  (should (equal quiz-item-prefixes "QASCETDX"))
  (should (equal (quiz-item-prefix :question) "Q"))
  (should (equal (quiz-item-prefix :alternatives) "X")))

(ert-deftest quiz-format-formats-items ()
  (should
   (equal
    (quiz-format-item :question 12 "Capital?")
    "Q12: Capital?\n"))
  (should
   (equal
    (quiz-format-item :answer 1)
    "A1: \n")))

(ert-deftest quiz-format-supports-item-shortcuts ()
  (should
   (equal
    (mapcar
     (lambda (formatter)
       (funcall formatter 2 "Text"))
     '(quiz-item-q quiz-item-a quiz-item-s quiz-item-c
       quiz-item-e quiz-item-t quiz-item-d quiz-item-x))
    '("Q2: Text\n" "A2: Text\n" "S2: Text\n" "C2: Text\n"
      "E2: Text\n" "T2: Text\n" "D2: Text\n" "X2: Text\n"))))

(ert-deftest quiz-format-rejects-invalid-items ()
  (should-error
   (quiz-format-item :unknown 1 "Text")
   :type 'user-error)
  (should-error
   (quiz-format-item :question 0 "Text")
   :type 'user-error)
  (should-error
   (quiz-format-item :question 1 nil)
   :type 'user-error))

(ert-deftest quiz-format-builds-item-regexps ()
  (should
   (string-match-p
    (quiz-item-start-regexp)
    "Q12: Question"))
  (should-not
   (string-match-p
    (quiz-item-start-regexp "QA")
    "S12: Source"))
  (should
   (string-match-p
    (quiz-item-start-regexp "QA" nil)
    "\n\nA2: Answer")))

(provide 'test-quiz-format)

;;; test-quiz-format.el ends here
