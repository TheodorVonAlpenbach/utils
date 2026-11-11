;;; test-quiz-import.el --- Tests for pasted quiz import -*- lexical-binding: t; -*-

(require 'ert)
(require 'quiz-import)

(ert-deftest quiz-import-parses-aftenposten-question ()
  (should
   (equal
    (quiz-import-aftenposten-parse-question
     "1. What is the answer?\nRiktig svar: Forty-two")
    '("What is the answer?" . "Forty-two"))))

(ert-deftest quiz-import-trims-field-whitespace ()
  (should
   (equal
    (quiz-import-aftenposten-parse-question
     "  7.   Question text  \n  Riktig svar:   Answer text  ")
    '("Question text" . "Answer text"))))

(ert-deftest quiz-import-rejects-malformed-question ()
  (should-error
   (quiz-import-aftenposten-parse-question
    "Question without expected labels")
   :type 'user-error))

(ert-deftest quiz-import-converts-question-region ()
  (with-temp-buffer
    (insert
     "1. Question?\n"
     "Riktig svar: Answer")
    (quiz-import-aftenposten-question
     (point-min)
     (point-max)
     3
     "Source")
    (should
     (equal
      (buffer-string)
      (concat
       "Q3: Question?\n"
       "A3: Answer\n"
       "S3: Source\n")))))

(ert-deftest quiz-import-converts-whole-buffer ()
  (with-temp-buffer
    (insert
     "1. First?\n"
     "Riktig svar: One\n\n"
     "2. Second?\n"
     "Riktig svar: Two\n")
    (should
     (= (quiz-import-aftenposten-buffer) 2))
    (should
     (equal
      (buffer-string)
      (concat
       "Q1: First?\n"
       "A1: One\n\n"
       "Q2: Second?\n"
       "A2: Two\n\n")))))

(ert-deftest quiz-import-validates-before-modifying-buffer ()
  (with-temp-buffer
    (insert
     "1. Valid?\n"
     "Riktig svar: Yes\n\n"
     "Malformed second question")
    (let ((original (buffer-string)))
      (should-error
       (quiz-import-aftenposten-buffer)
       :type 'user-error)
      (should
       (equal (buffer-string) original)))))

(ert-deftest quiz-entrypoint-loads-maintained-features ()
  (require 'quiz)
  (dolist (feature
           '(quiz-format
             quiz-navigation
             quiz-mode
             quiz-config
             quiz-edit
             quiz-layout
             quiz-cleanup
             quiz-csv
             quiz-summary
             quiz-mail
             quiz-import
             quiz))
    (should (featurep feature))))

(provide 'test-quiz-import)

;;; test-quiz-import.el ends here
