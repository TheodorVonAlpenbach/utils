;;; test-quiz-edit.el --- Tests for quiz structural editing -*- lexical-binding: t; -*-

(require 'ert)
(require 'quiz-edit)

(defconst quiz-edit-test--contents
  (concat
   "Q4: First question\n"
   "A4: First answer\n"
   "S4: First source\n\n"
   "Q9: Second question\n"
   "A9: Second answer\n")
  "Quiz text used by structural editing tests.")

(defmacro quiz-edit-test--with-buffer (&rest body)
  "Run BODY in a populated temporary quiz buffer."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert quiz-edit-test--contents)
     (goto-char (point-min))
     ,@body))

(ert-deftest quiz-edit-identifies-current-item ()
  (quiz-edit-test--with-buffer
    (search-forward "First answer")
    (should (eq (quiz-current-item) :answer))
    (should (quiz-current-item-p :answer))
    (should (= (quiz-current-item-number) 4))))

(ert-deftest quiz-edit-computes-next-question-number ()
  (with-temp-buffer
    (should (= (quiz-next-question-number) 1)))
  (quiz-edit-test--with-buffer
    (goto-char (point-max))
    (should (= (quiz-next-question-number) 10))))

(ert-deftest quiz-edit-sets-current-item-number ()
  (quiz-edit-test--with-buffer
    (search-forward "First answer")
    (quiz-set-item-number 7)
    (should (string-match-p "^A7:" (thing-at-point 'line t)))
    (goto-char (point-min))
    (should (looking-at "Q4:"))))

(ert-deftest quiz-edit-sets-whole-question-number ()
  (quiz-edit-test--with-buffer
    (search-forward "First answer")
    (quiz-set-question-number 3)
    (should
     (string-prefix-p
      (concat
       "Q3: First question\n"
       "A3: First answer\n"
       "S3: First source\n")
      (buffer-string)))
    (should (string-match-p "^Q9:" (buffer-string)))))

(ert-deftest quiz-edit-renumbers-all-questions ()
  (quiz-edit-test--with-buffer
    (should (= (quiz-renumber-questions 2) 2))
    (should
     (equal
      (buffer-string)
      (concat
       "Q2: First question\n"
       "A2: First answer\n"
       "S2: First source\n\n"
       "Q3: Second question\n"
       "A3: Second answer\n")))))

(ert-deftest quiz-edit-inserts-question-into-empty-buffer ()
  (with-temp-buffer
    (let ((end
           (quiz-insert-question-base
            "Question?" "Answer" "Source")))
      (should (= end (point-max)))
      (should
       (equal
        (buffer-string)
        (concat
         "Q1: Question?\n"
         "A1: Answer\n"
         "S1: Source\n\n"))))))

(ert-deftest quiz-edit-appends-numbered-question ()
  (quiz-edit-test--with-buffer
    (let ((end
           (quiz-insert-question
            "Third?" "Third answer" "Third source"
            (current-buffer))))
      (should (= end (point-max)))
      (should
       (string-suffix-p
        (concat
         "\nQ10: Third?\n"
         "A10: Third answer\n"
         "S10: Third source\n\n")
        (buffer-string)))
      (goto-char end)
      (should (quiz-beginning-of-item-text "Q"))
      (should (looking-at "Third?")))))

(ert-deftest quiz-edit-inserts-extended-template ()
  (with-temp-buffer
    (quiz-insert-question-template)
    (should
     (equal
      (buffer-string)
      (concat
       "Q1: \n"
       "A1: \n"
       "C1: \n"
       "E1: \n"
       "T1: \n"
       "D1: \n\n")))))

(provide 'test-quiz-edit)

;;; test-quiz-edit.el ends here
