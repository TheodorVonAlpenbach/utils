;;; test-quiz-navigation.el --- Tests for quiz navigation -*- lexical-binding: t; -*-

(require 'ert)
(require 'quiz-navigation)

(defconst quiz-navigation-test--contents
  (concat
   "Q1: First question\n"
   "continued\n"
   "A1: First answer\n"
   "S1: First source\n\n"
   "Q2: Second question\n"
   "A2: Second answer\n"
   "C2: Comment\n")
  "Quiz text used by navigation tests.")

(defmacro quiz-navigation-test--with-buffer (&rest body)
  "Run BODY in a populated temporary quiz buffer."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert quiz-navigation-test--contents)
     (goto-char (point-min))
     ,@body))

(ert-deftest quiz-navigation-moves-to-list-boundaries ()
  (quiz-navigation-test--with-buffer
    (goto-char 20)
    (should (= (quiz-beginning-of-list) (point-min)))
    (should (= (quiz-end-of-list) (point-max)))))

(ert-deftest quiz-navigation-finds-question-boundaries ()
  (quiz-navigation-test--with-buffer
    (search-forward "First answer")
    (should
     (= (quiz-beginning-of-question)
        (point-min)))
    (should
     (equal
      (buffer-substring-no-properties
       (point)
       (quiz-end-of-question))
      (string-trim-right
       (car
        (split-string
         quiz-navigation-test--contents "\n\n")))))))

(ert-deftest quiz-navigation-moves-between-questions ()
  (quiz-navigation-test--with-buffer
    (should (quiz-forward-question))
    (should (looking-at "Q2:"))
    (should (quiz-backward-question))
    (should (looking-at "Q1:"))
    (should (quiz-goto-question 2))
    (should (looking-at "Q2:"))
    (should-not (quiz-goto-question 3))))

(ert-deftest quiz-navigation-supports-negative-question-movement ()
  (quiz-navigation-test--with-buffer
    (quiz-goto-question 2)
    (should (quiz-forward-question -1))
    (should (looking-at "Q1:"))
    (should (quiz-backward-question -1))
    (should (looking-at "Q2:"))))

(ert-deftest quiz-navigation-moves-between-items ()
  (quiz-navigation-test--with-buffer
    (search-forward "continued")
    (should (quiz-beginning-of-item))
    (should (looking-at "Q1:"))
    (should (quiz-forward-item))
    (should (looking-at "A1:"))
    (should (quiz-backward-item))
    (should (looking-at "Q1:"))))

(ert-deftest quiz-navigation-finds-item-text-and-boundaries ()
  (quiz-navigation-test--with-buffer
    (search-forward "First answer")
    (let ((start
           (quiz-current-item-region-start))
          (end
           (quiz-current-item-region-end)))
      (should
       (equal
        (buffer-substring-no-properties start end)
        "A1: First answer")))
    (should (quiz-beginning-of-item-text))
    (should (looking-at "First answer"))))

(ert-deftest quiz-navigation-reports-question-end-at-position ()
  (quiz-navigation-test--with-buffer
    (let ((position
           (progn
             (search-forward "Second question")
             (point))))
      (should
       (equal
        (buffer-substring-no-properties
         (save-excursion
           (goto-char position)
           (quiz-beginning-of-question))
         (quiz-question-end position))
        (string-trim-right
         (cadr
          (split-string
           quiz-navigation-test--contents "\n\n"))))))))

(provide 'test-quiz-navigation)

;;; test-quiz-navigation.el ends here
