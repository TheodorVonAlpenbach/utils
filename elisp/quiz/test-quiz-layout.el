;;; test-quiz-layout.el --- Tests for quiz paragraph layout -*- lexical-binding: t; -*-

(require 'ert)
(require 'quiz-layout)

(ert-deftest quiz-layout-computes-digit-aware-indent ()
  (dolist (case
           '(("Q1: Text" 3)
             ("Q12: Text" 4)
             ("Q123: Text" 5)))
    (with-temp-buffer
      (insert (car case))
      (goto-char (point-max))
      (should
       (= (quiz-paragraph-indent)
          (cadr case))))))

(ert-deftest quiz-layout-computes-hanging-column ()
  (with-temp-buffer
    (insert "Q12: Text")
    (goto-char (point-max))
    (let ((quiz-item-hanging-space 2))
      (should (= (quiz-hanging-column) 6)))))

(ert-deftest quiz-layout-rejects-non-item-position ()
  (with-temp-buffer
    (insert "Ordinary text")
    (should-error
     (quiz-paragraph-indent)
     :type 'user-error)
    (should-error
     (quiz-fill-item)
     :type 'user-error)))

(ert-deftest quiz-layout-fills-with-hanging-indent ()
  (with-temp-buffer
    (insert
     (concat
      "Q12: This is a sufficiently long question "
      "to wrap over several lines when it is filled."))
    (goto-char (point-max))
    (let ((fill-column 34))
      (quiz-fill-item))
    (let ((lines
           (split-string (buffer-string) "\n")))
      (should (> (length lines) 1))
      (should (string-prefix-p "Q12: " (car lines)))
      (dolist (line (cdr lines))
        (should
         (string-prefix-p "     " line))))))

(ert-deftest quiz-layout-retains-hanging-space-alias ()
  (let ((quiz-item-hanging-space 3))
    (should (= quiz-hanging-space 3))))

(provide 'test-quiz-layout)

;;; test-quiz-layout.el ends here
