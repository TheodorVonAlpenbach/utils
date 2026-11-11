;;; test-quiz-cleanup.el --- Tests for quiz cleanup commands -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'quiz-cleanup)

(defconst quiz-cleanup-test--contents
  (concat
   "Q1: first question\n"
   "A1: first answer\n"
   "S1: first source\n\n"
   "Q2: second question\n"
   "A2: second answer\n"
   "S2: second source\n")
  "Quiz text used by cleanup tests.")

(defmacro quiz-cleanup-test--with-buffer (&rest body)
  "Run BODY in a populated temporary quiz buffer."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert quiz-cleanup-test--contents)
     (goto-char (point-min))
     ,@body))

(ert-deftest quiz-cleanup-repairs-known-mojibake ()
  (with-temp-buffer
    (insert "blÃ¥bÃ¦r Â«smÃ¶rÂ»")
    (quiz-repair-mojibake)
    (should
     (equal
      (buffer-string)
      "blåbær «smör»"))))

(ert-deftest quiz-cleanup-removes-next-item ()
  (quiz-cleanup-test--with-buffer
    (should (quiz-remove-A))
    (should-not
     (string-match-p "^A1:" (buffer-string)))
    (should
     (string-match-p "^S1:" (buffer-string)))
    (should
     (string-match-p "^A2:" (buffer-string)))))

(ert-deftest quiz-cleanup-removes-items-from-whole-buffer ()
  (quiz-cleanup-test--with-buffer
    (should (= (quiz-remove-S-buffer) 2))
    (should-not
     (string-match-p "^S[0-9]+:" (buffer-string)))
    (should
     (string-match-p "\n\nQ2:" (buffer-string)))))

(ert-deftest quiz-cleanup-moves-question-region-to-answer ()
  (with-temp-buffer
    (insert
     "Q1: Capital of Norway Oslo\n"
     "A1: \n")
    (goto-char (point-min))
    (search-forward "Oslo")
    (let ((end (point))
          (beg (- (point) 4)))
      (quiz-move-active-region-to-a beg end))
    (should
     (equal
      (buffer-string)
      (concat
       "Q1: Capital of Norway \n"
       "A1: Oslo\n")))))

(ert-deftest quiz-cleanup-rejects-region-outside-question ()
  (with-temp-buffer
    (insert "A1: Answer")
    (goto-char (point-max))
    (should-error
     (quiz-move-active-region-to-a 5 11)
     :type 'user-error)))

(ert-deftest quiz-cleanup-normalizes-question-and-answer ()
  (with-temp-buffer
    (insert
     "Q1: first question!!!\n"
     "A1: first answer...\n")
    (goto-char (point-min))
    (let ((fill-column 80)
          saved)
      (cl-letf
          (((symbol-function 'save-buffer)
            (lambda (&rest _arguments)
              (setq saved t))))
        (quiz-super-save))
      (should saved))
    (should
     (equal
      (buffer-string)
      (concat
       "Q1: First question?\n"
       "A1: First answer\n")))))

(ert-deftest quiz-cleanup-preserves-answer-spacing ()
  (with-temp-buffer
    (insert
     "Q1: Question text\n"
     "A1: existing answer\n")
    (goto-char (point-min))
    (search-forward "text")
    (quiz-move-active-region-to-a
     (- (point) 4)
     (point))
    (should
     (string-match-p
      "^A1: text existing answer$"
      (buffer-string)))))

(provide 'test-quiz-cleanup)

;;; test-quiz-cleanup.el ends here
