;;; test-cram.el --- Tests for the interactive Cram interface -*- lexical-binding: t; -*-

(require 'ert)
(require 'cram)

(defconst cram-test--user
  '(1 "Ada" (1500 100)
    (:metadata :created "2026-01-01" :updated "2026-01-02")))

(defconst cram-test--problem
  '(2 "birds-2" "Which bird?" "Raven" nil nil nil (1700 80)
    (:metadata :created "2026-01-01" :updated "2026-01-03")))

(ert-deftest cram-formats-problems-and-matches ()
  (should (equal (cram-format-problem "Question") "Question\n"))
  (should (equal (cram-format-problem "Question" 4) "Ques"))
  (should
   (equal
    (cram-format-match cram-test--user cram-test--problem)
    (concat
     "Your rating: 1500 (RD = 100)\n"
     "Problem rating: 1700 (RD = 80)\n\n"
     "Problem #2:\nWhich bird?\n"))))

(ert-deftest cram-formats-short-judgements ()
  (should
   (string-match-p
    "Correct! Your score was 1.00"
    (substring-no-properties
     (cram-short-judgement
      1.0 "Raven" "Raven" nil))))
  (should
   (string-match-p
    "Incorrect! Correct response is \"Raven\""
    (substring-no-properties
     (cram-short-judgement
      0.0 "Crow" "Raven" '("Corvus corax"))))))

(ert-deftest cram-formats-long-judgements ()
  (let ((judgement
         (substring-no-properties
          (cram-long-judgement
           "Raven" 1500 "Raven" nil 1.0
           '((1500 100) (1700 80))
           '((1520 95) (1680 75))))))
    (should (string-match-p "Time spent: 1.5 seconds" judgement))
    (should (string-match-p "Your new rating is 1520 (20)" judgement))
    (should (string-match-p "Problem's new RD is 75 (-5)" judgement))))

(ert-deftest cram-reads-response-and-detects-judgement ()
  (with-temp-buffer
    (insert "  Raven  \n")
    (goto-char (point-min))
    (should (equal (cram-read-response) "Raven"))
    (should-not (cram-response-submitted-p))
    (insert "Time spent: 1.0 seconds")
    (should (cram-response-submitted-p))))

(ert-deftest cram-answer-keymap-inserts-response-characters ()
  (with-temp-buffer
    (let ((command (lookup-key cram-answer-mode-map "7")))
      (should (commandp command))
      (call-interactively command)
      (should (equal (buffer-string) "7"))))
  (should (eq (lookup-key cram-answer-mode-map "n")
              #'cram-enter)))

(ert-deftest cram-entry-commands-use-their-filters ()
  (let (calls)
    (cl-letf (((symbol-function 'cram-1)
               (lambda (filter &optional level)
                 (push (list filter level) calls))))
      (cram 2)
      (serbo-cram 3)
      (quiz-cram 4)
      (quiz-plants 5)
      (quiz-norske-fugler 6))
    (should
     (equal
      (nreverse calls)
      '(("^sk-ref-[3]" 2)
        ("^sk-ref-[0-9]" 3)
        ("^csv-ref-[1]" 4)
        ("^csv-planter-" 5)
        ("^csv-norske-fugler-" 6))))))

(provide 'test-cram)

;;; test-cram.el ends here
