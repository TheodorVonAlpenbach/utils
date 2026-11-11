;;; test-maths.el --- Tests for the interactive Maths interface -*- lexical-binding: t; -*-

(require 'ert)
(require 'maths)

(defconst maths-test--user
  '(1 "Ada" 10 1500 100
    (:metadata :created "2026-01-01" :updated "2026-01-02")))

(defconst maths-test--task
  '(2 :addition 1 (3 4) 7 1700 80
    (:metadata :created "2026-01-01" :updated "2026-01-03")))

(ert-deftest maths-formats-tasks-and-matches ()
  (should (equal (maths-format-task :addition '(3 4))
                 "3 + 4 = "))
  (should (equal (maths-format-task :addition '(3 4) 7)
                 "3 + 4 = 7"))
  (should
   (equal
    (maths-format-match maths-test--user maths-test--task)
    (concat
     "Your rating: 1500 (RD = 100)\n"
     "Task rating: 1700 (RD = 80)\n\n"
     "Problem #2:\n3 + 4 = "))))

(ert-deftest maths-formats-short-judgements ()
  (should
   (string-match-p
    "Correct! Your score was 1.00"
    (substring-no-properties
     (maths-short-judgement 1.0 7 7))))
  (should
   (string-match-p
    "Incorrect! Correct answer is 7"
    (substring-no-properties
     (maths-short-judgement 0.0 6 7)))))

(ert-deftest maths-formats-long-judgements ()
  (let ((judgement
         (substring-no-properties
          (maths-long-judgement
           7 1500 7 1.0
           '((1500 100) (1700 80))
           '((1520 95) (1680 75))))))
    (should (string-match-p "Time spent: 1.5 seconds" judgement))
    (should (string-match-p "Your new rating is 1520 (20)" judgement))
    (should (string-match-p "Task's new RD is 75 (-5)" judgement))))

(ert-deftest maths-reads-answer-and-detects-judgement ()
  (with-temp-buffer
    (insert "7")
    (goto-char (point-min))
    (should (= (maths-read-answer) 7))
    (should-not (maths-answer-submitted-p))
    (goto-char (point-max))
    (insert "\nTime spent: 1.0 seconds")
    (should (maths-answer-submitted-p))))

(ert-deftest maths-keymap-inserts-answer-characters ()
  (with-temp-buffer
    (let ((command (lookup-key maths-mode-map "7")))
      (should (commandp command))
      (call-interactively command)
      (should (equal (buffer-string) "7"))))
  (should (eq (lookup-key maths-mode-map "n")
              #'maths-enter)))

(ert-deftest maths-command-initializes-session ()
  (let (events)
    (cl-letf (((symbol-function 'maths-init)
               (lambda () (push 'init events)))
              ((symbol-function 'switch-to-buffer)
               (lambda (buffer) (push (list 'buffer buffer) events)))
              ((symbol-function 'maths-mode)
               (lambda () (push 'mode events)))
              ((symbol-function 'maths-new)
               (lambda (level) (push (list 'new level) events))))
      (maths 3))
    (should
     (equal
      (nreverse events)
      `(init
        (buffer ,+maths-buffer+)
        mode
        (new 3))))))

(provide 'test-maths)

;;; test-maths.el ends here
