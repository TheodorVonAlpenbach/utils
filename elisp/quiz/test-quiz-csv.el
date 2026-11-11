;;; test-quiz-csv.el --- Tests for quiz CSV export -*- lexical-binding: t; -*-

(require 'ert)
(require 'quiz-csv)

(defmacro quiz-csv-test--with-buffer (&rest body)
  "Run BODY in a temporary populated quiz buffer."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (rename-buffer "sample.qz" t)
     (insert
      "Q1: Capital of Norway?\n"
      "A1: Oslo\n"
      "S1: Atlas\n\n"
      "Q2: Name A;B\n"
      "A2: He said \"yes\"\n"
      "S2: Notes\n")
     ,@body))

(ert-deftest quiz-csv-collects-question-blocks ()
  (quiz-csv-test--with-buffer
    (let ((questions (quiz-collect-qs (current-buffer))))
      (should (= (length questions) 2))
      (should (string-prefix-p "Q1:" (car questions)))
      (should (string-prefix-p "Q2:" (cadr questions))))))

(ert-deftest quiz-csv-parses-item-text ()
  (should
   (equal
    (quiz-parse-q
     "Q1: A long\n  question\nA1: Answer\nE1: Extra\nS1: Source")
    '("A long question" "Answer" "Extra" "Source"))))

(ert-deftest quiz-csv-converts-and-escapes-fields ()
  (quiz-csv-test--with-buffer
    (should
     (equal
      (quiz-convert-to-csv (current-buffer))
      (concat
       "Capital of Norway?;Oslo;Atlas\n"
       "\"Name A;B\";\"He said \"\"yes\"\"\";Notes")))))

(ert-deftest quiz-csv-can-remove-source-fields ()
  (quiz-csv-test--with-buffer
    (should
     (equal
      (quiz-convert-to-csv (current-buffer) t)
      (concat
       "Capital of Norway?;Oslo\n"
       "\"Name A;B\";\"He said \"\"yes\"\"\"")))))

(ert-deftest quiz-csv-builds-portable-output-path ()
  (let ((quiz-csv-directory "/tmp/quiz-export/"))
    (should
     (equal
      (quiz-csv-path "archive/sample.qz")
      "/tmp/quiz-export/sample.csv"))))

(ert-deftest quiz-csv-writes-export-file ()
  (quiz-csv-test--with-buffer
    (let ((quiz-csv-directory
           (make-temp-file "quiz-csv-test-" t)))
      (unwind-protect
          (let ((path
                 (quiz-save-as-csv (current-buffer) t)))
            (should (file-exists-p path))
            (should
             (equal
              (with-temp-buffer
                (insert-file-contents path)
                (buffer-string))
              (concat
               "Capital of Norway?;Oslo\n"
               "\"Name A;B\";\"He said \"\"yes\"\"\""))))
        (delete-directory quiz-csv-directory t)))))

(provide 'test-quiz-csv)

;;; test-quiz-csv.el ends here
