;;; test-cram-import.el --- Tests for Cram problem imports -*- lexical-binding: t; -*-

(require 'ert)
(require 'cram-import)

(defun cram-import-test--database ()
  "Return a Cram database containing one problem."
  (let* ((database (cram-create-database))
         (problems (ld-find-table '(:cram :problem) database)))
    (setf
     (ld-table-data problems)
     (copy-tree
      '((1 "existing-1" "Old question" "Old answer"
         "old.jpg" "Old alternative" "Old hint" (1700 80)
         (:metadata :created "2026-01-01"
          :updated "2026-01-01")))))
    database))

(ert-deftest cram-import-converts-problem-fields ()
  (should
   (equal
    (cram-import--problem-arguments
     '("source-1" "Question" "Answer" "" "" "Hint" "3"))
    '("source-1" "Question" "Answer" nil nil "Hint" (1500 350))))
  (should-error
   (cram-import--problem-arguments '("too" "short"))
   :type 'user-error))

(ert-deftest cram-import-inserts-and-updates-problems ()
  (let* ((*current-database* (cram-import-test--database))
         (file (make-temp-file "cram-import-" nil ".csv"))
         (csv
          (concat
           "source-id;question;answer;picture;alternatives;hints;difficulty\n"
           "existing-1;New question;New answer;new.jpg;;New hint;3\n"
           "new-2;Second question;Second answer;;;;3\n")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert csv))
          (cl-letf (((symbol-function 'format-time-string)
                     (lambda (&rest _) "now"))
                    ((symbol-function 'message) #'ignore))
            (let ((rows (cram-import-csv file))
                  (existing (cram-db-get-problem 1))
                  (inserted (cram-db-get-problem 2)))
              (should (= (length rows) 2))
              (should (equal (cram-problem-question existing)
                             "New question"))
              (should (equal (cram-problem-rating existing)
                             '(1700 80)))
              (should (equal (cram-problem-source-id inserted)
                             "new-2"))
              (should (equal (cram-problem-rating inserted)
                             '(1500 350)))
              (should (= (length (ld-table-data* :problem)) 2)))))
      (delete-file file))))

(ert-deftest cram-import-formats-question-and-picture ()
  (should (equal (cram-format-question "A") "A"))
  (should (equal (cram-format-question "A, B") "A og B"))
  (should (equal (cram-format-question "A, B, C")
                 "A, B og C"))
  (should (equal (cram-format-picture "parnassia palustris")
                 "Parnassia_palustris.jpg")))

(ert-deftest cram-import-formats-plant-rows ()
  (with-temp-buffer
    (rename-buffer "plantescratch" t)
    (insert "Aremark\tParnassia palustris\tjåblom\n")
    (should
     (equal
      (cram-parse-planter 4 (current-buffer) "plant-")
      (concat
       "plant-4;Hvilken plante er kommuneblomst i Aremark;"
       "jåblom;Parnassia_palustris.jpg;;;3")))))

(ert-deftest cram-import-formats-bird-rows ()
  (should
   (equal
    (cram-format-norsk-fugl-ancestors
     '("Galliformes" "Phasianidae" ""))
    "Galliformes - Phasianidae"))
  (should
   (equal
    (cram-format-norsk-fugl
     1 "Galliformes" "Phasianidae" ""
     "Coturnix" "coturnix" "vaktel")
    (concat
     "csv-norske-fugler-1;Galliformes - Phasianidae - "
     "Coturnix coturnix;vaktel;"
     "norske-fugler/Coturnix_coturnix.jpg;;;3"))))

(provide 'test-cram-import)

;;; test-cram-import.el ends here
