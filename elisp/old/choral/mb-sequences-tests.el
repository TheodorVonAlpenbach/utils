(require 'ert)
(require 'mb-sequences)

;;;; ls-assignment
(ert-deftest test-copy-if ()
  "Test of copy-if"
  ;; test types
  (let ((seqs (list '() '(1) '(a) (make-list 1000 nil)
		    (vector) (vector 1) "" "string")))
    (should (equal (mapcar (bind #'copy-if #'always 1) seqs) seqs)))

  ;; :from-end
  (should (equal (copy-if #'oddp '(1 2 3 4) :count 1) '(1)))
  (should (equal (copy-if #'oddp '(1 2 3 4) :count 1 :from-end nil) '(1)))
  (should (equal (copy-if #'oddp '(1 2 3 4) :count 1 :from-end t) '(3)))

  ;; :count
  (should (equal (length (copy-if #'always "12345" :count 0)) 0))
  (should (equal (length (copy-if #'always "12345" :count 1)) 1))
  (should (equal (length (copy-if #'always "12345" :count 5)) 5))
  (should (equal (length (copy-if #'always "12345")) 5)))
