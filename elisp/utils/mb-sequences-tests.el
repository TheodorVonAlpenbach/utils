(require 'ert)
(require 'mb-sequences)

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

(ert-deftest test-nminimum-nokey ()
  "Test of nminimum-nokey"
  (should (equal (nminimum-nokey [5118 5002 5116] #'<) '(5002 1)))
  (should (equal (nminimum-nokey [5118 5002 5116] #'>) '(5118 0))))

(ert-deftest test-project ()
  "Test of PROJECT"
  (should (equal '(b a c b) (project '(a b c) '(1 0 2 1))))
  (should (equal '(a b c) (project '(a b c) t)))
  (should (equal 'b (project '(a b c) 1)))
  (should (equal 99 (project "abc" 2)))
  (should (equal "c" (project "abc" '(2))))
  (should (equal 2 (project '(0 1 2 3) #'(lambda (x) (elt x 2)))))
  (should (equal '(0 1 2 (3))
		 (project '(0 1 2 3) (list #'first
					   #'second
					   #'(lambda (x) (elt x 2))
					   #'last)))))

(ert-deftest test-project-sequence ()
  "Test of PROJECT-SEQUENCE"
  (should (equal '(49 57) (project-sequence '("01" "09") 1)))
  (should (equal '("1" "9") (project-sequence '("01" "09") (list 1))))
  (should (equal '((2 3) (5 6)) (project-sequence '((1 2 3) (4 5 6)) '(1 2)))))
