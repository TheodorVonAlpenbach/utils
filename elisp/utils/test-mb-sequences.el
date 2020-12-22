(require 'ert)

(ert-deftest test-minimum ()
"Test of `positions'"
 (should (equal (minimum '(1 2 3 5 5) :start 3) '(5 3 5)))
 (should (equal (minimum '(1 2 3 5 5)) '(1 0 1)))
)

(ert-deftest test-positions ()
"Test of `positions'"
 (should (equal (positions 'a '(a b c a)) '(0 3)))
 (should (equal (positions '(a) '(a b c a)) '(0 3)))
 (should (equal (positions '(a b b) '(a b c a)) '(0 1 3)))
 (should (equal (positions "a" '("a" "b" "c" "a")) nil))
 (should (equal (positions "a" '("a" "b" "c" "a") :test #'string=) '(0 3)))
 (should (equal (positions '(a) '((a) (b) (c) (a)) :key #'car) '(0 3))))

(ert-deftest test-replace-sequence ()
  "Test of `replace-sequence'"
  (should (equal (replace-sequence "01234" "ab" 1)
		 "0ab34"))
  (should (equal (replace-sequence "01234" "ab" 1 1)
		 "0ab1234"))
  (should (equal (replace-sequence "01234" "ab" 1 -1)
		 "0ab4")))

(ert-deftest test-list< ()
  "Test of list<"
  ;; test types
  (should (list< '("n") '("n" "c1") :test #'string<)))

(ert-deftest test-min-element ()
  "Test of min-element"
  ;; test types
  (should (equal (min-element '()) nil)))
;;(string< "n" "o")

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

(ert-deftest test-insert-sequence ()
  "Test of INSERT-SEQUENCE"
  (should (all-equal (insert-sequence "15010" "_" :start1 2)
		     (insert-sequence "15010" "_" :start1 2 :end1 2)
		     (insert-sequence "15010" "_" :start1 2 :end1 -3)
		     (insert-sequence "15010" "_" :start1 -3 :end1 -3)
		     (insert-sequence "15010" "_" :start1 -3 :end1 2)
		     "15_010"))
  (should (equal (insert-sequence "15010" "abc")
		 "abc15010"))
  (should (equal (insert-sequence "15010" "abc" :start1 1)
		 "1abc5010"))
  (should (equal (insert-sequence "15010" "abc" :start1 1 :end1 2)
		 "1abc010"))
  (should (equal (insert-sequence "15010" "abc" :start1 1 :end1 2 :start2 1)
		 "1bc010"))
  (should (equal (insert-sequence "15010" "abc" :start1 1 :end1 2 :start2 1 :end2 -1)
		 "1b010"))
  (should (equal '(1 2 3 4 5)
		 (insert-sequence '(1 2 5) '(3 4) :start1 2)))
  (should (equal [1 2 3 4 5]
		 (insert-sequence [1 2 5] [3 4] :start1 2)))
  (should (equal "complete"
		 (insert-sequence "comete" "pl"
				  :start1 3)))
  (should (equal "concrete"
		 (insert-sequence "comete" "ncr"
				  :start1 2 :end1 3)))
  (should (equal "concrete"
		 (insert-sequence "comete" "incr"
				  :start1 2 :end1 3 :start2 1)))
  (should (equal "concrete"
		 (insert-sequence "comete" "incredible"
				  :start1 2 :end1 3 :start2 1 :end2 4)))
  (should (equal "incrediblecomet"
		 (insert-sequence "comet" "incredible")))
  (should (equal "ediblecomet"
		 (insert-sequence "comet" "incredible"
				  :start2 4)))
  (should (equal "incomet"
		 (insert-sequence "comet" "incredible"
				  :end2 2)))
  (should (equal "dicomet"
		 (insert-sequence "comet" "incredible" :start2 5 :end2 7))))

(ert-deftest test-as-list ()
  "Test of `as-list'"
 (should (equal (as-list (x "abc")
		  (rest x))
		"bc")))

(ert-deftest test-split-if ()
  "Test of `split-if'"
 (should (equal (split-if #'oddp '(1 2 3 4 5)) '(nil (1 2) (3 4) (5))))
 (should (equal (split-if (bind #'equal ?a) "babab") '("b" "ab" "ab"))))

(ert-deftest test-type-of-super ()
  "Test of `type-of-super'"
 (should (equal (type-of-super '(1 2 3)) 'list)))

(provide 'test-mb-sequences)
