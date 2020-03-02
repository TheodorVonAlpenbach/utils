(require 'ert)
(require 'mb-utils-div)

(ert-deftest test-all-equal ()
  "Test of `all-equal'"
  (should (equal (all-equal) t))
  (should (equal (all-equal 1) t))
  (should (equal (all-equal 1 1) t))
  (should (equal (all-equal 1 2) nil)))

(ert-deftest test-push-list ()
  "Test of `push-list'"
  (let ((list '(a))
	(prefix '(pre fix)))
    (should (equal (push-list '() list) '(a)))
    (should (equal list '(a)))
    (should (equal (push-list prefix list) '(pre fix a)))
    (should (equal list '(pre fix a)))
    (should (equal prefix '(pre fix)))))

(ert-deftest test-push-back-list ()
"Test of `push-back'"
   (let ((list (list 'a))
	 (suffix '(suf fix)))
    (should (equal (push-back-list '() list) '(a)))
    (should (equal list '(a)))
    (should (equal (push-back-list suffix list) '(a suf fix)))
    (should (equal list '(a suf fix)))
    (should (equal suffix '(suf fix)))))

(ert-deftest test-last-elt ()
"Test of `last-elt'"
 (should (equal (last-elt '(a b c) 0) 'c))
 (should (equal (last-elt '(a b c) 1) 'b))
 (should (equal (last-elt '(a b c)) 'c)))

(ert-deftest test-nor ()
  "Test of `nor'"
  (should (nor))
  (should (nor nil))
  (should (nor nil nil nil))
  (should-not (nor nil t))
  (should-not (nor nil t (error "Not visible")))
  (should (equal 666 (condition-case nil
			 (nor nil nil (error "Visible"))
		       (error 666)))))

(ert-deftest test-nand ()
  "Test of `nand'"
  (should-not (nand)) ;; empty argument list -> no argument is nil!
  (should (nand nil))
  (should (nand t nil))
  (should (nand t nil (error "Not visible")))
  (should (equal 666 (condition-case nil
			 (nand t t (error "Visible"))
		       (error 666)))))

(ert-deftest test-push-back ()
  "Test of `push-' macros"
  (let ((list (list 'a)))
    (should (equal (push-back 'b list) '(a b)))
    (should (equal list '(a b)))))

(ert-deftest test-pushnew-list ()
  "Test of `pushnew-list'"
  (let ((list '(c d e)))
    (should (equal (pushnew-list '(a b c) list) '(a b c d e)))
    (should (equal list '(a b c d e)))))

(ert-deftest test-ssymbol ()
  "Test of `ssymbol'"
  (should (equal (mapcar #'ssymbol
		   (list 1 0.3 "1" "0.3" "xcv" '(quote zxc) 'vbn))
		 '(1 0.3 1 0.3 xcv zxc vbn))))

(ert-deftest test-notf ()
  "Test of `notf'"
  (eval '(defun mb-test-notf ()
	  (lexical-let ((a t)
		(b '(t nil))
		(res nil))
	    (notf a)
	    (push (not a) res)
	    (notf (first b) (second b))
	    (push (equal b '(nil t)) res)
	    (push (not-null b) res)
	    (notf b)
	    (push (not b) res))))
  (lexical-let ((res (mb-test-notf))) 
    (should (apply #'all-true res))))

;; (defun mb-test-notf ()
;;   (let ((a t)
;; 	(b '(t nil))
;; 	(res nil))
;;     (notf a)
;;     (push (not a) res)
;;     (notf (first b) (second b))
;;     (push (equal b '(nil t)) res)
;;     (push (not-null b) res)
;;     (notf b)
;;     (push (not b) res)))
;;(mb-test-notf)

(provide 'test-mb-utils-div)
