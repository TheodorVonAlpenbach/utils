;;; test-mb-utils-div.el --- Tests for mb-utils-div -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-utils-div)

(ert-deftest mb-utils-div-all-equal ()
  "Test of `all-equal'"
  (should (equal (all-equal) t))
  (should (equal (all-equal 1) t))
  (should (equal (all-equal 1 1) t))
  (should (equal (all-equal 1 2) nil))
  (should (equal (all-equal 2 2 2) t))
  (should (equal (all-equal 1 2 2) nil)))

(ert-deftest mb-utils-div-equal-elements ()
  "Test of `equal-elements'"
  (should (equal (equal-elements []) t))
  (should (equal (equal-elements [1]) t))
  (should (equal (equal-elements [1 1]) t))
  (should (equal (equal-elements [1 2]) nil))
  (should (equal (equal-elements [2 2 2]) t))
  (should (equal (equal-elements [1 2 2]) nil)))

(ert-deftest mb-utils-div-push-list ()
  "Test of `push-list'"
  (let ((list '(a))
	(prefix '(pre fix)))
    (should (equal (push-list '() list) '(a)))
    (should (equal list '(a)))
    (should (equal (push-list prefix list) '(pre fix a)))
    (should (equal list '(pre fix a)))
    (should (equal prefix '(pre fix)))))

(ert-deftest mb-utils-div-push-back-list ()
"Test of `push-back'"
   (let ((list (list 'a))
	 (suffix '(suf fix)))
    (should (equal (push-back-list '() list) '(a)))
    (should (equal list '(a)))
    (should (equal (push-back-list suffix list) '(a suf fix)))
    (should (equal list '(a suf fix)))
    (should (equal suffix '(suf fix)))))

(ert-deftest mb-utils-div-last-elt ()
"Test of `last-elt'"
 (should (equal (last-elt '(a b c) 0) 'c))
 (should (equal (last-elt '(a b c) 1) 'b))
 (should (equal (last-elt '(a b c)) 'c)))

(ert-deftest mb-utils-div-nor ()
  "Test of `nor'"
  (should (nor))
  (should (nor nil))
  (should (nor nil nil nil))
  (should-not (nor nil t))
  (should-not (nor nil t (error "Not visible")))
  (should (equal 666 (condition-case nil
			 (nor nil nil (error "Visible"))
		       (error 666)))))

(ert-deftest mb-utils-div-nand ()
  "Test of `nand'"
  (should-not (nand)) ;; empty argument list -> no argument is nil!
  (should (nand nil))
  (should (nand t nil))
  (should (nand t nil (error "Not visible")))
  (should (equal 666 (condition-case nil
			 (nand t t (error "Visible"))
		       (error 666)))))

(ert-deftest mb-utils-div-push-back ()
  "Test of `push-' macros"
  (let ((list (list 'a)))
    (should (equal (push-back 'b list) '(a b)))
    (should (equal list '(a b)))))

(ert-deftest mb-utils-div-pushnew-list ()
  "Test of `pushnew-list'"
  (let ((list '(c d e)))
    (should (equal (pushnew-list '(a b c) list) '(a b c d e)))
    (should (equal list '(a b c d e)))))

(ert-deftest mb-utils-div-ssymbol ()
  "Test of `ssymbol'"
  (should (equal (mapcar #'ssymbol
		   (list 1 0.3 "1" "0.3" "xcv" '(quote zxc) 'vbn
			 (list 1 0.3 "1" "0.3" "xcv" '(quote zxc) 'vbn)))
		 '(1 0.3 1 0.3 xcv zxc vbn (1 0.3 1 0.3 xcv zxc vbn)))))

(ert-deftest mb-utils-div-notf ()
  "Test of `notf'"
  (let ((a t)
	(b (list t nil)))
    (notf a)
    (should-not a)
    (notf (cl-first b) (cl-second b))
    (should (equal b '(nil t)))
    (notf b)
    (should-not b)))

(ert-deftest mb-utils-div-plist-delete ()
  "Test of `plist-delete'"
 (should (equal (plist-delete '(:qwe qwe :ewq ewq) :ewq) '(:qwe qwe)))
 (should (equal (plist-delete '(:qwe qwe :ewq ewq) :qwe) '(:ewq ewq)))
 (should (equal (plist-delete '(:qwe qwe :ewq ewq :asd asd) :ewq) '(:qwe qwe :asd asd))))

(ert-deftest mb-utils-div-plist-pop ()
  "Test of `plist-pop'"
  (should (equal (let ((plist '(:qwe 1 :ewq 2)))
		   (list (plist-pop plist :ewq) plist))
		 '(2 (:qwe 1))))
  (should (equal (let ((plist '(:qwe 1 :ewq 2)))
		   (list (plist-pop plist :qwe) plist))
		 '(1 (:ewq 2))))
  (should (equal (let ((plist '(:qwe nil :ewq 2)))
		   (list (plist-pop plist :qwe) plist))
		 '(nil (:ewq 2)))))

(ert-deftest mb-utils-div-pop-until ()
  (let ((items '(1 2 3 4)))
    (should (equal (pop-until items 3) '(1 2)))
    (should (equal items '(3 4))))
  (let ((items '(1 2 3)))
    (should-not (pop-until items))
    (should (equal items '(1 2 3)))))

(ert-deftest mb-utils-div-modify-if ()
  "Test of `modify-if'"
  (should (equal (modify-if 1 #'zerop 'zero) 1))
  (should (equal (modify-if 0 #'zerop 'zero) 'zero)))

(ert-deftest mb-utils-div-listify ()
  "Test of `listify'"
  (should (equal (listify "a") '("a")))
  (should (equal (listify 'a) '(a)))
  (should (equal (listify (list 1 2 3)) (list 1 2 3)))
  (let ((function (lambda () nil)))
    (should (equal (listify function) (list function))))
  (should (equal (listify [1 2 3]) (list 1 2 3))))

(ert-deftest mb-utils-div-vectorify ()
  "Test of `vectorify'"
  (should (equal (vectorify "a") ["a"]))
  (should (equal (vectorify 'a) [a]))
  (should (equal (vectorify '(1 2 3)) [1 2 3]))
  (should (equal (vectorify [1 2 3]) [1 2 3])))

(ert-deftest mb-utils-div-apply-if ()
  "Test of `apply-if'"
  (should (equal (apply-if (1+ 1) (cl-oddp 1)) 2))
  (should (equal (apply-if (1+ 1) (cl-oddp 1) #'1+) 3))
  (should (equal (apply-if (1+ 1) (cl-oddp 1) #'1+ #'1+) 4))
  (should (equal (apply-if (1+ 1) (cl-evenp 1)) 2))
  (should (equal (apply-if (1+ 1) (cl-evenp 1) #'1+) 2))
  (should (equal (apply-if (1+ 1) (cl-evenp 1) #'1+ #'1+) 2)))

(ert-deftest mb-utils-div-bind-star ()
  (should (equal (funcall (bind* #'list 'a 0) 'x)
		 '(a x)))
  (should (equal (funcall (bind* #'list '(a b) '(0 2)) 'x 'y)
		 '(a x b y)))
  (should (equal (funcall (bind* #'list '(a b)) 'x)
		 '(x a b))))

(ert-deftest mb-utils-div-iintern ()
  (should-not (iintern ""))
  (should (eq (iintern "alpha") 'alpha))
  (should (= (iintern "-12") -12))
  (should (eq (iintern 'alpha) 'alpha)))

(defun mb-utils-div-test--signature (required &optional optional &rest rest)
  (list required optional rest))

(ert-deftest mb-utils-div-function-args ()
  (should (equal (function-signature 'mb-utils-div-test--signature)
		 '(required &optional optional &rest rest)))
  (should (equal (function-args 'mb-utils-div-test--signature nil)
		 '(required)))
  (should (equal (function-args 'mb-utils-div-test--signature '&optional)
		 '(optional)))
  (should (equal (function-args 'mb-utils-div-test--signature '&rest)
		 '(rest)))
  (should (equal (function-args 'mb-utils-div-test--signature '&key)
		 nil)))

(ert-deftest mb-utils-div-min-max-and-push-unique ()
  (should (= (min* 3 1 2) 1))
  (should (= (max* 3 1 2) 3))
  (let ((container (list '(b))))
    (should (equal (push-unique 'a (cl-first container)) '(a b)))
    (should (equal (push-unique 'a (cl-first container)) '(a b)))
    (should (equal container '((a b))))))

(provide 'test-mb-utils-div)

;;; test-mb-utils-div.el ends here
