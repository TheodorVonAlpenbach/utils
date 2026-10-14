;;; test-mb-anaphora.el --- Tests for mb-anaphora -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-anaphora)

(ert-deftest mb-anaphora-aif ()
  (let ((calls 0))
    (should (= (aif (progn (cl-incf calls) 4)
                       (* it 2)
                     0)
               8))
    (should (= calls 1)))
  (should (eq (aif nil
                  'then
                'first-else
                'last-else)
              'last-else)))

(ert-deftest mb-anaphora-awhen ()
  (let ((body-ran nil))
    (should-not (awhen nil
                  (setq body-ran t)))
    (should-not body-ran))
  (should (= (awhen 4
               (1+ it))
             5)))

(ert-deftest mb-anaphora-acond ()
  (let ((tests 0))
    (should (equal (acond
                    ((progn (cl-incf tests) nil)
                     (ignore it)
                     'unreachable)
                    ((progn (cl-incf tests) "value") (concat it "!"))
                    ((progn (cl-incf tests) t)
                     (ignore it)
                     'too-late))
                   "value!"))
    (should (= tests 2)))
  (should-not (acond)))

(ert-deftest mb-anaphora-nested-bindings ()
  (should (equal (aif 2
                     (list it
                           (awhen 3 it)
                           it))
                 '(2 3 2))))

(ert-deftest mb-anaphora-asetf ()
  (let ((items (list (list 2) 1)))
    (should (equal (asetf (car items)
                          (list (* 3 (car it))))
                   '(6)))
    (should (equal items '((6) 1)))))

;;; test-mb-anaphora.el ends here
