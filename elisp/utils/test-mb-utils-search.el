;;; test-mb-utils-search.el --- Tests for mb-utils-search -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-utils-search)

(ert-deftest mb-utils-search-current-buffer ()
  (with-temp-buffer
    (insert "alpha beta gamma")
    (goto-char 4)
    (should (= (exists-match-p "beta") 11))
    (should (= (point) 4))
    (should-not (exists-match-p "delta"))))

(ert-deftest mb-utils-search-designated-buffer ()
  (with-temp-buffer
    (let ((buffer (generate-new-buffer " *mb-utils-search*")))
      (unwind-protect
	  (progn
	    (insert "current only")
	    (with-current-buffer buffer
	      (insert "target")
	      (goto-char 3))
	    (should (= (exists-match-p "target" buffer) 7))
	    (should (= (with-current-buffer buffer (point)) 3))
	    (should-not (exists-match-p "target")))
	(kill-buffer buffer)))))

(ert-deftest mb-utils-search-respects-narrowing ()
  (with-temp-buffer
    (insert "outside\ninside\noutside")
    (narrow-to-region 9 15)
    (should (= (exists-match-p "inside") 15))
    (should-not (exists-match-p "outside"))))

(ert-deftest mb-utils-search-preserves-match-data ()
  (with-temp-buffer
    (insert "old target")
    (string-match "\\(old\\)" "old")
    (let ((before (match-data)))
      (should (exists-match-p "target"))
      (should (equal (match-data) before))))
  (with-temp-buffer
    (should-error (exists-match-p "["))))

(provide 'test-mb-utils-search)

;;; test-mb-utils-search.el ends here
