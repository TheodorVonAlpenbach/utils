;;; test-mb-utils-geometry.el --- Tests for mb-utils-geometry -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-utils-geometry)

(ert-deftest mb-utils-geometry-one-dimensional-objects ()
  (let ((object (make-1d 10 20)))
    (should (= (1d-start object) 10))
    (should (= (1d-length object) 20)))
  (should (= (rest-space 3 10 100) 60))
  (should-error (rest-space -1 10 100)))

(ert-deftest mb-utils-geometry-align-evenly ()
  (should (equal (1d-align '((99 10) (42 20)) :world 90)
		 '((20.0 10) (50.0 20))))
  (should-not (1d-align nil :world 90)))

(ert-deftest mb-utils-geometry-align-fixed-margins ()
  (should (equal (1d-align '((0 10) (0 20))
			   :left-space 10 :right-space 20 :world 100)
		 '((10 10) (60.0 20))))
  (should (equal (1d-align '((0 20)) :left-space 10 :world 100)
		 '((10 20))))
  (should (equal (1d-align '((0 20)) :right-space 10 :world 100)
		 '((70.0 20))))
  (should-error
   (1d-align '((0 60)) :left-space 10 :right-space 20 :world 100))
  (should-error
   (1d-align '((0 80) (0 30)) :left-space 1 :world 100)))

(ert-deftest mb-utils-geometry-interspaces ()
  (should (equal (1d-interspaces 3 10 100)
		 '((10 20.0) (40.0 20.0) (70.0 20.0))))
  (should-error (1d-interspaces 0 10 100))
  (should-error (1d-interspaces 2 40 100)))

(ert-deftest mb-utils-geometry-transform-and-move ()
  (let ((object '(10 20)))
    (should (equal (1d-transform object 5 2) '(25 40)))
    (should (equal (1d-move object -3) '(7 20)))
    (should (equal object '(10 20))))
  (should-error (1d-transform '(10 20) 0 -1)))

(ert-deftest mb-utils-geometry-minmax ()
  (should (equal (minmax '(1 3 2 4 0)) '(0 4)))
  (should (equal (minmax '(2.5 -1.0 8)) '(-1.0 8)))
  (should-error (minmax nil)))

(ert-deftest mb-utils-geometry-bounding-box ()
  (should (equal (bounding-box '((0 0) (-1 3) (1 2)))
		 '((-1 1) (0 3))))
  (should (equal (bounding-box '((2 4))) '((2 2) (4 4))))
  (should-error (bounding-box nil))
  (should-error (bounding-box '((1 2 3)))))

(provide 'test-mb-utils-geometry)

;;; test-mb-utils-geometry.el ends here
