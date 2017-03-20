(require 'ert)
(require 'mb-lists)

(ert-deftest test-subdirs ()
  "Test of `subdirs'"
  (let ((subdirs (subdirs "./")))
    (should (equal subdirs nil))))

