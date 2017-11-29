(require 'ert)
(require 'mb-octave)

(ert-deftest test-texinfo-@fiy ()
  "Test of `andcat'"
  (should (equal (texinfo-@fiy "foo") "@foo{}"))
  (should (equal (texinfo-@fiy "foo" "bar") "@foo{bar}")))
