(require 'ert)
(require 'mb-octave)

(ert-deftest test-octave-buffers ()
"Test of `octave-buffers'"
 (should (every #'octave-mode-p (octave-buffers))))

(ert-deftest test-texinfo-@fiy ()
  "Test of `andcat'"
  (should (equal (texinfo-@fiy "foo") "@foo{}"))
  (should (equal (texinfo-@fiy "foo" "bar") "@foo{bar}")))
