(require 'ert)
(require 'mb-texinfo)

(ert-deftest test-texinfo-atfiy ()
  "Test of `texinfo-atfiy'"
  (should (equal (texinfo-atfiy "foo") "@foo{}"))
  (should (equal (texinfo-atfiy "foo" "bar") "@foo{bar}")))

(provide 'test-mb-texinfo)
