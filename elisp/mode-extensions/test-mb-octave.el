(require 'ert)
(require 'mb-octave)

(ert-deftest test-octave-buffers ()
"Test of `octave-buffers'"
 (should (cl-every #'octave-mode-p (octave-buffers))))

(provide 'test-mb-octave)
