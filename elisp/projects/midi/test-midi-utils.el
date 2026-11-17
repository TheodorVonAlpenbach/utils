;;; test-midi-utils.el --- Tests for MIDI utilities -*- lexical-binding: t; -*-

(require 'ert)
(require 'midi-utils)

(ert-deftest midi-utils-variable-length-decoding ()
  "Test decoding MIDI variable-length quantities from a hexl stream."
  (dolist (case '(((0) 0 1)
                  ((127) 127 1)
                  ((#x81 0) 128 2)
                  ((#xff #xff #xff #x7f) #x0fffffff 4)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert (apply #'unibyte-string (car case)))
      (hexl-mode)
      (hexl-beginning-of-buffer 1)
      (should
       (equal (midi-read-variable-length-integer)
              (cdr case))))))

(ert-deftest midi-utils-variable-length-encoding ()
  "Test encoding MIDI variable-length quantities."
  (should (equal (midi-int-to-variable-length-quantity 0) '(0)))
  (should (equal (midi-int-to-variable-length-quantity 127) '(127)))
  (should (equal (midi-int-to-variable-length-quantity 128) '(#x81 0)))
  (should
   (equal (midi-int-to-variable-length-quantity #x0fffffff)
          '(#xff #xff #xff #x7f))))

(provide 'test-midi-utils)

;;; test-midi-utils.el ends here
