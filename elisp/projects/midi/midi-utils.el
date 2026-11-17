;;; midi-utils.el --- MIDI encoding and decoding helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Conversion helpers for MIDI variable-length quantities.

;;; Code:

(require 'cl-lib)
(require 'mb-utils-io)
(require 'mb-utils-math)

(defun midi-read-variable-length-integer ()
  "Read a MIDI variable-length integer from the current hexl stream.
Return a list containing its decoded value and encoded byte count."
  (let ((bytes (cl-loop for byte = (read-byte) then (read-byte)
                       collect (mod byte 128)
                       until (< byte 128))))
    (list (cl-reduce (lambda (value byte) (+ (* value 128) byte))
                     bytes :initial-value 0)
          (length bytes))))

(defun midi-int-to-variable-length-quantity (integer)
  "Convert INTEGER to a MIDI variable-length quantity of at most four bytes."
  (last (int-to-variable-length-quantity integer) 4))

(provide 'midi-utils)

;;; midi-utils.el ends here
