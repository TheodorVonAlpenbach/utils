;;; rot47.el --- display a buffer in ROT47  -*- lexical-binding: t -*-
(require 'rot13)

(defconst rot47-translate-table
  (let ((table (copy-sequence rot13-translate-table)))
    (cl-loop for i from 33 below 80
      do (aset table i (+ i 47)))
    (cl-loop for i from 80 below 127
      do (aset table i (- i 47)))
    table)
  "String table for ROT47 translation.
See `rot47-region` for the definition of ROT47")

(defun rot47-region (start end)
  "Rotate through all 47 printable ASCII characters,
except SPACE."
  (interactive "r")
  (translate-region start end rot47-translate-table))
  
(defun rot47-string (string)
  "Return ROT47 encryption of STRING."
  (with-temp-buffer
    (insert string)
    (rot47-region (point-min) (point-max))
    (buffer-string-no-properties)))
;;(rot47-string "Q")

(defun rot47 (object &optional start end)
  "ROT47 encrypt OBJECT, a buffer or string.
If OBJECT is a buffer, encrypt the region between START and END.
If OBJECT is a string, encrypt it in its entirety, ignoring START
and END, and return the encrypted string."
  (if (bufferp object)
      (with-current-buffer object
	(rot47-region start end))
    (rot47-string object)))
;;(rot47 "-Q")
;;(string-to-clipboard (rot47 "O.AC.Q?tf0nn.fa"))

(provide 'rot47)
