;;; utils
(defun midi-int-to-variable-length-quantity (integer)
  "Converts iNTEGER to a variable length quantity with max length of 4 bytes.
Note that #xFFFFFFF, the highest allowed variable integer, is
also the max integer in Emacs Lisp."
  (last (int-to-variable-length-quantity integer) 4)) ;;;;maximum length is 4 bytes) 
;;(midi-int-to-variable-length-quantity #x0FFFFFFF)
