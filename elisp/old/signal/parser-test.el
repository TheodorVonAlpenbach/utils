;;;; Extremely simple RIFF WAVE parser
;;;; See http://www.sonicspot.com/guide/wavefiles.html
(require 'bindat)
(require 'mb-utils-io) ;`read-byte-vector', `2s-complement-function'
(require 'mb-lists)    ;`transpose'

(defconst wave-file-header
  '((:id              str 4)
    (:size            u32r)
    (:format          str 4)
    (:fmt-id          str 4)
    (:fmt-size        u32r)
    (:audio-format    u16r)
    (:num-channels    u16r)
    (:sample-rate     u32r)
    (:byte-rate       u32r)
    (:block-align     u16r)
    (:bits-per-sample u16r)
    (:extra-format    repeat (eval (if (zerop (- (bindat-get-field struct :fmt-size) 16)) 0 1)) (:foo u32r))
    (:data-id         str 4)
    (:data-size       u32r)))

;; Define setf-able property methods for all wave properties: wave-id, wave-size, etc
(defmacro define-wave-property-method (property)
  (with-gensyms (gsym gproperty)
    `(lexical-let ((,gsym (intern (format "wave-%s" (downcase (keyword-name ,property)))))
		   (,gproperty ,property)) 
       (defmacro ,gsym (wav) `(cdr (assoc ,,gproperty ,wav)))
       (defalias ,gsym (quote ,gsym)))))
(loop for x in (mapcar #'first wave-file-header) do (define-wave-property-method x))

(defun* wave-convert-samples (samples &key inverse)
  (loop with fn = (2s-complement-function 16 :inverse inverse)
	for x in samples do (asetf (cdr (car x)) (funcall fn it))))

(defun* bytes->u16r (data)
  (let ((res (make-vector (/ (length data) 2) 0)))
    (loop for i below (length data) by 2
	  for j from 0
	  for v = (logior (aref data i) (lsh (aref data (1+ i)) 8))
	  do (setf (aref res j) v))
    res))

(defun* u16r->bytes (ints &optional padding)
  (let ((res (make-vector (* (length ints) 2) 0)))
    (loop for i below (length res) by 2
	  for v in ints
	  for byte1 = (logand v 255)
	  for byte2 = (logand (lsh v -8) 255)
	  do (setf (aref res i) byte1)
	  do (setf (aref res (1+ i)) byte2))
    res))

(defun* wave-convert-data (data &key inverse)
  (mapcar (2s-complement-function 16 :inverse inverse) data))

(defun read-wave-file (file)
  (let* ((header (bindat-unpack wave-file-header (read-byte-vector file :to 200)))
	 (data (wave-convert-data (bytes->u16r (read-byte-vector file :from (+ 28 (wave-fmt-size header)))))))
    (cons (cons :samples data) header)))
;;(setf wav (read-wave-file "/cygdrive/c/Users/eier/Documents/MATLAB/lyder/ragtime.wav"))

(defun write-wave-file (wav file)
  (write-byte-vector (concat (bindat-pack wave-file-header (rest wav))
			     (u16r->bytes (wave-convert-data (wave-samples wav) :inverse t)))
		     file))
;;(write-wave-file wav "/cygdrive/c/Users/eier/Documents/MATLAB/lyder/ragtime3.wav")

(provide 'wave-parser)
