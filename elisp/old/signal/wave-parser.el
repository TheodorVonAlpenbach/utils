;;;; Extremely simple RIFF WAVE parser
;;;; See http://www.sonicspot.com/guide/wavefiles.html
(require 'bindat)
(require 'mb-utils-io) ;`read-byte-vector', `2s-complement-function'
(require 'mb-lists)    ;`transpose'

(defconst wave-file
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
    (:data-size       u32r)
    (:samples         repeat (eval (/ (bindat-get-field struct :data-size) 2)) (:sample u16r))))

;; Define setf-able property methods for all wave properties: wave-id, wave-size, etc
(defmacro define-wave-property-method (property)
  (with-gensyms (gsym gproperty)
    `(lexical-let ((,gsym (intern (format "wave-%s" (downcase (keyword-name ,property)))))
		   (,gproperty ,property)) 
       (defmacro ,gsym (wav) `(cdr (assoc ,,gproperty ,wav)))
       (defalias ,gsym (quote ,gsym)))))
(loop for x in (mapcar #'first wave-file) do (define-wave-property-method x))

(defun* wave-convert-samples (samples &key inverse)
  (loop with fn = (2s-complement-function 16 :inverse inverse)
	for x in samples do (asetf (cdr (car x)) (funcall fn it))))

(defun read-wave-file (file)
  (let ((wav (bindat-unpack wave-file (read-byte-vector file))))
    (wave-convert-samples (wave-samples wav))
    wav))

(defun write-wave-file (wav file)
  (let ((wav-copy (wave-copy wav)))
    (wave-convert-samples (wave-samples wav-copy) :inverse t)
    (write-byte-vector (bindat-pack wave-file wav) file)))

(provide 'wave-parser)
