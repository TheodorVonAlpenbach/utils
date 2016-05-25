(require 'mb-utils-div)

;;TODO: why should this be a list type?!
(defstruct (duration :named (:conc-name d-))
  (unit 4) ;1: whole-note; 2: half-note; 4: quarter-note;  etc where N denotes a base duration that is 1/N of a whole-note
  (value 1))

(defun* d-new (&optional (value 1) (unit 4))
  (make-duration :unit unit :value value))
;;(d-new)

(defun* d-copy (duration &key 
			 (unit (d-unit duration))
			 (value (d-value duration)))
  (make-duration :unit unit
		 :value value))
;;(d-copy (d-new))

(defconst duration-map-lilypond
  '((7.875 "1.....")
    (7.75 "1....")
    (7.5 "1...")
    (7 "1..")
    (6 "1.")
    (4 "1")
    (3.875 "2....")
    (3.75 "2...")
    (3.5 "2..")
    (3 "2.")
    (2 "2")
    (1.875 "4...")
    (1.75 "4..")
    (1.5 "4.")
    (1 "4")
    (0.875 "8..")
    (0.75 "8.")
    (0.5 "8")
    (0.375 "16.")
    (0.25 "16")))
;;(mapcar #'first duration-map-lilypond)

(defun d-time (duration)
  "Convert to an absolute time."
  (d-value duration))

(defun d-add (duration1 duration2)
  (when (/= (d-unit duration1)
	    (d-unit duration2))
    (error "Units are different in arguments: %S %S" duration1 duration2))
  (d-new (+ (d-value duration1)
	    (d-value duration2))
	 (d-unit duration1)))
;;(d-add (d-new 1) (d-new 2))

(defun dvalue-split-at-constant (dvalue max-length)
  "Helper function for dvalue-split-illegal-duration"
  (when (>= dvalue max-length)
    (- dvalue max-length)))
;;(mapcar (bind #'dvalue-split-at-constant 4) '(1 2 3 4 5 6 7 8 9 ))

(defun d-split-at-constant (duration max-length)
  "Helper function for dvalue-split-illegal-duration"
  (dvalue-split-at-constant (d-value duration) max-length))
;;(mapcar (bind #'d-split-at-constant 4) (mapcar #'d-new '(1 2 3 4 5 6 7 8 9 )))

(defun dvalue-split-illegal-duration (dvalue legal-durations-values)
  "Helper function for d-split-illegal-duration"
  (let ((res (member-if (bind #'<= dvalue) legal-durations-values)))
    (when res
      (cons (first res) 
	    (dvalue-split-illegal-duration 
	     (dvalue-split-at-constant dvalue (first res))
	     (rest res))))))
;;(dvalue-split-illegal-duration 2.5 (mapcar #'first duration-map-lilypond))

(defun d-split-illegal-duration (duration legal-durations-values)
  (dvalue-split-illegal-duration (d-value duration) legal-durations-values))
;;(d-split-illegal-duration (d-new 2.5) (mapcar #'first duration-map-lilypond))
;;(mvt-test)

(defun d-split-total (duration unit)
  "Splits duration into a list of durations, each with dvalue UNIT"
  (multiple-value-bind (length remainder)
      (floor* (d-value duration) unit)
    (let ((res (make-list length unit)))
      (if (zerop remainder)
	res (nconc res (list remainder))))))
;;(d-split-total (d-new 1.25) .25)

;;; read/write
(defun* d-to-string-lilypond (duration)
  (tmap-0-1 (d-value duration) duration-map-lilypond :test #'=))
;;(d-to-string-lilypond (make-duration))

(defun* d-to-string (duration &optional (print-style mu-default-print-style))
  (and duration 
       (case print-style
	 (lilypond (d-to-string-lilypond duration))
	 (otherwise (string-to-int (or (d-to-string-lilypond duration)
				       "0"))))))
;;(mapcar #'d-to-string (list nil (d-new 0) (d-new 1) (d-new 1 8)))

(defun* d-from-string-lilypond (duration-string &optional (print-style mu-default-print-style))
  (let* ((d-parts (split-string duration-string "\\."))
	 (unit 4.0)
	 (base-value (/ unit (string-to-int (first d-parts))))
	 (num-dots (1- (length d-parts)))
	 (value (* base-value
		   (1+ (- 1 (expt .5 num-dots))))))
    (d-new value unit))) ;always
;;(d-from-string-lilypond "4.")

(defun* d-from-string (duration-string &optional (print-style mu-default-print-style))
  (case print-style
    (lilypond (d-from-string-lilypond duration-string))
    (otherwise (error "Not implemented for style '%S'" print-style))))
;;(d-from-string "8." 'lilypond)

(provide 'duration)
