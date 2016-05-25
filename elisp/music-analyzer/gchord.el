(require 'chord)
(require 'schord)

(defstruct (gchord (:conc-name gch-))
  (chord (schordx-new))	 ;note-pitch object
  (start-time 0)
  (duration (d-new 1)))
;;(make-gchord)

(defun* gch-new (&optional (chord (schordx-new)) (start-time 0) (duration (d-new 1)))
  (make-gchord :chord chord :start-time start-time :duration duration))
;;(gch-new)

(defun gchord-type (gchord)
  (when (listp (gch-chord gchord))
    (let ((x (first (gch-chord gchord))))
      (cond ((chrome-p x) 'chord)
	    ((integerp x) 'schord)
	    ((listp x) (if (symbolp (first x)) 'chordx 'schordx))))))
;;(gchord-type (gch-new (chordx-new)))

(defun* gch-copy (gchord &key 
			 (chord (case (gchord-type gchord)
				  (schordx (schordx-copy (gch-chord gchord)))
				  (chordx (chordx-copy (gch-chord gchord)))))
			 (start-time (gch-start-time gchord))
			 (duration (gch-duration gchord)))
  (gch-new chord start-time duration))
;;(gch-copy (gch-new) :duration 123)

(defun gch-dvalue (gchord)
  (d-value (gch-duration gchord)))
;;(gch-dvalue (gch-new))

(defun gch-end-time (gchord)
  (+ (gch-start-time gchord) (gch-dvalue gchord)))
;;(gch-end-time (gch-new))

(defun gch-chord-relation (type x y)
  "Returns the relation between chords X and Y (of TYPE). Helper
function for `gch-relation'"
  (case type
   (schordx (schordx-relation x y))
   (chordx (chordx-relation x y))))

(defun gch-relation (gchord1 gchord2)
  "Returns the relation between GCHORD1 and GCHORD2."
  (gch-chord-relation (gchord-type gchord1) (gch-chord gchord1) (gch-chord gchord2)))

(defun gch-remove-seventh (gchord)
  "Removes the seventh part of GCHORD. Non destructive"
  (gch-copy gchord :chord (case (gchord-type gchord)
			    (schordx (schordx-remove-seventh (gch-chord gchord))))))
;;(gchord-remove-seventh (gch-new (schordx-new '(1 4 7 10))))

(defun* gch-from-string (s &optional (type 'chordx) (style mu-default-print-style))
  "NB! This version doesn't parse START-TIME and DURATION.
GCHORDS is typically used with segmentations, so reading from
string is currently used in tests only"
  (gch-new (case type
	     (chordx (chordx-from-string s style))
	     (schordx (schordx-from-string s style)))))
;;(gch-from-string "Am" 'schordx)

(defun* gch-to-string (gchord &optional (print-style mu-default-print-style))
  "Prints gchord. See comment in `gch-from-string'"
  (case (gchord-type gchord)
    (chord (chord-to-string (gch-chord gchord) print-style))
    (chordx (chordx-to-string (gch-chord gchord) print-style))
    (schord (schord-to-string (gch-chord gchord) print-style))
    (schordx (schordx-to-string (gch-chord gchord) print-style))))
;;(mapcar #'gch-to-string (list (gch-new (chordx-new))))

(provide 'gchord)
