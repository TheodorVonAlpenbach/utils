(require 'mb-utils-strings)
(require 'chrome)
(require 'yamal-octave)

;; base is a pitch in one of the base-chromees: (0 2 4 5 7 9 11) (ie. (C D E F G A B))
;; accidentals is a signed integer where the absolute value indicates the number of accidentals
;; if negative, number of flats
;; if positive, number of sharps
;; and, consequently, if zero, no accidentals

(defstruct (pitch :named (:conc-name p-))
  (chrome (make-chrome))
  (octave 4))
;;(p-chrome (make-pitch) 'lilypond)
;;(p-to-string (make-pitch) 'lilypond)

(defun* p-new (&optional (chrome (chrome-new)) (octave 4))
  (make-pitch :chrome chrome :octave octave))
;;(p-new)

(defun* p-copy (pitch &key 
		      (chrome (p-chrome pitch))
		      (octave (p-octave pitch)))
  (make-pitch :chrome (chrome-copy chrome)
	      :octave octave))
;;(p-copy (p-new))

(defun p-set-chrome (pitch)
  (spc-to-chrome (p-chrome pitch)))

(defun* p-ansi (pitch &optional (base-octave 0))
  (+ (* (- (p-octave pitch) base-octave) 12)
     (chb-to-spc (chrome-base (p-chrome pitch)))
     (chrome-accidentals (p-chrome pitch))))
;;(p-ansi (p-new)) ==> 48
;;midi:
;;(p-ansi (p-new) -1) ==> 60

(defun p< (pitch1 pitch2)
  (< (p-ansi pitch1) (p-ansi pitch2)))
;;(p< (p-new) (p-new (chrome-new 1)))

(defun p> (pitch1 pitch2) (p< pitch2 pitch1))
(defun p<= (pitch1 pitch2) (not (p< pitch2 pitch1)))
(defun p>= (pitch1 pitch2) (p<= pitch2 pitch1))
(defun p= (pitch1 pitch2) (equal pitch1 pitch2))
(defun p/= (pitch1 pitch2) (not (p= pitch1 pitch2)))
;;(p= (p-new) (p-new (chrome-new 0)))

(defun* p+ (pitch1 pitch2 &optional (n 1))
  (let* ((res-ansi (+ (p-ansi pitch1)
		      (* n (p-ansi pitch2))))
	 (res-pbc* (+ (chrome-base (p-chrome pitch1))
		      (* n (chrome-base (p-chrome pitch2)))))
	 (res-pbc (mod res-pbc* 7))
	 (res-octave (+ (p-octave pitch1)
			(* n (p-octave pitch2))
			(floor res-pbc* 7)))
	 (res* (p-new (chrome-new res-pbc) res-octave))
	 (res*-ansi (p-ansi res*))
	 (res-accidentals (- res-ansi res*-ansi)))
    (setf (chrome-accidentals (p-chrome res*)) res-accidentals)
    res*))
;;(p-transpose (p-from-string "A#4") (p-from-string "G#4") -1)

(defun* p-transpose (pitch interval &optional (n 1))
  (p+ pitch interval n))
;;(p-transpose (p-from-string "F4") (i-from-symbol 'M2))

(defun p- (pitch1 pitch2)
  (p+ pitch1 pitch2 -1))
;;(p- (p-from-string "F4") (p-from-string "B2"))

(defun p-interval (pitch1 pitch2)
  (p- pitch2 pitch1))
;;(p-interval (p-from-string "B2") (p-from-string "F4"))

(defun p-nalterate (pitch alteration)
  "See `chrome-nalterate'"
  (chrome-nalterate (p-chrome pitch) n))

(defun p-alterate (pitch n)
  "See `chrome-alterate'"
  (p-nalterate (p-copy pitch) n))

(defun p-alteration-p (pitch1 pitch2 &optional n)
  "Returns nil iff PITCH2 is an N-alteration of PITCH1. See
`chrome-alteration-p' for optional argument N."
  (chrome-alteration-p (p-chrome pitch1) (p-chrome pitch2) n))
;;(p-alteration-p (p-from-string "F4") (p-from-string "F#4"))

;;; read/write
(defun* p-to-string (pitch &optional (print-style mu-default-print-style))
  (if (listp pitch)
    (concat* (mapcar (bind #'p-to-string print-style) pitch) :pre "[" :in " " :suf "]")
    (case print-style
      (norwegian (error "Not implemented"))
      (otherwise (concat (chrome-to-string (p-chrome pitch) print-style)
			 (octave-to-string (p-octave pitch) print-style))))))
;;(p-to-string (mapcar #'p-from-string '("C1" "C#1" "G1")))

(defun* p2-to-string (pitch-pair &optional (print-style mu-default-print-style))
  (concat* (mapcar (bind #'p-to-string print-style) pitch-pair) :pre "[" :in " " :suf "]"))

(defun p-from-string-lilypond (p-string)
  (let ((o-pos (or (position-if #'(lambda (x) (member x '(?' ?,))) p-string)
		   (length p-string))))
    (p-new (chrome-from-string (substring p-string 0 o-pos) 'lilypond)
	   (octave-from-string (substring p-string o-pos) 'lilypond))))
;;(p-from-string-lilypond "c,")

(defun* p-from-string (p-string &optional (print-style mu-default-print-style))
  "Reads a pitch string on the ANSI (?) format.
TODO: what is this format actually called?
TODO: MIDI format should not store pitch as a string, but as an integer"
  (case print-style
    ('lilypond (p-from-string-lilypond p-string))
    (otherwise (let ((pc (chrome-from-string (substring* p-string 0 -1)))
		     (o (octave-from-string (substring* p-string -1))))
		 (p-new pc o)))))
;;(p-from-string "Eb1")
;;(p-from-string "c''" 'lilypond)

(provide 'pitch)
