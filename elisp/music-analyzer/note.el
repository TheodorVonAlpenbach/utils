;;;; A note represents a simple sound element from an unspecified simple sound source.
(require 'pitch)
(require 'duration)

(defstruct (note :named (:conc-name n-))
  (pitch (make-pitch))	 ;note-pitch object
  (start-time 0)
  (duration (d-new 1))   ;duration object
  (tied nil))		 ;boolean

;;other properties
;;start-dynamics (dynamics value at start)
;;end-dynamics
;;slur-begin
;;slur-end
;;frase-begin
;;frase-end
;;accents
;;glissando-start
;;etc
;;(make-note)

(defun* n-new (&optional (pitch (p-new)) (start-time 0.0) (duration (d-new 1)) (tied nil))
  (make-note :pitch pitch :start-time start-time :duration duration :tied tied))
;;(n-new (p-new 1) 1.5 (d-new 2) t)

(defun* n-copy (note &key
		     (pitch (n-pitch note))
		     (start-time (n-start-time note))
		     (duration (n-duration note))
		     (tied (n-tied note)))
  (make-note :pitch (p-copy pitch)
	     :start-time start-time
	     :duration (d-copy duration)
	     :tied tied))
;;(n-copy (n-new) :tied t)

(defun n-equal (x y)
  "A slight modification of plain `equal', with respect to
start-time (ignored) and ties"
  (and (equal (n-pitch x) (n-pitch y))
       (equal (n-duration x) (n-duration y))
       ;;accept x-x, x-x-, x x, but not x x-
       (or (n-tied x)
	   (not (n-tied y)))))
;;(n-equal (n-new) (n-new))
;;(equal (n-new) (n-new))

(defmacro n-dvalue (note)
  `(d-value (n-duration ,note)))
;;(let ((n (n-new)))  (setf (n-dvalue n) 123) n)

(defun n-chrome (note)
  (p-chrome (n-pitch note)))
(defsetf n-chrome (note) (chrome)
  `(setf (p-chrome (n-pitch ,note)) ,chrome))
;;(n-chrome (n-new))

(defun n-set-chrome (note)
  (message "This is now obsolete, use `n-chrome' instead")
  (p-set-chrome (n-pitch note)))

(defun n-dtime (note)
  "What's this?"
  (d-time (n-duration note)))

(defun n-end-time (n) (+ (n-start-time n) (n-dtime n)))
;;(n-end-time (n-new))

(defun* n-shift-start-time (n shift)
  (let ((new (copy-note n)))
    (setf (n-start-time new) (+ shift (n-start-time new)))
    new))
;;(n-shift-start-time (make-note) -1)

(defun n-interval (note1 note2)
  (p-interval (n-pitch note1) (n-pitch note2)))

(defun note= (n1 n2)
  (and (equal (n-pitch n1) (n-pitch n2))
       (= (n-start-time n1) (n-start-time n2))
       (equal (n-duration n1) (n-duration n2))
       (equal (n-tied n1) (n-tied n2))))

(defun n-alteration-p (note1 note2 &optional n)
  "Returns nil iff NOTE2 is an N-alteration of NOTE1. See
`chrome-alteration-p' for optional argument N."
  (p-alteration-p (n-pitch note1) (n-pitch note2) n))

(defun n-split-across-bars (note time-signature)
  "Splits a note that crosses a measure bar"
  (let* ((mp (mp-new (n-start-time note) time-signature))
	 (rem (mp-measure-remainder mp))
	 (d (n-duration note)))
    (when (> (d-value d) rem)
      (let ((n1 (copy-note note))
	    (n2 (copy-note note)))
	(setf (d-value (n-duration n1)) rem)
	(setf (d-value (n-duration n2)) (d-split (d-value d) rem))))))

(defun n-split-1 (note d-split)
  (let ((res (loop with st-orig = (n-start-time note)
		   for st = st-orig then (+ st d)
		   for d in d-split
		   for n = (n-copy note)
		   do (setf (d-value (n-duration n)) d)
		   do (setf (n-tied n) t)
		   do (setf (n-start-time n) st)
		   collect n)))
    ;;modify last note to be NOT tied
    (setf (n-tied (nth* -1 res)) nil)
    res))

(defun n-split (note d-split-function)
  "Splits a note that crosses a measure bar"
  (let ((d-split (funcall d-split-function (n-duration note))))
    (if (and d-split (and (> (length d-split) 1)))
      (n-split-1 note d-split)
      (list (n-copy note)))))
;;(mvt-submovement (mvt-test t) 14 15)
;;(movement-to-lilypond (mvt-submovement (mvt-test) 14 15) :title "Midi extraction" :file "c:/Documents and Settings/matsb/My Documents/data/ly/midi.ly" :start t :view nil)
;;(n-split (make-note :duration (d-new 2.5)) (bind #'d-split-illegal-duration (mapcar #'first duration-map-lilypond)))
;;(n-split (make-note :duration (d-new 2.5)) (bind #'d-split-at-constant 4))

;;; read and write
(defun* n-to-string (note &optional (print-style mu-default-print-style))
  (case print-style
    (lilypond (concat (p-to-string (n-pitch note) print-style) 
		      (d-to-string (n-duration note) print-style)
		      (if (n-tied note) " ~" "")))
    
    (english (format "%s (%S %S)%s" 
			  (p-to-string (n-pitch note) print-style)
			  (n-start-time note)
			  (d-to-string (n-duration note) print-style)
			  (if (n-tied note) "---" "")))
    (otherwise (error "Not implemented"))))
;;(n-to-string (make-note :tied t))

(defun* n-from-string-lilypond (note-string)
  (let* ((p-part (first (split-string note-string "[0-9._\()~-]")))
	 (p (and (string/= p-part "r")
		 (p-from-string p-part 'lilypond)))
	 (d-part (string-match* "[0-9]+\\.*" note-string))
	 (d (and (> (length d-part) 0) (d-from-string d-part 'lilypond)))
	 (tied-p (not-empty (string-match* "~" note-string))))
    (make-note :pitch p
	       :start-time nil
	       :duration d
	       :tied tied-p)))
;;(n-from-string-lilypond "c''4")

(defun* n-from-string (note-string &optional (print-style mu-default-print-style))
  (case print-style
    (lilypond (n-from-string-lilypond note-string))
    (otherwise (error "note-from-string doesn't support print-style '%S'" print-style))))
;;(note-from-string "sfe" 'lilypond)

(provide 'note)
