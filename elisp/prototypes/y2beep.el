(defconst +base-frequencies+ '(("C" 261.6)
			       ("C#" 277.2)
			       ("D" 293.7)
			       ("D#" 311.1)
			       ("E" 329.6)
			       ("F" 349.2)
			       ("F#" 370.0)
			       ("G" 392.0)
			       ("G#" 415.3)
			       ("A" 440.0)
			       ("A#" 466.2)
			       ("B" 493.9)
			       ("C" 523.2)))
;;(second (cl-assoc "D" +base-frequencies+ :test #'string=))

(defconst +base-tempo+ 1000
  "Milliseconds per beat")

;;; Lengths are like lilypond 1, 2, 4, 8, 16 etc
;;; We also include punctuations: 1.5, 3, 6, 12, 24 etc

;;; Scales are like ASCII, the table above is for the fourth octave

;;; For simplicity we put the octave before the pitch class
;;; 4A is «A4», i.e. frequency 400

(defun y2b-frequency (spitch)
  "Convert pitch symbol to frequency"
  (destructuring-bind (o pc) (split-at-position spitch 1)
    (round (* (second (cl-assoc pc +base-frequencies+ :test #'string=))
	      (expt 2 (- (string-to-integer o) 4))))))
;;(y2b-frequency "4A#")

(defun y2b-milliseconds (sduration)
  (round (* 4 +base-tempo+ (/ 1.0 (string-to-number sduration)))))
;;(mapcar #'y2b-milliseconds '("1" "1.5" "2" "4" "8" "16"))

(defun y2b (snotes)
  (concat* (loop for (spitch sduration) in (cut (split-string snotes))
		 for f = (y2b-frequency spitch)
		 for ms = (y2b-milliseconds sduration)
		 collect (format "-f %d -l %d" f ms))
    :pre "beep "
    :in " -n "))
;;(y2b "4G 3 4F 8 4E 4 4D 4 4C 4 4D 4 4E 4 4F 4 4G 6 4A 8 4G 4 4F 4 4E 1.5")
