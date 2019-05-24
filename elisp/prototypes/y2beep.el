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

(defun y2b-frequency (soctave spc)
  "Convert pitch symbol to frequency"
  (awhen (cl-assoc (upcase spc) +base-frequencies+ :test #'string=)
    (round (* (second it)
	      (expt 2 (- (string-to-integer soctave) 4))))))
;;(y2b-frequency "5" "C")
;;(loop for o from 3 to 6 collect (y2b-frequency (number-to-string o) "A"))
;;(loop for o from 3 to 6 collect (y2b-frequency (number-to-string o) "R"))

(defun y2b-geometric-series (base end)
  (loop for i from 0 to end sum (expt base i)))
;;(y2b-geometric-series 1 1)

(cl-defun y2b-parse-sduration (sduration)
  (aif (position ?. sduration)
    (list (string-to-integer (substring sduration 0 it))
	  (- (length sduration) it))
    (list (string-to-integer sduration) 0)))
;;(mapcar #'y2b-parse-sduration '("4" "4.."))

(cl-defun y2b-milliseconds (sduration &optional (tempo 60))
  (destructuring-bind (bd dots) (y2b-parse-sduration sduration)
      (round (/ (* 4 +base-tempo+ 60 (y2b-geometric-series 0.5 dots))
		bd tempo))))
;;(mapcar (bind #'y2b-milliseconds 480) '("1" "1.5" "2" "4" "8" "16"))
;;(y2b-milliseconds "1.")

(defun y2b-parse-spitch (spitch)
  (if (string-match "[0-9]" (substring spitch 0 1))
    (split-at-position spitch 1)
    (list nil spitch)))
;;(mapcar #'y2b-parse-spitch '("5C" "4C#" "C" "C#"))

(defun y2b-parse-snote (snote)
  (aif (string-match "[0-9]\\.*$" snote)
    (destructuring-bind (soctave spc)
	(y2b-parse-spitch (substring snote 0 it))
      (list soctave spc (substring snote it)))
    (append (y2b-parse-spitch snote) (list nil))))
;;(mapcar #'y2b-parse-snote '("C4." "5C8" "4C2" "4C#2" "C2" "C#2" "4C" "4C#" "C" "C#"))
;;(string-match "[0-9]\\.*$" "5C4.")

(cl-defun y2b-1 (svoice tempo transpose)
  (loop for snote in (split-string svoice)
	for (soctave spc sduration) = (y2b-parse-snote snote)
	for last-sduration = (or sduration "4") then (or sduration last-sduration)
	for last-soctave = (or soctave "4") then (or soctave last-soctave)
	for f = (y2b-frequency last-soctave spc)
	for ms = (y2b-milliseconds last-sduration tempo)
	collect (if f
		  (format "-f %d -l %d" f ms)
		  (format "-D %d" ms))))
;;(y2b-1 "5C8 R4  4G8 F#" 240 1)

(cl-defun y2b (svoice &key (tempo 60) transpose tag)
  (concat* (loop for (a b) in (pairs (append (y2b-1 svoice tempo transpose) '("dummy")))
		 if (not (string= (substring a 0 2) "-D"))
		 collect (if (string= (substring b 0 2) "-D")
			   (format "%s %s" a b)
			   a))
    :pre "beep "
    :in " -n "))

(defun y2b-join-delay (ds)
  (format "-D %d"
    (loop for d in ds
	  sum (string-to-number (second (split-string d))))))
;;(y2b-join-delay '("-D 10" "-D 5"))

(cl-defun y2b (svoice &key (tempo 60) transpose tag)
  (concat* (loop for (n . ds) in (group (y2b-1 svoice tempo transpose)
				   :test #'(lambda (x y)
					     (string= (substring y 0 2) "-D")))
		 collect (if ds (concat n " " (y2b-join-delay ds)) n))
    :pre "beep " :in " -n "))
;;(y2b "5C8 R4  4G8 F# G  G#3  G8 R4  R3  B4 R8  5C8 R2" :tempo 480 :tag "Shave And A Haircut")
;;(y2b "4G4 5C C D8 E F D E4 D8 E F4 E D8 C D4 C2" :tempo 480 "Het Wilhelmus")
;;(y2b "4G3 F8 E4 D C D E F G6 A8 G4 F E2" "Ja, vi elsker")
;;(y2b "5c8 4b 5c r 4c r c r g f e g 5c 4b 5c e d c d r 4d r d r" :tempo 120 "Sea song")
;;(insert (y2b "4g4 r8 g8 a2 g 5c 4b2 r4 4g4 r8 g8 a2 g 5d c2 r4 4g4 r8 g8 5g2 e c 4b a 5f4 r8 f8 e2 c2 d c1" :tempo 240 :tag "Happy Birthday"))
(provide 'y2beep)

(defun ly2b (m env)
  "ENV is tempo, relative pitch, duration. Defaults are 60 bpm, c4, 4, respectively.
Note is (FREQUENCY DURATION). A rest is (nil DURATION)."
  (case :t
    ()))

;;(ly2b '(:t (60 120) (:r c5 (c8 b c r c- r c r g+ f e g c b c e d c d r d- r d))))
