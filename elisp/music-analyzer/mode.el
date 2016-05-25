(defun mode-scales-read-shortcut-definition (mode-scales-shortcut-definition)
  (unless (= (length (second mode-scales-shortcut-definition)) 7)
    (error "Definition '%S' does not contain 7 shortcut pitches" mode-scales-shortcut-definition))
  (list (first mode-scales-shortcut-definition)
	(loop for x in (second mode-scales-shortcut-definition)
	      for i from 0
	      collect (if (listp x)
			(loop for y in x collect (chrome-new i y))
			(chrome-new i x)))))
;;(mode-scales-read-shortcut-definition '(0 0 -1 0 0 (-1 0) (0 -1)))

(defconst mu-modes
  (mapcar #'mode-scales-read-shortcut-definition
	  '((major (0 0 0 0 0 0 0))
	    (ionian (0 0 0 0 0 0 0))
	    (minor (0 0 -1 0 0 (-1 0) (0 -1)))
	    (aeolian (0 0 -1 0 0 -1 (-1 0)))
	    (dorian (0 0 -1 0 0 0 (-1 0)))
	    (phrygian (0 -1 -1 0 0 -1 -1))
	    (lydian (0 0 0 1 0 0 0))
	    (mixolydian (0 0 0 0 0 0 (-1 0))))))

(defun* mode-scale (mode &optional (with-alterations t))
  "Returns the scale in MODE based on `chrome-new'. 

Iff WITH-ALTERATIONS is non nil then the possible alterations on
a scale step is included as a list of pitch classes, for example

\(mode-scale 'mixolydian t\) 

returns the pitch class tree (pitch classes being converted to
hfsymbols for clarity)

\(C D E F G A (Bb B) C\)"
  (if with-alterations
    (tmap-0-1 mode mu-modes)
    (loop for x in (mode-scale mode t)
	  collect (if (listp x) (first x) x))))
;;(maptree #'chrome-to-string (mode-scale 'aeolian t))

(defun m-leading-tone (mode)
 (let ((ln (nth 6 (mode-scale mode t))))
   (if (listp ln)
     (minimum ln #'> :key #'chrome-accidentals)
     ln)))
;;(chrome-to-string (m-leading-tone 'aeolian))

(defun m-intervals (mode)
  (loop for pc in (tmap-0-1 mode mu-modes)
	collect (p-new pc 0)))
;;(m-intervals 'aeolian)

(defun m-third (mode &optional picardy)
  "http://en.wikipedia.org/wiki/Picardy_third"
  (let ((third (nth 2 (m-intervals mode))))
    (if (and picardy 
	     (= (i-alteration third) -1))
      (i-nalterate third 1)
      third)))
;;(m-third 'aeolian t)

(defun m-third-p (interval mode &optional allow-picardy)
  "http://en.wikipedia.org/wiki/Picardy_third"
  (nth 2 (m-intervals mode)))

(defun* mode-from-string (mode-string &optional (print-style mu-default-print-style))
  (find (intern mode-string) (mapcar #'first mu-modes)))
;;(mapcar #'mode-from-string '("major" "minor" "dorian"))

(provide 'mu-mode)
