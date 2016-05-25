(require '4-lilypond)
(require '4-chord)

(defun test-choral ()
  (4-part-choral-analysis-format (4-part-lilypond-parse-choral (test-score)) 8))
;;(test-choral)

;;note and interval utils
;;sn: string note, eg. "a,,,", "a'" for 1A, a2
;;n: lilypond-note, but only used as note, not duration; also enharmonics are equal eis equals f
;;iv: interval, based on n
;;voices are called soprano, alto, tenor, bass when appropriate; otherwise referred to as 0 1 2 3
;;v: voice
(defun read-note (sn) (4-part-lilypond-parse-voice-token sn))

(defun octave< (n1 n2) (< (lnote-pitch n1) (lnote-pitch n2)))
(defun octave= (n1 n2) (= (lnote-pitch n1) (lnote-pitch n2)))
(defun octave- (n1 n2) (- (lnote-pitch n1) (lnote-pitch n2)))
(defun octave<= (n1 n2) (or (octave< n1 n2) (octave= n1 n2)))

(defun diatonic-order< (n1 n2) (< (diatonic-order n1) (diatonic-order n2)))
(defun diatonic-order= (n1 n2) (= (diatonic-order n1) (diatonic-order n2)))
(defun diatonic-order- (n1 n2) (- (diatonic-order n1) (diatonic-order n2)))

(defun n< (n1 n2) (or (octave< n1 n2) 
		      (and (octave= n1 n2)
			   (diatonic-order< n1 n2))))
(defun n= (n1 n2) (and (octave= n1 n2) (diatonic-order= n1 n2)))
(defun n<= (n1 n2) (not (n< n2 n1)))
(defun n/= (n1 n2) (not (n= n1 n2)))
(defun n- (n1 n2) (+ (diatonic-order- n1 n2) (* 12 (octave- n1 n2))))
;;(n- (read-note "b''") (read-note "d'"))

(defun make-interval (n1 n2) (list n1 n2))
(defun read-interval (sn1 sn2) (make-interval (read-note sn1) (read-note sn2)))
(defun iv-l (iv) (first iv))
(defun iv-h (iv) (second iv))
(defun within-iv (n iv) (and (n<= (iv-l iv) n) (n<= n (iv-h iv))))
(defun iv-length (iv) (n- (iv-h iv) (iv-l iv)))

;;chorals (CHs) are all in analysis format (with subtactus etc)
(defmacro c-pitch (ch) `(first ,ch))
(defmacro c-key (ch) `(second ,ch))
(defmacro c-measure (ch) `(third ,ch))
(defmacro c-rythm (ch) `(first (c-measure ,ch)))
(defmacro c-tactus (ch) `(second (c-measure ,ch)))
(defmacro c-partial (ch) `(fourth ,ch))
(defmacro c-subtactus (ch) `(fifth ,ch))
(defmacro c-voices (ch) `(sixth ,ch))
(defmacro c-soprano (ch) `(c-voices (first ,ch)))
(defmacro c-alto (ch) `(c-voices (second ,ch)))
(defmacro c-tenor (ch) `(c-voices (third ,ch)))
(defmacro c-bass (ch) `(c-voices (fourth ,ch)))
(defun c-flatten (ch) (mapcar #'flatten (c-voices ch))) ;;obsolete?
(defun c-lchords (ch) (apply #'mapcar* #'list (c-flatten ch)))
(defun c-lchord (ch position) (nth (c-nth-subtactus ch position) (c-lchords ch)))
(defun c-chords (ch) (mapcar #'lchord-to-chord (c-lchords ch)))
(defun c-beats/bar (ch) (c-rythm ch))
(defun c-subbeats/beat (ch) (/ (c-subtactus ch) (c-tactus ch)))
(defun c-subbeats/bar (ch) (* (c-beats/bar ch) (c-subbeats/beat ch)))
(defun c-key-note (ch) (read-note (first (c-key ch))))
(defun c-key-mode (ch) (string-case (second (c-key ch)) ("major" 'major) ("minor" 'minor)))
;;(c-key-mode (test-choral))

(defun c-copy (ch) (copy-tree ch))

(defun c-position (ch subtactus-i)
  "Returns note position as a list (bar-number beat-number subbeat-number)"
  ;;The intial calculations are 0-based, ie. first bar number is 0 (not 1)
  (let* ((subtactus (c-subtactus ch))
	 (partial (c-partial ch))
	 (subbeats-partial (if partial (/ subtactus partial) 0))
	 (subtactus-i* (- subtactus-i subbeats-partial))
	 (tactus (c-tactus ch))
	 (subbeats/beat (/ subtactus tactus))
	 (bar-number (if (< subtactus-i* 0) -1 (/ subtactus-i* subtactus)))
	 (bar-subtactus (mod subtactus-i* subtactus))
	 (beat-number (/ bar-subtactus subbeats/beat))
	 (subbeat-number (mod bar-subtactus subbeats/beat)))
    (list (1+ bar-number) (1+ beat-number) (1+ subbeat-number))))
;;(c-position (test-choral) 2)

(defun c-nth-subtactus (ch position)
  "Position is a list (bar &optional (beat 1) (subbeat 1)). Or
position is a number, then it refers directly to the nth
subtactus."
  (if (listp position)
    (let* ((bar (1- (first position)))	;convert to zero based
	   (beat (1- (or (second position) 1)))
	   (subbeat (1- (or (third position) 1)))
	   (partial (c-partial ch))
	   (subbeats-partial (if partial (/ (c-subtactus ch) partial) 0)))
      (+ subbeats-partial
	 (* bar (c-subbeats/bar ch))
	 (* beat (c-subbeats/beat ch))
	 subbeat))
    position))
;;(c-nth-subtactus (test-choral) '(2 2 2))
;;(c-nth-subtactus (test-choral) 3)

(defun c-extract (ch from to)
  (let ((from-subtactus (c-nth-subtactus ch from)) ;;change this later
	(to-subtactus (c-nth-subtactus ch to))
	(ch-extract (c-copy ch)))
    (setf (c-voices ch-extract) 
	  (mapcar #'(lambda (x) (subseq x from-subtactus to-subtactus))
		  (c-voices ch-extract)))
    ch-extract))
;;(c-extract (load-choral "c:/Users/Theodor/Documents/projects/UiO/MUS-1221/Mappeoppgave-3/Mappeoppgave-3.ly") '(4) '(5))


;;(apply #'and* (list t t t t))

;;rules
(defconst ambitus-soprano-typical (read-interval  "c'" "a''"))
(defconst ambitus-alto-typical (read-interval  "g" "d''"))
(defconst ambitus-tenor-typical (read-interval "c" "g'"))
(defconst ambitus-bass-typical (read-interval  "f," "d'"))
(defconst ambitus-typical (list ambitus-soprano-typical ambitus-alto-typical ambitus-tenor-typical ambitus-bass-typical))

(defconst voice-names '("soprano" "alto" "tenor" "bass"))
;;todo define contruct DEFRULE 

;;todo define with-voice, with-notes, with-chords, with-chord-pairs

(defun rule-ambitus-typcial (n v)
  (within-iv n (nth v ambitus-typical)))

(defun voice-name (v &optional long-format-p)
  (let ((voice-name (nth v voice-names)))
    (if long-format-p voice-name (substring voice-name 0 1))))

(defun describe-note (v nth-tactus nth-subtactus tactus subtactus &optional long-format-p)
  (let ((bar (/ nth-tactus tactus))
	(beat (mod nth-tactus tactus)))
    (if long-format-p
      (format "For %s, in bar %d, on beat %d at subbeat %d: " (voice-name v long-format-p) bar beat nth-subtactus)
      (format "%s|%d|%d:%d: " (voice-name v long-format-p) bar beat nth-subtactus))))

;; traversing utils
(defun do-intervals (lchord1 lchord2 interval-function)
  (loop for v1 below 4
	do (loop for v2 from (1+ v1) below 4
		 for iv1 = (chord-interval lchord1 v1 v2)
		 for iv2 = (chord-interval lchord2 v1 v2)
		 do (funcall interval-function iv1 iv2 v1 v2))))

(defun do-lchords (ch lchord-function)
  (let ((lchords (c-lchords ch)))
    (loop for lchord1 in lchords
	  for lchord2 in (rest lchords)
	  for subtactus-i from 1
	  do (funcall lchord-function lchord1 lchord2))))

(defvar my-rules
  () ;;note-rules ;;report format
  )

(defconst intervals
  '((0 perfect-unison unison)
    (1 minor-second second)
    (2 major-second second)
    (3 minor-third third)
    (4 major-third third)
    (5 perfect-fourth fourth)
    (6 tritone n/a)
    (6 augmented-fourth fourth)
    (6 diminished-fifth fifth)
    (7 perfect-fifth fifth)
    (8 augmented-fifth fifth)
    (8 minor-sixth sixth)
    (9 major-sixth sixth)
    (10 minor-seventh seventh)
    (11 major-seventh seventh)
    (12 perfect-octave octave)))

(defun find-interval (interval-name) (find interval-name intervals :key #'second))
(defun interval-value (interval) (first interval))
;;(interval-value (find-interval 'perfect-fifth))
(defun parallel-motion (iv1 iv2) ())

(defun parallel-p (iv1 iv2) (= (iv-length iv1) (iv-length iv2)))

(defun lchord-interval (lchord v1 v2)
  "Returns interval from V1's tone to V2's tone in LCHORD"
  (make-interval (nth v2 lchord) (nth v1 lchord))) ;from low to high

(defun describe-notes (v1 v2 subtactus-i ch &optional long-format-p)
  (let ((pos (c-position ch subtactus-i)))
    (apply #'format 
	   (if long-format-p "For %s and %s, in bar %d, on beat %d at subbeat %d: " "%s-%s|%d|%d:%d: ")
	   (voice-name v1 long-format-p) (voice-name v2 long-format-p) pos)))

(defun check-parallels-in-interval (iv1 iv2 v1 v2 interval subtactus-i ch)
  (if (and (zerop (mod (- (abs (iv-length iv1)) (first interval)) 12))
	   (parallel-p iv1 iv2))
    (message "%sParallel %ss" 
	     (describe-notes v1 v2 subtactus-i ch)
	     (symbol-name (second interval)))))

(defun check-parallels-in-intervals (iv1 iv2 v1 v2 intervals subtactus-i ch)
  (loop for interval in intervals 
	do (check-parallels-in-interval iv1 iv2 v1 v2 interval subtactus-i ch)))

(defun lchord-function-equal (lchord1 lchord2 ch)
  "Simple version, only compare for equality. Later add 7ths, 9ths, suspensions etc."
  (equal (chord-function-l (lchord-to-chord lchord1) ch) (chord-function-l (lchord-to-chord lchord2) ch)))

(defun check-parallels-in-lchords (lchord1 lchord2 intervals subtactus-i ch) 
  (when (not (lchord-function-equal lchord1 lchord2 ch))
    (do-intervals lchord1 lchord2 #'(lambda (iv1 iv2 v1 v2)
				    (check-parallels-in-intervals iv1 iv2 v1 v2 intervals subtactus-i ch)))))

(defun check-parallels (ch interval-names)
  (let ((intervals (mapcar #'find-interval interval-names)))
    (do-lchords ch #'(lambda (lchord1 lchord2)
		      (check-parallels-in-lchords lchord1 lchord2 intervals subtactus-i ch)))))
;;(check-parallels (test-choral) '(perfect-fifth))

;;(check-parallels (c-extract (test-choral) 2 4) '(perfect-fifth))

(defun check-ambitus (ch)
  ;;check ambitus
  (loop for v below 4 do
	(loop for tactus-notes in (nth v (c-voices ch))
	      for tactus-i from 1
	      do (loop for subtactus-note in tactus-notes
		       for subtactus-i from 1
		       if (not (rule-ambitus-typcial subtactus-note v))
		       do (message "%sOutside ambitus" 
				   (describe-note v tactus-i subtactus-i (c-tactus ch) (c-subtactus ch)))))))

(defun load-choral (filename)
  (4-part-choral-analysis-format
   (4-part-lilypond-parse-choral 
    (4-part-lilypond-parse-score-from-file filename)) 8))
;;(load-choral "c:/Users/Theodor/Documents/projects/UiO/MUS-1221/Mappeoppgave-3/Mappeoppgave-3.ly")

(defun check-choral-from-file (filename)
  (check-choral (load-choral filename)))

(defun check-choral (choral)
  (check-parallels choral '(perfect-fifth perfect-octave))
  (mapcar (bind #'chord-function-print (list (diatonic-order (c-key-note choral)) (c-key-mode choral)))
	  (c-chords choral)))
;;(check-lilypond-choral "c:/Users/Theodor/Documents/projects/UiO/MUS-1221/Mappeoppgave-3/Mappeoppgave-3.ly")

;;(check-choral (test-choral))

(provide '4-choral)