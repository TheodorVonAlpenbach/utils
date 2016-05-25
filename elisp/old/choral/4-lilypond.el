;;;; definitions
;; lpitch: integers indicating absolute pitch -3 -> 2C; -1 -> C; 0 -> c; 2 -> c2
;; mode: tone
;; choral: 4 part choral
;; relative-pitch (lrp) : lilypond tone that first tone in choral is relative to
;; should perhaps be changed to relative-tone
;; lilypond-voice is a {...} string representing a 4-part voice. It is parsed to either

;; lvr (lilypond-voice-relative) where each note is relative to the
;; preceeding with respect to pitch and duration. E.g. "a,,4 a"
;; represents two equal notes

;; lva (lilypond-voice-absolute) where each note is absolute with
;; respect to pitch and duration. E.g. "a,,,4 a,,,4" represents two equal notes

(defun test-score ()
  (4-part-lilypond-parse-score-from-file "c:/Users/Theodor/Documents/projects/UiO/MUS-1221/scratch.ly"))
;;(test-score)

(defun test-lvr ()
  (4-part-lilypond-parse-voice "{d4 bes a | g2 fis4 | g ees' d | bes2. | d4 c bes | a bes a | g2. ~ | g4 }"))
;;(test-lvr)

(defun test-ln ()
  (nth 1 (test-lvr)))
;;(test-ln)

(defconst space-regexp "[ \f\t\n\r\v]*")
(defconst 4-part-lilypond-score-beginning "\\\\score\\s-*{")
(defconst 4-part-lilypond-character-bag "[ \f\t\n\r\v{|}~]+")
(defconst 4-part-lilypond-tone-regexp "\\(\\([cdefgab]\\)\\(\\(?:[ei]s\\)*\\)\\)")
(defconst 4-part-lilypond-pitch-regexp "\\('+\\|,+\\)?")
(defconst 4-part-lilypond-duration-regexp "\\(\\([1248]\\|16\\)\\(\\.*\\)\\)?")
(defconst 4-part-lilypond-note-regexp
  (concat "\\b" 
	  4-part-lilypond-tone-regexp
	  4-part-lilypond-pitch-regexp
	  4-part-lilypond-duration-regexp
	  "\\b"))
(defconst 4-part-lilypond-mode-regexp "\\\\\\(major\\|minor\\)")
(defconst 4-part-lilypond-key-regexp
  (concat "\\\\key" 
	  space-regexp
	  4-part-lilypond-tone-regexp
	  space-regexp
	  4-part-lilypond-mode-regexp))
(defconst 4-part-lilypond-relative-pitch-regexp
  (concat "\\\\relative"
	  space-regexp
	  4-part-lilypond-tone-regexp
	  4-part-lilypond-pitch-regexp))
(defconst 4-part-lilypond-measure-regexp
  (concat "\\\\time"
	  space-regexp
	  "[1-9][0-9]*/"
	  4-part-lilypond-duration-regexp))
(defconst 4-part-lilypond-partial-regexp
  (concat "\\\\partial" 
	  space-regexp
	  4-part-lilypond-duration-regexp))
(defconst 4-part-lilypond-staff-regexp
  (concat "\\\\new"
	  space-regexp
	  "Staff"
	  space-regexp
	  "{[^{]*\\({[^}]*}\\)[^{]*\\({[^}]*}\\)[^}]*}"))
(defconst 4-part-lilypond-two-staffs-regexp
  (concat 4-part-lilypond-staff-regexp
	  space-regexp
	  "\\\\new"
	  space-regexp
	  "Staff"
	  space-regexp
	  "{[^{]*\\({[^}]*}\\)[^{]*\\({[^}]*}\\)[^}]*}"
	  ))
;;(string-match 4-part-lilypond-two-staffs-regexp (test-score))
;;(match-string 4 (test-score))
;;(string-match 4-part-lilypond-staff-regexp (test-score))

(defstruct (lilypond-note (:conc-name lnote-))
  "ENTRIES is a hash table of LPEs (se below). Max-index should be
autoincremented each time new entry is added to db."
  (tone)
  (tone-base)
  (mode)
  (pitch)
  (duration)
  (punctuation))

;;(let ((token "bes,2.")) (string-match 4-part-lilypond-note-regexp token) (mapcar #'(lambda (x) (match-string x token)) (0-n 10)))

(defconst tone-base-map
  '(("c" 0)  ("d" 1) ("e" 2) ("f" 3) ("g" 4) ("a" 5) ("b" 6)))

(defun map-tone-base (tone-base)
  (second (assoc tone-base tone-base-map)))
;;(mapcar #'map-tone-base '("c"  "d" "e" "f" "g" "a" "b"))

(defun tone-base-difference (tone-base1 tone-base2)
  (- (map-tone-base tone-base2) (map-tone-base tone-base1)))
;;(tone-base-difference "b" "d")

(defconst tone-base-diatonic-map
  '(("c" 0)  ("d" 2) ("e" 4) ("f" 5) ("g" 7) ("a" 9) ("b" 11)))

(defun map-tone-base-diatonic (tone-base)
  (second (assoc tone-base tone-base-diatonic-map)))

(defun ln-mode-count (ln)
  (- (string-num-matches "is" (lnote-mode ln))
     (string-num-matches "es" (lnote-mode ln))))
;;(ln-mode-count (read-note "fesis"))

(defun diatonic-order (ln)
  (+ (map-tone-base-diatonic (lnote-tone-base ln))
     (ln-mode-count ln)))
;;(mapcar #'diatonic-order (mapcar #'read-note '("c" "cis" "des" "d" "dis" "ees" "e" "eis" "fes" "f" "fis" "ges" "g" "gis" "aes" "a" "ais" "bes" "b" "bis" "ces")))

(defun 4-part-lilypond-convert-pitch-string-to-number (pitch-string)
  (if pitch-string
    (if (= (length pitch-string) 0)
      0
      (if (eql (char pitch-string 0) ?')
	(length pitch-string)
	(- (length pitch-string))))
    0))
;;(mapcar #' 4-part-lilypond-convert-pitch-string-to-number '("''" "" nil ",,"))

(defun* 4-part-lilypond-parse-score-from-buffer (&optional (buffer (current-buffer)))
  (with-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward 4-part-lilypond-score-beginning nil t)
	(backward-char 1)
	(let ((beg (point))
	      (end nil))
	  (forward-sexp)
	  (setq end (point))
	  (setq beg (re-search-backward 4-part-lilypond-score-beginning nil t))
	  (buffer-substring-no-properties beg end))))))
;;(4-part-lilypond-parse-score-from-buffer "scratch.ly")

(defun 4-part-lilypond-parse-score-from-file (filename)
  (with-temp-buffer 
    (insert-file filename)
    (4-part-lilypond-parse-score-from-buffer)))
;;(4-part-lilypond-parse-score-from-file "c:/Users/Theodor/Documents/projects/UiO/MUS-1221/Mappeoppgave-3/Mappeoppgave-3.ly")

(defun 4-part-lilypond-parse-relative-pitch (lilypond-choral)
  (string-match 4-part-lilypond-relative-pitch-regexp lilypond-choral)
  (make-lilypond-note 
   :tone (concat (match-string 1 lilypond-choral)
		 (match-string 3 lilypond-choral))
   :tone-base (match-string 2 lilypond-choral)
   :mode (match-string 3 lilypond-choral)
   :pitch (4-part-lilypond-convert-pitch-string-to-number
	   (match-string 4 lilypond-choral))))
;;(4-part-lilypond-parse-relative-pitch (test-score))

(defun 4-part-lilypond-parse-key (lilypond-choral)
  (string-match 4-part-lilypond-key-regexp lilypond-choral)
  (list (match-string 1 lilypond-choral)
	(match-string 4 lilypond-choral)))
;;(4-part-lilypond-parse-key (test-score))

(defun 4-part-lilypond-parse-measure (lilypond-choral)
  (string-match 4-part-lilypond-measure-regexp lilypond-choral)
  (list (string-to-number (match-string 1 lilypond-choral))
	(string-to-number (match-string 2 lilypond-choral))))
;;(4-part-lilypond-parse-measure (test-score))

(defun 4-part-lilypond-parse-partial (lilypond-choral)
  (when (string-match 4-part-lilypond-partial-regexp lilypond-choral)
    (string-to-number (match-string 1 lilypond-choral))))
;;(4-part-lilypond-parse-partial (test-score))

(defun 4-part-lilypond-parse-voices (lilypond-choral)
  (string-match 4-part-lilypond-two-staffs-regexp lilypond-choral)
  (list (match-string 1 lilypond-choral)
	(match-string 2 lilypond-choral)
	(match-string 3 lilypond-choral)
	(match-string 4 lilypond-choral)))
;;(4-part-lilypond-parse-voices (test-score))

(defun 4-part-lilypond-parse-voice-token (voice-token)
  (when (string-match 4-part-lilypond-note-regexp voice-token)
    (let ((tone (match-string 1 voice-token))
	  (tone-base (match-string 2 voice-token))
	  (mode (match-string 3 voice-token))
	  (relative-pitch (match-string 4 voice-token))
	  (duration (match-string 6 voice-token))
	  (punctuation (match-string 7 voice-token)))
      (make-lilypond-note 
       :tone tone
       :tone-base tone-base
       :mode (or mode "")
       :pitch (4-part-lilypond-convert-pitch-string-to-number relative-pitch)
       :duration duration
       :punctuation punctuation))))
;;(read-note "feses")
;;(4-part-lilypond-parse-voice-token "bes,2.")
;;(4-part-lilypond-parse-voice-token "b,2.")

(defun 4-part-lilypond-parse-voice (lilypond-voice)
  (remove nil (mapcar #'4-part-lilypond-parse-voice-token
		      (split-string lilypond-voice 4-part-lilypond-character-bag t))) )
;;(4-part-lilypond-parse-voice "{ g4 | g bes a fis | g a bes a | bes c d c8 bes | c2 bes4\n		     g4 | g bes a fis | g a bes a | bes c d c8 bes | c2 bes4\n		     d4 | d bes c d | bes bes a \n		     a4 | g bes a fis | g a bes a | bes c d c8 bes | a2 g4\n		     \\bar \"|.\"\n		   }")

(defun 4-part-lilypond-pitch-difference (tone-base1 tone-base2)
  (let ((tone-base-difference (tone-base-difference tone-base1 tone-base2)))
    (if (< tone-base-difference -3)
      1
      (if (< tone-base-difference 4)
	0 -1))))
;;(4-part-lilypond-pitch-difference "c" "b")

(defun 4-part-lilypond-make-absolute (lilypond-voice-relative relative-note measure)
  "Measure is used for deciding the duration of first note if
first note has relative duration. Seems to work actually!"
  ;;first add a fictive note to voice
  (loop for last-duration = measure then new-duration
	for last-absolute-pitch = (lnote-pitch relative-note) then new-absolute-pitch
	for last-tone-base = (lnote-tone-base relative-note) then tone-base
	for lrn-last in (cons relative-note lilypond-voice-relative)
	for lrn in lilypond-voice-relative
	for relative-pitch = (lnote-pitch lrn)
	for tone-base = (lnote-tone-base lrn)
	for pitch-difference = (4-part-lilypond-pitch-difference last-tone-base tone-base)
	for new-absolute-pitch = (+ last-absolute-pitch relative-pitch pitch-difference)
	for relative-duration = (lnote-duration lrn) 
	for new-duration = (if relative-duration relative-duration last-duration)
	for new-punctuation = (if relative-duration (lnote-punctuation lrn) (lnote-punctuation lrn-last))
	collect (make-lilypond-note 
		 :tone (lnote-tone lrn)
		 :tone-base (lnote-tone-base lrn)
		 :mode (lnote-mode lrn)
		 :pitch new-absolute-pitch
		 :duration new-duration
		 :punctuation new-punctuation)))
;;(4-part-lilypond-make-absolute (test-lvr) (4-part-lilypond-parse-relative-pitch (test-score)) 4)

(defun 4-part-lilypond-parse-choral (lilypond-choral)
  "LILYPOND-CHORAL is a string starting with with
  substring ""\score {"" and ending with ""}"". Method parses the relative pitch, key, measure and the four voices."
  (let* ((relative-pitch (4-part-lilypond-parse-relative-pitch lilypond-choral))
	 (key (4-part-lilypond-parse-key lilypond-choral))
	 (measure (4-part-lilypond-parse-measure lilypond-choral))
	 (partial (4-part-lilypond-parse-partial lilypond-choral))
	 (absolute-voices 
	  (loop for pitch = relative-pitch then (last-elt voice-absolute)
		for voice-as-string in (4-part-lilypond-parse-voices lilypond-choral)
		for voice-absolute = (4-part-lilypond-make-absolute (4-part-lilypond-parse-voice voice-as-string) 
								    pitch measure)
		collect voice-absolute)))
    (list relative-pitch key measure partial absolute-voices)))
;;(4-part-lilypond-parse-choral (test-score))

(defun tactus-duration (lnote tactus)
  "Returns duration of lnote as number of tactus pulses it
  covers. Eg. if tactus is 4 and duration of some lnotes are 8,
  4, 2, 2., then the method returns .5, 1, 2, 3 respectively"
  (let ((n (length (lnote-punctuation lnote))))
    (* (/ tactus (string-to-number (lnote-duration lnote)))
       (/ (- (expt 2 (+ n 1)) 1.0)
	  (expt 2 n)))))
;;(tactus-duration (test-ln) 4)

(defun 4-part-subdivide-voice (lv measure subtactus)
  "Generelize later. For now only subdivides 4s to 8s."
  (flatten
   (loop for lnote in lv
	 for subtactus-units = (round (tactus-duration lnote subtactus))
	 collect (make-list subtactus-units lnote))))
;;(4-part-subdivide-voice (4-part-lilypond-make-absolute (test-lvr) (4-part-lilypond-parse-relative-pitch (test-score)) 4) '(4 4) 8)

(defun 4-part-choral-analysis-format (lc subtactus)
  (list (first lc)
	(second lc)
	(third lc)
	(fourth lc)
	subtactus
	(mapcar #'(lambda (x)
		    (4-part-subdivide-voice x (third lc) subtactus)) (fifth lc))))
;;(4-part-choral-analysis-format (4-part-lilypond-parse-choral (test-score)) 8)

(provide '4-lilypond)
