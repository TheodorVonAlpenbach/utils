;;; pitch 0 for c1-b1, 1 for c2-b2, -1 for c-b etc
;;; octave-modifier: number as in c1, c2, 1A, 2F
;;; small-octave-modifier c1, c2
;;; big-octave-modifier 1A, 2F

(defconst 4tone-regexp "\\`\\([0-9]?\\)\\([A-Ga-g]\\)\\(\\(es\\)*\\|\\(is\\)*\\)\\([0-9]?\\)\\'")

(defun 4tone-parts (4tone)
  (string-match 4tone-regexp 4tone)
  (mapcar #'(lambda (n) (match-string n 4tone)) '(1 2 4 5 6)))
;;(4tone-parts "1Bes")

(defstruct (4-part-note (:conc-name 4note-))
  "ENTRIES is a hash table of LPEs (se below). Max-index should be
autoincremented each time new entry is added to db."
  (tone) ;1Ees Ees ees ees1 ees1...
  (duration)) 
;; duration before split: 1 2 4 (2 4) (4 8 16)
;; after split: 4 8 16 where 4 is pulse

(defun ltone-to-4tone (ltone pitch)
  (case pitch
    ((1 2 3 4 5 6)
     (format "%s%d" (downcase ltone) pitch))
    (0 (downcase ltone))
    (-1 (capitalize ltone))
    ((-2 -3 -4 -5 -6)
     (format "%d%s" (- -1 pitch) (capitalize ltone)))))
;;(ltone-to-4tone "ees" -2)

(defun 4note-parse-lnote (lnote relative-4tone last-duration)
  ;;up or down
  (make-4-part-note
   :tone (4note-parse-tone lnote pitch)
   :duration duration))

(defun 4note-parse-lvoice (lvoice relative-ltone measure)
  (let ((relative-4tone)
	(4voice))
    (dolist (lnote lvoice (reverse 4voice))
      )))

(defun 4tone-to-dtone (4tone)
  "4TONE must be welldefined. Can add validation later"
  (let* ((4tone-parts (4tone-parts 4tone))
	 (big-octave-modifier (string-to-number (nth 0 4tone-parts)))
	 (base-name (nth 1 4tone-parts))
	 (flats (nth 2 4tone-parts))
	 (sharps (nth 3 4tone-parts))
	 (small-octave-modifier (string-to-number (nth 4 4tone-parts)))
	 (dtone 0))
    (when (> small-octave-modifier 0)
      (setq dtone (+ -12 dtone (* 12 small-octave-modifier))))
    (when (> big-octave-modifier 0)
      (setq dtone (+ -24 dtone (* -12  big-octave-modifier))))
    (when (nor (> small-octave-modifier 0) (> big-octave-modifier 0))
      (setq dtone (- dtone (if (upper-case-p (elt base-name 0)) 
			     24 12))))
    (when base-name ;should of course always evaluate to t
      (setq dtone (+ dtone
		     (string-case (downcase base-name) 
		       ("c" 0)
		       ("d" 2)
		       ("e" 4)
		       ("f" 5)
		       ("g" 7)
		       ("a" 9)
		       ("b" 11)))))
    (when flats
      ;;es's
      (setq dtone (- dtone (/ (length flats) 2))))
    (when sharps
      ;;is's
      (setq dtone (+ dtone (/ (length sharps) 2))))

    dtone))
;;(mapcar #'4tone-to-dtone '("2B" "1B" "B" "b" "b1" "b2"))