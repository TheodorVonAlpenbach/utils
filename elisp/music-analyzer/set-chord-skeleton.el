;;(require 'set-chords)

;;;; NBNBNBNB!! This module is being obsolete! It is still only kept for back reference to methods

;;; SCS
(defconst schord-skeleton-info-list ;; abbrev skeleton
  '(((4) major-triad-without-fifth "\5") ;c e
    ((3) minor-triad-without-fifth "m\5") ;c es
    ((4 7) major-triad "") ;c e g
    ((3 7) minor-triad "m") ;c es g
    ((3 6) diminished-triad "d") ;c es fis
    ((4 8) augmented-triad "A")  ;c e gis
    ((5 7) suspended-triad "sus4")  ;c f g
    ((4 11) major-seventh-without-fifth "maj7\5") ;c e h
    ((3 10) minor-seventh-without-fifth "m7\5") ;c es b
    ((4 10) dominant-seventh-without-fifth "7\5") ;c e b
    ((7 10) dominant-seventh-without-third "7\3") ;c g b
    ((3 11) minor-major-seventh-without-fifth "m+7\5") ;c es b
    ((5 11) suspended-seventh-without-fifth "7sus4\5")   ;c f b
    ((2 4 7) major-ninth-suspension) ;c d e g
    ((2 3 7) minor-ninth-suspension) ;c d es g
    ((4 7 11) major-seventh "maj7") ;c e g h
    ((3 7 10) minor-seventh "m7") ;c es g b
    ((4 7 10) dominant-seventh "7") ;c e g b = V^7
    ((4 6 10) dominant-seventh-flat-five "7-5") ;c e ges b
    ((3 6 9) diminished-seventh "d7" dim) ;c es ges bes = vii^7
    ((3 6 10) half-diminished-seventh "m7-5") ;c es ges b = ii^7
    ((3 7 11) minor-major-seventh "m7-5") ;c es g h
    ((4 8 11) augmented-major-seventh "A7") ;c e gis h
    ((4 8 10) augmented-seventh "A-7") ;c e gis b = iii^7
    ((5 7 11) suspended-seventh "maj7sus4") ;c f g h
    ((5 7 10) suspended-dominant-seventh "7sus4"))
  "TODO: This should be generated from `chord-skeleton-info-list'") ;c f g b

(defalias 'schosk-info-schosk 'first)
(defalias 'schosk-info-chord-type 'second)
(defalias 'schosk-info-chord-abbrev 'third)

(defun skeleton-chord (skeleton)
  (first skeleton))
;;(mapcar #'skeleton-chord set-chord-skeletons)

(defun skeleton-name (skeleton)
  (second skeleton))
;;(mapcar #'skeleton-name set-chord-skeletons)

(defun skeleton-chord-symbol (skeleton)
  (third skeleton))
;;(mapcar #'skeleton-chord-symbol set-chord-skeletons)

(defun skeleton-from-schord (scs)
  (find scs schord-skeleton-info-list :key #'first :test #'equal))
;;(mapcar #'skeleton scs-test)

(defun skeleton-from-chord-symbol (chord-type)
  (find chord-type schord-skeleton-info-list :key #'third :test #'string=))
;;(skeleton-from-chord-symbol "7")


;;; SCS
(defun scs-chord (scs)
  (first (skeleton scs)))
;;(mapcar #'scs-chord scs-test)

(defun scs-name (scs)
  (second (skeleton scs)))
;;(mapcar #'scs-name scs-test)

(defun* scs-chord-symbol (scs &optional (print-style mu-default-print-style))
  (third (skeleton scs)))
;;(mapcar #'scs-chord-symbol scs-test)

(defun scs-from-chord-symbol (chord-type)
  (first (skeleton-from-chord-symbol chord-type)))
;;(scs-from-chord-symbol "7")
;;(mapcar #'scs-from-chord-symbol '("m7" "" "a" "d"))

(defun* scs-from-sc (sc &optional (ordered t) (trivial nil))
  "Returns the skeleton of set chord SC. 
Iff ORDERED is non nil, the result is sorted. 
Iff TRIVIAL is nil a trivial skeleton is returned."
  (let ((res (mapcar (bind #'spc-transpose (- (first sc)))
		     (remove-duplicates sc :from-end t))))
    (when ordered (setq res (sort res #'<)))
    (unless trivial (setq res (rest res)))
    res))
;;(scs-from-sc '(7 2 7 10) nil t)
;;(mapcar #'scs-from-sc '((7 2 7 10) (7 2 10) (2 2 6 9)))

(defun scs-to-sc (scs sc-root)
  (sc-transpose (cons 0 scs) sc-root))
;;(scs-to-sc (first (nth 17 schord-skeleton-info-list)) 1)
;;(mapcar (compose (bind #'scs-to-sc 1) #'scs-from-chord-symbol) '("m7" nil "a" "d"))

(defun skeleton-to-sc (skeleton sc-root)
  (scs-to-sc (first skeleton) sc-root))
;;(scs-to-sc (nth 17 set-chord-skeletons) 1)
;;(mapcar (compose (bind #'scs-to-sc 1) #'scs-from-chord-symbol) '("m7" nil "a" "d"))

(defun* scs-ninvert (scs &optional (n 1))
  "Returns the Nth inversion of set chord skeleton SCS. Destructive."
  (let ((trivialp (= (first scs) 0)))
    (unless trivialp scs (push 0 scs))
    (scs-from-sc (nrotate-list scs n) t trivialp)))
;;(scs-ninvert '(4 7) 2)

(defun* scs-invert (scs &optional (n 1))
  "Returns the Nth inversion of set chord skeleton SCS. Not destructive."
  (scs-ninvert (copy-list scs) n))
;;(scs-invert (first (find 'major-triad set-chord-skeletons :key #'second)) 3)

(defun scs-type (scs)
  "Returns the classification of set chord skeleton SCS.
Classification is a pair of two elements (SKELETON
INVERSION-DEGREE)"
  (loop for i to (length scs) 
	for chordtype = (find (scs-invert scs (- i)) set-chord-skeletons
			      :key #'first :test #'equal)
	if chordtype return (list chordtype i)))
;;(mapcar #'scs-type '((5 9)))

;; (defconst scs-types
;;   (loop for x in schord-skeleton-info-list
;; 	collect (list (first x)
;; 		      (mapcar (bind #'tmap-1-0 schord-skeleton-info-list) (second x)))))

(defun scs-type-p (scs type)
  (find scs (second (assoc type scs-types)) :test #'equal))
;;(mapcar (bind #'scs-type-p 'major) '((4) (3) (4 7))) 

(provide 'set-chord-skeleton)
