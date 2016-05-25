(require 'interval)
(require 'chrome)

(defconst chord-skeleton-info-list
  '(((M3) major-triad-without-fifth "\\5") ;c e
    ((m3) minor-triad-without-fifth "m\\5") ;c es
    ((M3 P5) major-triad "") ;c e g
    ((m3 P5) minor-triad "m") ;c es g
    ((m3 d5) diminished-triad "d") ;c es ges
    ((M3 A5) augmented-triad "A")  ;c e gis
    ((P4 P5) suspended-triad "sus4")  ;c f g
    ((M3 M7) major-seventh-without-fifth "maj7\\5") ;c e h
    ((m3 m7) minor-seventh-without-fifth "m7\\5") ;c es b
    ((M3 m7) dominant-seventh-without-fifth "7\\5") ;c e b
    ((P5 m7) dominant-seventh-without-third "7\\3") ;c g b
    ((m3 M7) minor-major-seventh-without-fifth "m+7\\5") ;c es b
    ((P4 M7) suspended-seventh-without-fifth "7sus4\\5")   ;c f b
    ((M2 M3 P5) major-ninth-suspension) ;c d e g
    ((M2 m3 P5) minor-ninth-suspension) ;c d es g
    ((M3 P5 M7) major-seventh "maj7") ;c e g h
    ((m3 P5 m7) minor-seventh "m7") ;c es g b
    ((M3 P5 m7) dominant-seventh "7") ;c e g b = V^7
    ((M3 d5 m7) dominant-seventh-flat-five "7-5") ;c e ges b
    ((m3 d5 d7) diminished-seventh "d7" dim) ;c es ges bes = vii^7
    ((m3 d5 m7) half-diminished-seventh "m7-5") ;c es ges b = ii^7
    ((m3 P5 M7) minor-major-seventh "m7-5") ;c es g h
    ((M3 A5 M7) augmented-major-seventh "A7") ;c e gis h
    ((M3 A5 m7) augmented-seventh "A-7") ;c e gis b = iii^7
    ((P4 P5 M7) suspended-seventh "maj7sus4") ;c f g h
    ((P4 P5 m7) suspended-dominant-seventh "7sus4"))
  "Each element is a list on form (chord-skeleton chord-name-symbol chord-type-string)")

;;; style based
(defun chord-type-group-flatten (ctg-definitions)
  (let ((res ()))
    (loop for definition in ctg-definitions
	  for name = (first definition)
	  for values = (second definition)
	  for values-extended = (loop for value in values
				      for referenced-definition = (find value res :key #'first)
				      collect (if referenced-definition 
						(second referenced-definition)
						value))
	  collect (list name (remove-duplicates (flatten values-extended))) into res
	  finally return res)))
;;(chord-type-group-flatten chord-type-group-list)

(defconst chord-type-group-list
  (chord-type-group-flatten
   '((majors (major-triad-without-fifth major-triad))
     (minors (minor-triad-without-fifth minor-triad))
     (tonic (majors minors))
     (dominants (dominant-seventh-without-fifth dominant-seventh-without-third dominant-seventh))
     (diminished (diminished-triad diminished-seventh half-diminished-seventh))
     (augmented (augmented-triad augmented-major-seventh augmented-seventh))
     (dominantic (majors dominants diminished-triad diminished-seventh))
     (minor-majors (minor-major-seventh-without-fifth))
     (major-sevenths (major-seventh major-seventh-without-fifth))
     (minor-sevenths (minor-seventh minor-seventh-without-fifth minor-major))
     (sevenths (major-sevenths minor-majors minor-sevenths)))))

(defun* chord-type-groups (chord-type &optional symbols-only (groups chord-type-group-list))
  "Returns the chord type groups that contains CHORD-TYPE"
  (let ((result (copy-if (bind #'find chord-type 1) groups :key #'second)))
    (if symbols-only (mapcar #'first result) result)))
;;(chord-type-groups 'major-triad t)

(defun chord-type-group-p (chord-type chord-type-group)
  "Returns nil iff CHORD-TYPE belongs to CHORD-TYPE-GROUP"
  (find chord-type (second (find chord-type-group chord-type-group-list :key #'first))))
;;(chord-type-group-p 'major-triad 'dominantic)

(defalias 'chosk-info-chosk 'first)
(defalias 'chosk-info-chord-type 'second) ;;TODO: rename to chord-type
(defalias 'chosk-info-chord-abbrev 'third) ;;TODO: rename to chord-abbrev or chord-symbol

(defun chosk-new ()
  (chosk-info-chosk (third chord-skeleton-info-list)))
;;(chosk-new)

(defun chosk-info-from-chosk (chord-skeleton)
  (find chord-skeleton chord-skeleton-info-list :key #'chosk-info-chosk :test #'equal))
;;(mapcar #'chosk-info-from-chosk (mapcar #'chosk-info-to-chosk chord-skeleton-info-list))

(defun chosk-info-from-chord-type (chord-type)
  (find chord-type chord-skeleton-info-list :key #'chosk-info-chord-type :test #'equal))
;;(mapcar #'chosk-info-from-chord-type (mapcar #'chosk-info-chord-type chord-skeleton-info-list))

(defun chosk-info-from-chord-abbrev (chord-type)
  (find chord-type chord-skeleton-info-list :key #'chosk-info-chord-abbrev :test #'equal))
;;(mapcar #'chosk-info-from-chord-abbrev (mapcar #'chosk-info-chord-abbrev chord-skeleton-info-list))


(provide 'chord-skeleton)
