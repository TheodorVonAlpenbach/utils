(require 'mb-utils-div)
(require 'chordx) ;;chord as skeleton

;;;; Short-cut: SCHORDX
;;;; '(schosk inversion root)
;;;; schosk:        see schosk.el
;;;; inversion:     0 (root-position), 1, ... 
;;;; root: spc

;;;; This is also the module where the basic analysis methods should be implemented
;;;; If one wants to invoke analysis on schord level, just forward methods to
;;;; the corresponding schord* analyis, or event write a general function
;;;; (defun define-schord (schordx-method)...)

(defun* schordx-new (&optional (schosk (schosk-new)) (inversion 0) (root 0))
  (list schosk inversion root))
;;(schordx-new)

(defalias 'schordx-copy 'copy-tree)
(defalias 'schordx-p 'list-p)

(defalias 'schordx-schosk 'first)
(defalias 'schordx-inversion 'second)
(defalias 'schordx-root 'third)

;;; conversions
(defun schordx-from-chord (chord)
  (schord-to-schordx (schord-from-chord chord)))
;;(schordx-from-chord (chromes-from-string "F# A D"))

(defun schordx-to-schosk-info (schordx)
  (schosk-info-from-schosk (schordx-schosk schordx)))
;;(schordx-to-schosk-info (schordx-new))

(defun schordx-from-chordx (chordx)
  (schordx-new (schosk-from-chosk (chordx-chosk chordx))
	       (chordx-inversion chordx)
	       (spc-from-chrome (chordx-root chordx))))
;;(schordx-from-chordx (chordx-new))

(defun* schordx-to-chordx (schordx &optional (reference-chrome (chrome-new)))
  (chordx-new (schosk-to-chosk (schordx-schosk schordx))
	      (schordx-inversion schordx)
	      (spc-to-chrome (schordx-root schordx) reference-chrome)))
;;(schordx-to-chordx (schordx-new))

(defun schordx-best-diminished (schordx reference-spitch)
  (if (= (mod (- reference-spitch (schordx-root schordx)) 3) 1)
    (schordx-new (schordx-schosk schordx) 
		 (schordx-inversion schordx)
		 (mod (1- reference-spitch) 12))
    schordx))

(defun* schordx-to-chord (schordx &optional (reference-chrome (chrome-new)))
  (let ((schordx* (if (schordx-diminished-p schordx)
		    (schordx-best-diminished schordx (chrome-to-spitch reference-chrome))
		    schordx)))
    (chord-from-chordx (schordx-to-chordx schordx* reference-chrome))))
;;(schordx-to-chord (schordx-new))


;;; unary relation queries
(defun schordx-type (schordx)
  "Returns the type of SCHORDX, e.g. 'major-triad or dominant-seventh-flat-five"
  (schosk-to-chord-type (schordx-schosk schordx)))

(defun schordx-type-p (schordx type)
  "Is SCHORDX of type TYPE?"
  (eq (schordx-type schordx) type))

(defun* schordx-group (schordx &optional symbols-only (groups chord-type-group-list))
  "Returns the group of SCHORDX, e.g. 'major-triad or dominant-seventh-flat-five"
  (chord-type-groups (schordx-type schordx) symbols-only groups))
;;(schordx-group (schordx-new))

(defun schordx-chord-type-group-p (schordx chord-type-group)
  "Returns non-nil iff schordx belongs to CHORD-TYPE-GROUP"
  (schosk-chord-type-group-p (schordx-schosk schordx) chord-type-group))
;;(schosk-chord-type-group-p '(4 7) 'dominantic)

;; common short cuts
(defun schordx-tonic-p (schordx)
  "Returns nil iff SCHORDX is not a major or minor triad chord."
  (schordx-chord-type-group-p schordx 'tonic))
;;(schordx-tonic-p (schordx-new '(4 7)))

(defun schordx-dominant-p (schordx)
  "Must be dominant, ie. a major triad does not qualify TODO: is
these functions necessary now? The body almost as short as the
call expression"
  (schordx-chord-type-group-p schordx 'dominant))
;;(schordx-dominant-p (schordx-new '(4 7)))

(defun schordx-dominantic-p (schordx)
  "Returns nil iff SCHORDX is not a set chord that could functions as a dominant."
  (schordx-chord-type-group-p schordx 'dominantic))
;;(schordx-dominantic-p (schordx-new '(4 7 10)))

(defun schordx-diminished-p (schordx)
  "Returns nil iff SCHORDX is not a set chord that is diminished"
  (schordx-chord-type-group-p schordx 'diminished))
;;(schordx-diminished-p (schordx-new '(3 6)))


;;; binary relation queries
(defun schordx-root-difference (x y)
  (- (schordx-root y) (schordx-root x)))

(defun schordx-dominant-fifth-relation-p (x y)
  (and (= 5 (mod (schordx-root-difference x y) 12))
       (schordx-dominantic-p x)
       (schordx-tonic-p y)))

(defun schordx-dominant-dim-relation-p (x y)
  "Dm T."
  (and (= 1 (mod (schordx-root-difference x y) 3))
       ;;(schordx-tonic-p y) ;;perhaps in some styles, but not now
       (schordx-diminished-p x))) 

(defun schordx-dominant-submediant-relation-p (x y)
  "Ds T"
  (or
   ;; minor (must be in 1st inversion? not for now)
   (and (eq (schordx-type x) 'augmented-triad)
	(= 1 (mod (schordx-root-difference x y) 4))
	(schordx-tonic-p y)) ;; allow for picardian resolution
   ;; major: must be in 1st inversion
   (and (= 8 (mod (schordx-root-difference x y) 3))
	(schordx-type-p x 'minor) ;;allow for 7th? not for now
	(= (schordx-inversion x) 1) ;;1st inversion
	(schordx-group-p y 'majors))))

(defun schordx-dominantic-relation-p (x y)
  (or (schordx-dominant-fifth-relation-p x y)
      (schordx-dominant-dim-relation-p x y)
      (schordx-dominant-submediant-relation-p x y)))
;;(schordx-dominant-relation-p  '(7 9 1 4) '(5 9 2 2))
;;(schordx-type '(0 4 7))

(defun schordx-neapolitan-relation-p (x y)
  (and (= (mod (schordx-root-difference x y) 12) 11)
       (schordx-chord-type-group-p x 'majors)
       (schordx-tonic-p y)))

(defun schordx-subdominant-relation-p (x y)
  "Note the difference to subdominantic"
  (and (= (mod (schordx-root-difference x y) 12) 7)
       ;;allow alterations in this version
       (schordx-tonic-p y)))

(defun schordx-subsubdominant-relation-p (x y)
  "Is X the SS of Y"
  (and (= (mod (schordx-root-difference x y) 12) 10)
       ;;allow alterations in this version
       (schordx-tonic-p y)))

(defun schordx-subdominantic-relation-p (x y)
  (or (schordx-subdominant-relation-p x y)
      (schordx-subsubdominant-relation-p x y)
      (schordx-neapolitan-relation-p x y)))

(defun schordx-relation (x y)
  (cond 
   ((mequal #'schordx-root x y) 'equal)
   ((schordx-dominantic-relation-p x y) 'dominantic)
   ((schordx-neapolitan-relation-p x y) 'neapolitan)
   ((schordx-subdominantic-relation-p x y) 'subdominantic)))


;;; read/print
(defun* schordx-from-string (s &optional (style mu-default-print-style))
  (schordx-from-chordx (chordx-from-string s)))

(defun* schordx-to-string (schordx &optional (style mu-default-print-style))
  (format "%d%s" (schordx-root schordx) (schosk-info-chord-abbrev (schordx-to-schosk-info schordx))))
;;(schordx-to-string (schord-to-schordx '(5 9 2)))

(provide 'schordx)
