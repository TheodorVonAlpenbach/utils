(require 'set-chords)

;;; NB! the analysis below is in Bach style

(defun sc-in-key-p (sc key)
  "Returns nil iff SC is an unalted chord in KEY"
  (sc-scale-position sc key))

(defun sc-mode (sc)
  "TODO: This function is bad! Name and posistion and all"
  (case (second (skeleton (scs-from-sc sc)))
    ((major-triad-without-fifth
      major-triad
      major-triad major-seventh-without-fifth
      dominant-seventh-without-fifth major-ninth-suspension
      major-seventh dominant-seventh
      dominant-seventh-flat-five)
     'major)
    ((minor-triad-without-fifth
      minor-triad
      minor-seventh-without-fifth
      minor-major-seventh-without-fifth
      minor-ninth-suspension
      minor-seventh
      minor-major-seventh)
     'minor)
    ((suspended-seventh
      suspended-dominant-seventh
      suspended-seventh-without-fifth
      suspended-triad)
     'suspension)))
;;(sc-mode '(8 0 3))

(defun sc-major-p (sc) (eql (sc-mode sc) 'major))
(defun sc-minor-p (sc) (eql (sc-mode sc) 'minor))

(defun* sc-to-key (sc &optional (reference-key (make-key)))
  "Returns the most possible key given that chord SC is all we know, appart from REFERENCE-KEY."
  (make-key :chrome (spc-to-chrome (sc-root sc)
						  (k-root reference-key))
	    :mode (sc-mode sc)))
  
(defun set-tonicization (sc1 sc2 key)
  "Can't handle chords that require look aheads (e.g suspensions)"
  ;; 
  (cond
   ((or (sc-strictly-dominant-p sc1)
	(and (sc-dominant-p sc1)
	     (sc-minor-p sc2)))
    (sc-to-key sc2))
   ;; else just return reference key
   (t key))) 

(provide 'set-key-analysis)
