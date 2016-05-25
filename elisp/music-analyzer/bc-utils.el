(defun b1-legal-third-p (ibase mode &optional end-p)
  "Returns nil iff base interval IBASE is not a legal third in MODE.
If end-p is non-nil, special rules concering final note is
applied (i.e. M3 always accepted)."
  (or (i-is ibase (m-third mode))
      (and end-p 
	   (i-is ibase 'M3))))

(defun b1-legal-start-or-end-interval (interval mode end-p cp-super-p)
  "Checks that INTERVAL is legal as a start or final note in MODE in bc1. 
If END-P is nil INTERVAL is considered at the start, else at the end of counterpoint.
Iff cp-super-p is nil, cf is in the upper voice.

Util for Regel 1.1 hos Grinde"
  (and (neq (i-direction interval) :down)
       (let ((ibase (i-base interval)))
	 (if (not cp-super-p)
	   ;; must be octave equivalent
	   (i-is ibase 'P1)
	   ;; else, may be third or fifth as well
	   (or (i-is ibase 'P1)
	       (b1-legal-third-p ibase mode end-p)
	       (i-is ibase 'P5))))))

(defun cp-consonance-p (vinterval)
  "Returns non-nil iff VINTERVAL is consonant"
  (or (i-is vinterval '(P1 m3 M3 m6 M6) #'i~)
      (and (eq (i-direction vinterval) :up)
	   (i-is vinterval 'P5 #'i~))))

(defun vppair2-same-direction (vppair1 vppair2)
  "Returns non nil iff vertical pitch pairs VPPAIR1 and VPPAIR2 does not move in the same direction.
The return value is :up or :down if the motion is upwards or downwards respectively."
  (apply #'eq* (mapcar #'i-direction (mapcar* #'i-new vppair1 vppair2))))
;;(vppair2-parallel-motion (list (p-new) (p-new (chrome-new 1))) (list (p-new (chrome-new 1)) (p-new (chrome-new 2)))) => :up
;;(vppair2-parallel-motion (list (p-new (chrome-new 1)) (p-new (chrome-new 2))) (list (p-new) (p-new (chrome-new 1)))) => :down
;;(vppair2-parallel-motion (list (p-new (chrome-new 1)) (p-new (chrome-new 2))) (list (p-new (chrome-new 1)) (p-new (chrome-new 2)))) => nil

(defun vppair2-parallel-motion (vppair1 vppair2)
  "Returns the interval of vertical pitch pair VPPAIR1 (and VPPAIR2!) iff they move in parallel, otherwise nil."
  (and vppair1 vppair2
       (i= (apply #'i-new vppair1)
	   (apply #'i-new vppair2))
       (p/= (first vppair1) (first vppair2))
       (apply #'i-new vppair1)))
;;(vppair2-parallel-motion (list (p-new) (p-new (chrome-new 4))) (list (p-new (chrome-new 1)) (p-new (chrome-new 5))))

(defun vppairs-parallel-sequence-length (vppairs)
  "Returns the length of parallel sequence starting at top of vppairs"
  (when (> (length vppairs) 1) 
    (let ((istart (vppair2-parallel-motion (first vppairs) (second vppairs))))
      (when istart
	(loop for vppair1 in (nthcdr 1 vppairs)
	      for vppair2 in (nthcdr 2 vppairs)
	      for i = (vppair2-parallel-motion vppair1 vppair2)
	      while (i= i istart)
	      count t)))))

(defun chrome-in-mode (chrome key)
  "No modulation is allowed. Standard alterations in modes are allowed."
  (member chrome (flatten (k-scale key t))))


;; (defrule duration (note)
;;   (when (= (note-duration note) 4)
;;     (format "In voice %s note %s is not a whole note" (v-instrument v) (n-to-string n))))

;; (defrule duration (notes)
;;   (eq* (mapcar #'n-duration notes))
;;   "Not all notes have the same duration")

(provide 'bc-utils)