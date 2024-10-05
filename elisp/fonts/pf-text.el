(cl-defun pft-closed-path (subpaths)
  ())

(cl-defun pft-bezier3 (p1 c1 c2 p2)
  "Writes cubic bezier to pft format"
  ())

(cl-defun rdir () (1 0))
(cl-defun ldir () (-1 0))
(cl-defun udir () (0 1))
(cl-defun ddir () (0 -1))

(cl-defun bcircle ()
  "http://spencermortensen.com/articles/bezier-circle/"
  ())

(cl-defun cserif (corner right-dir left-dir)
  ())

(cl-defun pft-area (path &rest holes)
  )

(cl-defun pft-point (p)
  (format "rline" ))

(deffont "alberti"
    (defchar ("A" :u 700 :h u w: u) 
	(let* ((lmargin (/ 2 3))
	       (rmargin 1)
	       (z1 (list lmargin 0))
	       (z2 (list 6 12))
	       (z3 (list (- w rmargin) 0))
	       (lleg (v- z2 z1))
	       (right-leg (v- z2 z3))
	       (lleg-width 0.5)
	       (lleg-hw (* 3 lleg-width))
	       (z11 (right-shift z1 lleg-horizontal-width))
	       (rleg-width 1)		;config
	       (rleg-horizontal-width 
		(* 3 lleg-width))
	       (z12 (left-shift z3 rleg-horizontal-width))
	       (z4 (v-intersection
		    (lshift lleg lleg-horizontal-width) 
		    (rshift rleg rleg-horizontal-width))) 
	       (inner-lleg (v- z4 z11))
	       (inner-rleg (v- z4 z12))
	       (z7 (v-intersection inner-lleg downside-bar))
	       (z5 (v-intersection inner-lleg upside-bar))
	       (z6 (v-intersection inner-rleg upside-bar))
	       (z8 (v-intersection inner-rleg downside-bar)))
	  (pft-area
	   (pft-path
	    z2
	    (cserif z1 lleg (ldir))
	    (cserif z11 (rdir) inner-lleg)
	    z7 z8
	    (cserif z18 inner-rleg (ldir))
	    (cserif z3 (rdir) rleg))
	   (pft-path z4 z5 z6)))))

(cl-defun bcircle (p1 p2 center)
  "Return a bezier representation of a circular arch going from
point P1 to point P2 on a circle centered at point CENTER. The
bezier curve is formatted as the list (P1 C1 C2 P2), where C1 and
C2 are the two control points.
See http://stackoverflow.com/questions/734076/how-to-best-approximate-a-geometrical-arc-with-a-bezier-curve and the answer by MaxArt"
  (let* ((u (vec center p1))
	 (v (vec center p2))
	 (u+v (vec-add u v))
	 (k (bcircle-optimal-k (vec-angle u v)))
	 (pe (vec-scalar-mult u+v (/ 2 (vec-sqr u+v)))))
    (list p1
	  (vec-add (vec-scalar-mult p1 (- 1 k))
		   (vec-scalar-mult pe k))
	  (vec-add (vec-scalar-mult p2 (- 1 k))
		   (vec-scalar-mult pe k))
	  p2)))
;;(bcircle '(1 0) '(0 1) '(0 0))

(cl-defun bcircle-optimal-k (angle)
  "Return optimal bezier k for a circular arch of length ANGLE radians.
For use of k, see `bcircle'."
  (* (/ 4.0 (* 3 (+ (sec (/ angle 2)) 1)))))
;;(bcircle-optimal-k (/ pi 2))
