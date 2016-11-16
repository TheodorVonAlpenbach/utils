(defun pft-closed-path (subpaths)
  ())
(defun pft-bezier3 (p1 c1 c2 p2)
  "Writes cubic bezier to pft format"
  ())

(defun rdir () (1 0))
(defun ldir () (-1 0))
(defun udir () (0 1))
(defun ddir () (0 -1))

(defun bcircle ()
  "http://spencermortensen.com/articles/bezier-circle/"
  nil)

(defun pft-area (path &rest holes)
  )

(defun pft-point (p)
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
