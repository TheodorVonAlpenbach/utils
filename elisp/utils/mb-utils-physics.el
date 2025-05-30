(defconst +gravitational-constant+ 6.6740831E-11)
;;; some physics
(defconst +standard-gravitational-parameter-earth+ 3.9860044189E14)
(defconst +earth-mass+  5.9736E24)
(defconst +earth-radius-pole+ 6.356752E6)
(defconst +earth-radius-equator+ 6.378137E6)
(defconst +earth-radius-mean+ 6.371E6)
(defconst +moon-apogee+ 405.4E6)
(defconst +light-speed+ 299792458)
(defconst +astronomical-unit+ 1.495978707E11)
(defconst +earth-aphelion+ 1.521E11)
(defconst +earth-perihelion+ 1.471E11)
;;(sqrt (/ (* 2 +gravitational-constant+ +earth-mass+) +earth-radius-pole+))
;;(sqrt (/ (* 2 +gravitational-constant+ +earth-mass+) +earth-radius-equator+))
;;(/ (* +gravitational-constant+ +earth-mass+) (sq +earth-radius-mean+))
;;(* +gravitational-constant+ +earth-mass+)

(cl-defun escape-velocity2 (h m)
  (/ (* 2 +standard-gravitational-parameter-earth+) h))
;;(escape-velocity2 +earth-radius-mean+ +earth-mass+)

(cl-defun escape-velocity (&optional (h +earth-radius-mean+) (m +earth-mass+))
  (sqrt (escape-velocity2 h m)))
;;(escape-velocity)
;;(escape-velocity (+ +earth-radius-mean+ 10E3))
;;(escape-velocity (+ +earth-radius-mean+ 100E3))
;;(escape-velocity (+ +earth-radius-mean+ 1000E3))
;;(escape-velocity (+ +earth-radius-mean+ 2000E3))
;;(escape-velocity (+ +earth-radius-mean+ 9000E3))
;;(escape-velocity (+ +earth-radius-equator+ 9000E3))
;;(escape-velocity (+ (2* +earth-radius-mean+)))
;;(escape-velocity +moon-apogee+)
;;(escape-velocity (* 10 +moon-apogee+))
;;(escape-velocity (* 100 +moon-apogee+))
;;(escape-velocity (* 1000 +moon-apogee+))
;;(escape-velocity (* 10000 +moon-apogee+))
;;(escape-velocity (* 20000 +moon-apogee+))


(cl-defun J-to-cal (J)
  (* 0.239005736 J))
;;(J-to-cal 1515)

(cl-defun lbs-to-kg (lbs)
  (* 0.45359237 lbs))
;;(mapcar (compose #'round #'lbs-to-kg) '(196.6 193 191.8 190.2))

(cl-defun at->s (tt &optional (a 9.825))
  (* 0.5 a tt tt))
;;(at->s 10)

(cl-defun as->t (s &optional (a 9.825))
  (sqrt (/ (* 2 s) a)))
;;(as->t 381)

(cl-defun at->v (tt &optional (a 9.825))
  (* a tt))

(cl-defun as->v (s &optional (a 9.825))
  (at->v (as->t s) a))
;;(* 3.6 (as->v 381))

(cl-defun alcohol-unit (percentage &optional (milliliter 1000) (n 1))
  (if (symbolp percentage)
    (alcohol-unit
     (cl-case percentage
       (:pils .047)
       (:stout7 .07)
       (:imperial-stout .09)
       (:lokkatrollet .07)
       (:wine .12)
       (:ripasso .135)
       (:chimay-blue .09)
       (:heineken .05)
       (:lager .05)
       (:ipa .05))
     milliliter n)
    (* milliliter n (/ percentage 15.0))))
;;(alcohol-unit :ripasso 350)

(provide 'mb-utils-physics)
