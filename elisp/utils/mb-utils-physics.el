;;; mb-utils-physics.el --- Small physics and unit-conversion helpers -*- lexical-binding: t; -*-

(require 'cl-lib)

;;; Code:

(defconst +gravitational-constant+ 6.67430E-11)
;;; some physics
(defconst +standard-gravitational-parameter-earth+ 3.986004418E14)
(defconst +earth-mass+ 5.9722E24)
(defconst +earth-radius-pole+ 6.356752E6)
(defconst +earth-radius-equator+ 6.378137E6)
(defconst +earth-radius-mean+ 6.371E6)
(defconst +moon-apogee+ 405.4E6)
(defconst +light-speed+ 299792458)
(defconst +astronomical-unit+ 1.495978707E11)
(defconst +earth-aphelion+ 1.521E11)
(defconst +earth-perihelion+ 1.471E11)
(defconst +parsec-in-meters+
  (/ +astronomical-unit+ (tan (/ float-pi (* 180 3600))))
  "Length of one parsec in meters.")
;;(sqrt (/ (* 2 +gravitational-constant+ +earth-mass+) +earth-radius-pole+))
;;(sqrt (/ (* 2 +gravitational-constant+ +earth-mass+) +earth-radius-equator+))
;;(/ (* +gravitational-constant+ +earth-mass+) (sq +earth-radius-mean+))
;;(* +gravitational-constant+ +earth-mass+)

(cl-defun escape-velocity2 (radius &optional (mass +earth-mass+))
  "Return squared escape velocity at RADIUS from a body of MASS."
  (unless (and (numberp radius) (cl-plusp radius))
    (error "Radius must be positive: %S" radius))
  (unless (and (numberp mass) (not (cl-minusp mass)))
    (error "Mass must be non-negative: %S" mass))
  (/ (* 2 +gravitational-constant+ mass) radius))
;;(escape-velocity2 +earth-radius-mean+ +earth-mass+)

(cl-defun escape-velocity (&optional (radius +earth-radius-mean+)
			      (mass +earth-mass+))
  "Return escape velocity at RADIUS from a body of MASS."
  (sqrt (escape-velocity2 radius mass)))
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

(cl-defun meter-to-parsec (&optional (meters 1))
  "Convert METERS to parsecs."
  (/ meters +parsec-in-meters+))
;;(/ (parsec-to-meter) (* +light-speed+ 60 60 24 365.24))

(cl-defun parsec-to-meter (&optional (parsecs 1))
  "Convert PARSECS to meters."
  (* parsecs +parsec-in-meters+))

(cl-defun J-to-cal (joules)
  "Convert JOULES to thermochemical calories."
  (* 0.239005736 joules))
;;(J-to-cal 1515)

(cl-defun lbs-to-kg (lbs)
  "Convert LBS to kilograms."
  (* 0.45359237 lbs))
;;(mapcar (compose #'round #'lbs-to-kg) '(196.6 193 191.8 190.2))

(cl-defun at->s (tt &optional (a 9.825))
  "Return distance traveled from rest after time TT at acceleration A."
  (* 0.5 a tt tt))
;;(at->s 10)

(cl-defun as->t (s &optional (a 9.825))
  "Return time needed to travel S from rest at acceleration A."
  (when (zerop a)
    (error "Acceleration must be non-zero"))
  (sqrt (/ (* 2 s) a)))
;;(as->t 381)

(cl-defun at->v (tt &optional (a 9.825))
  "Return velocity after time TT at acceleration A."
  (* a tt))

(cl-defun as->v (s &optional (a 9.825))
  "Return velocity after traveling S from rest at acceleration A."
  (at->v (as->t s a) a))
;;(* 3.6 (as->v 381))

(cl-defun alcohol-unit (percentage &optional (milliliter 1000) (n 1))
  "Return 15 ml alcohol units for N servings of MILLILITER.
PERCENTAGE is an alcohol fraction or one of the supported drink symbols."
  (if (symbolp percentage)
      (let ((fraction
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
	       (:ipa .05)
	       (otherwise
		(error "Unknown drink type: %S" percentage)))))
	(alcohol-unit fraction milliliter n))
    (* milliliter n (/ percentage 15.0))))
;;(alcohol-unit :ripasso 350)

(provide 'mb-utils-physics)

;;; mb-utils-physics.el ends here
