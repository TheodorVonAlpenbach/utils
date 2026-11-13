;;; mb-utils-physics.el --- Small physics and unit-conversion helpers -*- lexical-binding: t; -*-

(require 'cl-lib)

;;; Code:

(defconst +gravitational-constant+ 6.67430E-11
  "Newtonian constant of gravitation in cubic meters per kilogram-second squared.")

(defconst +standard-gravitational-parameter-earth+ 3.986004418E14
  "Earth's standard gravitational parameter in cubic meters per second squared.")

(defconst +earth-mass+ 5.9722E24
  "Earth's mass in kilograms.")

(defconst +earth-radius-pole+ 6.356752E6
  "Earth's polar radius in meters.")

(defconst +earth-radius-equator+ 6.378137E6
  "Earth's equatorial radius in meters.")

(defconst +earth-radius-mean+ 6.371E6
  "Earth's mean radius in meters.")

(defconst +moon-apogee+ 405.4E6
  "Approximate maximum Earth-Moon center-to-center distance in meters.")

(defconst +light-speed+ 299792458
  "Speed of light in vacuum in meters per second.")

(defconst +astronomical-unit+ 1.495978707E11
  "Length of one astronomical unit in meters.")

(defconst +earth-aphelion+ 1.521E11
  "Approximate Earth-Sun distance at aphelion in meters.")

(defconst +earth-perihelion+ 1.471E11
  "Approximate Earth-Sun distance at perihelion in meters.")

(defconst +parsec-in-meters+
  (/ (* 648000.0 +astronomical-unit+) float-pi)
  "Length of one parsec in meters.")

(defconst +standard-gravity+ 9.80665
  "Standard acceleration due to gravity in meters per second squared.")

(defconst +joules-per-thermochemical-calorie+ 4.184
  "Number of joules in one thermochemical calorie.")

(defconst +kilograms-per-pound+ 0.45359237
  "Number of kilograms in one international avoirdupois pound.")

(defconst mb-physics--alcohol-fractions
  '((:pils . 0.047)
    (:stout7 . 0.07)
    (:imperial-stout . 0.09)
    (:lokkatrollet . 0.07)
    (:wine . 0.12)
    (:ripasso . 0.135)
    (:chimay-blue . 0.09)
    (:heineken . 0.05)
    (:lager . 0.05)
    (:ipa . 0.05))
  "Alcohol fractions for named drink types.")

(defun mb-physics--check-nonnegative (value name)
  "Signal an error unless VALUE is a nonnegative number named NAME."
  (unless (and (numberp value) (>= value 0))
    (error "%s must be non-negative: %S" name value)))

(cl-defun escape-velocity2 (radius &optional (mass +earth-mass+))
  "Return squared escape velocity at RADIUS from a body of MASS."
  (unless (and (numberp radius) (cl-plusp radius))
    (error "Radius must be positive: %S" radius))
  (unless (and (numberp mass) (not (cl-minusp mass)))
    (error "Mass must be non-negative: %S" mass))
  (/ (* 2 +gravitational-constant+ mass) radius))

(cl-defun escape-velocity (&optional (radius +earth-radius-mean+)
			      (mass +earth-mass+))
  "Return escape velocity at RADIUS from a body of MASS."
  (sqrt (escape-velocity2 radius mass)))

(cl-defun meter-to-parsec (&optional (meters 1))
  "Convert METERS to parsecs."
  (cl-check-type meters number)
  (/ meters +parsec-in-meters+))

(cl-defun parsec-to-meter (&optional (parsecs 1))
  "Convert PARSECS to meters."
  (cl-check-type parsecs number)
  (* parsecs +parsec-in-meters+))

(cl-defun J-to-cal (joules)
  "Convert JOULES to thermochemical calories."
  (cl-check-type joules number)
  (/ joules +joules-per-thermochemical-calorie+))

(cl-defun lbs-to-kg (lbs)
  "Convert LBS to kilograms."
  (cl-check-type lbs number)
  (* +kilograms-per-pound+ lbs))

(cl-defun at->s (tt &optional (a +standard-gravity+))
  "Return distance traveled from rest after time TT at acceleration A."
  (mb-physics--check-nonnegative tt "Time")
  (cl-check-type a number)
  (* 0.5 a tt tt))

(cl-defun as->t (s &optional (a +standard-gravity+))
  "Return time needed to travel S from rest at acceleration A."
  (cl-check-type s number)
  (cl-check-type a number)
  (when (zerop a)
    (error "Acceleration must be non-zero"))
  (when (cl-minusp (/ s a))
    (error "Distance and acceleration must have the same sign"))
  (sqrt (/ (* 2 s) a)))

(cl-defun at->v (tt &optional (a +standard-gravity+))
  "Return velocity after time TT at acceleration A."
  (mb-physics--check-nonnegative tt "Time")
  (cl-check-type a number)
  (* a tt))

(cl-defun as->v (s &optional (a +standard-gravity+))
  "Return velocity after traveling S from rest at acceleration A."
  (at->v (as->t s a) a))

(cl-defun alcohol-unit (percentage &optional (milliliter 1000) (n 1))
  "Return 15 ml alcohol units for N servings of MILLILITER.
PERCENTAGE is an alcohol fraction or one of the supported drink symbols."
  (if (symbolp percentage)
      (let ((entry (assq percentage mb-physics--alcohol-fractions)))
	(unless entry
	  (error "Unknown drink type: %S" percentage))
	(alcohol-unit (cdr entry) milliliter n))
    (unless (and (numberp percentage) (<= 0 percentage 1))
      (error "Alcohol fraction must be between zero and one: %S" percentage))
    (mb-physics--check-nonnegative milliliter "Volume")
    (mb-physics--check-nonnegative n "Serving count")
    (* milliliter n (/ percentage 15.0))))

(provide 'mb-utils-physics)

;;; mb-utils-physics.el ends here
