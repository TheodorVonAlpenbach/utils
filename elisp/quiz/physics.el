(defconst +gravity+ 9.80665)
(defconst +gravitational-constant+ 6.673848E-11 "m³/kg/s²")
(defconst +earth-radius+ 6.371E6 "m")
(defconst +earth-density+ 5.515E3 "kg/m3")
(defconst +earth-mass+ 5.97219E24 "kg")
(defconst +standard-atmosphere+ 101325 "Unit is Pascal, or N/m2")
(defconst +kelvin+ 273.15)
(defconst +boltzmann+ 1.3806505E-23)

(defconst +proton-charge+ -1.60217656535E-19)
(defconst +proton-mass+ 1.67262158E-27)
(defconst +electron-charge+ (- +proton-charge+))
(defconst +electron-mass+ 9.1093829140E-31)
;;(/ +proton-mass+ +electron-mass+)

(defconst +permittity-in-free-space+ 8.854187817E-12 "farad/meter")

(defun circle-area (radius)
  (* pi (sq radius)))

(defun circle-circumference (radius)
  (* pi radius))

(defun sphere-volume (radius)
  (* (/ 4.0 3) pi (expt radius 3)))
;;(sphere-volume +earth-radius+)

(defun mass-from-volume (volume density)
  (* volume density))
;;(mass-from-volume (sphere-volume +earth-radius+) +earth-density+)

(defun mass-density (mass volume)
  (/ (float mass) volume))

(defun* surface-gravity (&key (radius +earth-radius+) (density +earth-density+) (mass (mass-from-volume (sphere-volume radius) density)))
  (/ (* +gravitational-constant+ mass)
     (sq radius)))
;;(surface-gravity)

(defun time-to-touchdown (height)
  (sqrt (/ height .5 +gravity+)))
;;(time-to-touchdown 20)2.0196199771025523
;;(time-to-touchdown 4.9)

(defun* celcius (temperature &optional (unit :kelvin))
  (case unit
    (:kelvin (- temperature +kelvin+))))

(defun* kelvin (temperature &optional (unit :celcius))
  (case unit
    (:kelvin temperature)
    (:celcius (+ temperature +kelvin+))))
;;(kelvin 0)

(defun* ideal-gas-molecules (volume &key (pressure +standard-atmosphere+) (temperature (kelvin 0)))
  (/ (* pressure volume) +boltzmann+ temperature))
;;(ideal-gas-molecules 0.001)
;;(ideal-gas-molecules 0.001 :temperature (kelvin 20 :celcius))

(defun coulomb-force (q1 q2 distance)
  (/ (* q1 q2) 4 pi +permittity-in-free-space+ (sq distance)))
;;(coulomb-force +electron-charge+ +electron-charge+ 1E-10)

(defun coulomb-distance (q1 q2 force)
  (sqrt (/ (* q1 q2) 4 pi +permittity-in-free-space+ force)))
;;(coulomb-distance +electron-charge+ +electron-charge+ (* +electron-mass+ +gravity+))
;;(coulomb-distance (* 2 +proton-charge+) +electron-charge+ (- (* +electron-mass+ +gravity+)))

(defun quantify-prefix (prefix)
  (case prefix
    (:kilo 3) (:milli -3)
    (:mega 6) (:micro -6)
    (:giga 9) (:nano -9)
    (t prefix)))

(defun unit-prefix (unit)
  (if (atom unit)
    0
    (case (length unit)
      (1 0)
      (2 (first unit))
      (t (error "Doesn't yet support multiprefix")))))
;;(mapcar #'unit-prefix (list :meter '(:meter) '(:kilo :meter)))

(defun* change-unit-prefix (x &key (from 0) (to 0))
  (/ (* x (expt 10 (quantify-prefix from)))
     (expt 10 (quantify-prefix to))))
;;(change-unit-prefix 3 :from :kilo)

(defun* change-unit (x &key from to)
  (change-unit-prefix x :from (unit-prefix from) :to (unit-prefix to)))
;;(change-unit 3 :from '(:kilo :meter) :to :meter)

(defun get-unit (x default)
  (if (atom x)
    (list x default)
    (list (first x) (or (second x) default))))
;;(mapcar (bind #'get-unit :meter) '(3 (3) (3 (:kilo :meter))))

(defun get-length-unit (x) (get-unit x :meter))
(defun get-time-unit (x) (get-unit x :second))

(defun* velocity (distance time &optional (velocity-unit '(:meter :second)))
  (destructuring-bind (distance distance-unit) (get-length-unit distance)
    (destructuring-bind (time time-unit) (get-time-unit time)
      (/ (change-unit (float distance) :from distance-unit :to (first velocity-unit))
       (change-time-unit time :from-unit time-unit :to-unit (second velocity-unit))))))
;;(velocity '(44 (:kilo :meter)) '(31 :minute) '((:kilo :meter) :hour))

(defun* velocity-from-height (height &optional  (velocity-unit '(:meter :second)))
  "Using the relationship between potential energy (mgh) and kinetic energy (1/2 mv²)"
  (sqrt (* 2 +gravity+ height)))
;;(velocity-from-height 10)

(defun* velocity-from-kinetic-energy (kinetic-energy mass &optional  (velocity-unit '(:meter :second)))
  (sqrt (/ (* 2.0 kinetic-energy) mass)))
;;(velocity-from-kinetic-energy (* 1 +gravity+ 10) 1)

(defun* distance-vat (time &key (velocity 0) (acceleration 0))
  (destructuring-bind (time time-unit) (get-unit time :second)
    (destructuring-bind (velocity velocity-unit) (get-unit velocity '(:meter :second))
      (destructuring-bind (acceleration acceleration-unit) (get-unit acceleration '(:meter (:second :second)))
	(+ (* velocity time)
	   (* .5 acceleration (sq time)))))))
;;(distance-vat (* 0.5 5) :acceleration +gravity+)
;;(distance-vat (- 5.83 2.04) :acceleration +gravity+)

(defun acceleration-st (distance time)
  (/ (* 2 (float distance)) (sq time)))
;;(acceleration-st 70 3.8)

(defun radius-from-gravity (gravity))

(defun* revolution-speed (distance-from-center &optional (planet-mass +earth-mass+))
  "P 132."
  (sqrt (/ (* +gravitational-constant+ planet-mass) distance-from-center)))
;;(revolution-speed (+ 1E6 +earth-radius+))

(defun* period-of-revolution (distance-from-center &key (planet-mass +earth-mass+)
						   (speed (revolution-speed distance-from-center planet-mass))
						   (time-unit :second))
  "P 132"
  (change-time-unit (/ (* 2 pi distance-from-center) speed) :to-unit time-unit))

(defun moment-of-inertia-hoop (mass radius)
  "P 254"
  (* mass (sq radius)))
;;(moment-of-inertia-hoop 0.030 (/ 0.065 2))3.1687500000000005e-05

(defun moment-of-inertia-cylinder (mass radius)
  (* .5 mass (sq radius)))

(defun moment-of-inertia-hollow-cylinder (mass outer-radius inner-radius)
  (* .5 mass (+ (sq outer-radius) (sq inner-radius))))
;;(moment-of-inertia-hollow-cylinder 0.030 0.0325 0.0324)

(defun moment-of-inertia-sphere (mass radius)
  (* 0.4 mass (sq radius)))

(defun angular-velocity-from-period (period-of-revolution)
  "P 246, 250"
  (/ (* 2 pi) period-of-revolution))

(defun period-from-angular-velocity (angular-velocity)
  "I.e. constant angular-velocity"
  (/ (* 2 pi) angular-velocity))
;;(period-from-angular-velocity (angular-velocity-from-period 1))

(defun rpm-to-period (rpm)
  (change-time-unit (/ 1.0 rpm) :from-unit :minute))
;;(* (rpm-to-period 33) 33)

(defun angular-velocity-from-rpm (rpm)
  (angular-velocity-from-period (rpm-to-period rpm)))
;;(angular-velocity-from-rpm 33)

(defun angular-velocity-of-car-wheel (wheel-radius car-speed)
  (let* ((wheel-circumference (circle-circumference wheel-radius))
	 (period (/ wheel-circumference car-speed)))
    (angular-velocity-from-period period)))
;;(angular-velocity-of-car-wheel 0.4 20)
;;(angular-velocity-of-car-wheel (/ 0.7 2) (/ 30 3.6))


(defun potential-energy (height mass)
  (* mass +gravity+ height))

(defun kinetic-energy (mass velocity)
  (* 0.5 moment-of-inertia angular-velocity))

(defun angular-kinetic-energy (moment-of-inertia angular-velocity)
  (kinetic-energy moment-of-inertia angular-velocity))

(defun velocity-from-sphere-rolling-down (mass radius height rotation-period)
  (velocity-from-kinetic-energy 
   (- (potential-energy mass height) 
      (angular-kinetic-energy (moment-of-inertia-sphere mass radius)
			      (sq (angular-velocity-from-period rotation-period))))
   mass))
;;(velocity-from-rolling-down 0.3 0.05 10 0.5)

(defun number-of-anagrams (string)
  "Grimaldi 17"
  (let ((accumulation (mapcar #'second (accumulate-list (coerce string 'list)))))
    (reduce #'* (remove-factors (a-b 1 (sum accumulation)) (reduce #'* (mapcar #'faculty accumulation))))))
;;(number-of-anagrams "TALLAHASSEE")

(defun number-of-anagrams-without-adjacent-character (string char)
  "Grimaldi 17"
  (let ((number-of-chars (count char string))
	(string-without-char (delete char string)))
     (* (number-of-anagrams string-without-char)
	(binomial-coefficient (1+ (length string-without-char)) number-of-chars))))
;;(number-of-anagrams-without-adjacent-character "TALLAHASSEE" ?A)

(defun how-many? (x nx y ny z nz thing)
  (format "Hvis %s og %s har tilsammen %d %s, %s og %s har tilsammen %d %s og %s og %s har tilsammen %d %s, hvor mange %s har da %s?"
	  x y (+ nx ny) thing
	  x z (+ nx nz) thing
	  y z (+ ny nz) thing
	  thing z))
;;(how-many?)

(defun selection-with-repetition (number-of-distinct-objects repetitions)
  (binomial-coefficient (1- (+ number-of-distinct-objects repetitions)) repetitions))
;;

;;p 132
;;(period-of-revolution (+ 1E6 +earth-radius+) :time-unit :minute) 
