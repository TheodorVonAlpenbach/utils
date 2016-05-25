(defconst g-stay 0)
(defconst g-east 1)
(defconst g-west (- g-east))
(defconst g-south 1)
(defconst g-north (- g-south))

(defconst earth-mass 5.9736E24)  ;kg
(defconst earth-radius 6.371E6)
(defconst g-constant 6.674E-11)
(defconst gravity-universe-size '(200 50))
(defconst gravity-buffer-name "*gravity*")

(defun* vec-sum (&rest vecs)
  (if (= (length vecs) 1)
    (first vecs)
    (if (> (length vecs) 1)
      (mapcar* #'+ (first vecs) (apply #'vec-sum (rest vecs))))))
;;(vec-sum '(1 2 3) '(1 1 1))

(defun vec-distance (u v)
  (vec-length (vec- u v)))
;;(vec-distance '(0 0) '(1 -1))

(defun vec- (u v)
  (list (- (first u) (first v))
	(- (second u) (second v))))
;;(vec- '(40 0) '(20 0))

(defun vec-length (v)
  (sqrt (apply #'+ (mapcar #'sq v))))
;;(vec-length '(1 1))

(defun vec-scalar-mult (v c)
  (mapcar #'(lambda (x) (* x c)) v))
;;(vec-scalar-mult '(1 2 3) 2)

(defun vec-unit (v)
  (vec-scalar-mult v (/ 1 (vec-length v))))
;;(vec-length (vec-unit '(1000 1000)))

(defun g-buffer-column (gpos)
  (round (first gpos)))

(defun g-buffer-line (gpos)
  (1+ (round (second gpos))))

(defun g-buffer-position (gpos)
  (+ (* (1- (g-buffer-line gpos))
	(1+ (first gravity-universe-size)))
     (g-buffer-column gpos)))

(defstruct (g-object (:type list) :named (:conc-name go-))
  (type)
  (name)
  (center)
  (mass)
  (velocity))
;'(satelite (pos speed mass))

(defun go-real-center (x)
  (vec-scalar-mult (go-center x) gravity-distance-scale))

(defun go-relative-center (x)
  (vec-scalar-mult x (/ 1.0 gravity-distance-scale)))

(defun g-clear ()
  "Acts directly on buffer. Make sure the buffer is *gravity*"
  (with-buffer gravity-buffer-name
    (buffer-clear)
    (loop for x below (second gravity-universe-size)
	  do (insert (make-string (first gravity-universe-size) 32))
	  do (insert "\n"))))

(defun g-print-object (x)
  (with-buffer gravity-buffer-name
    (goto-char (g-buffer-position (go-center x)))
    (delete-char 1)
    (insert (case (go-type x)
	      ('satelite "x")
	      ('planet "P")
	      ('sun "*")))
    (goto-char (point-min))))

(defun g-refresh ()
  (g-clear)
  (loop for x in gravity-objects
	do (g-print-object x)))

(defun g-target ()
  (first gravity-objects))

(defun g-move (o x y)
  "Moves object x columns to the right and y lines down. Negative
values means opposite directions."
  (incf (first (go-center o)) x)
  (incf (second (go-center o)) y))

(defun g-move-target (x y)
  (g-move (g-target) x y)
  (g-refresh))
                                                                                                                                                                                                        
(defconst gravity-mode-map 
  (let ((map (make-sparse-keymap)))
    ;;"d" is center
    (define-key map "f" #'(lambda () (interactive) (g-move-target g-east g-stay)))
    (define-key map "s" #'(lambda () (interactive) (g-move-target g-west g-stay)))
    (define-key map "e" #'(lambda () (interactive) (g-move-target g-stay g-north)))
    (define-key map "x" #'(lambda () (interactive) (g-move-target g-stay g-south)))
    (define-key map "r" #'(lambda () (interactive) (g-move-target g-east g-north)))
    (define-key map "c" #'(lambda () (interactive) (g-move-target g-east g-south)))
    (define-key map "w" #'(lambda () (interactive) (g-move-target g-west g-north)))
    (define-key map "z" #'(lambda () (interactive) (g-move-target g-west g-south)))
    (define-key map "j" #'(lambda () (interactive) (gravity-job)))
    (define-key map "p" #'gravity-pause)
    (define-key map "ø" #'gravity-stop)
    map))

(defun gravity-mode () "Major mode for gravity game.
 \\{gravity-mode-map}
 \\<gravity-mode-map>"
       (interactive)
       (kill-all-local-variables)
       (use-local-map gravity-mode-map)
       (setq gravity-objects nil)
       (setq gravity-distance-scale earth-radius)
       (setq gravity-time-scale (* 6 60 60.0))
       (setq local-abbrev-table text-mode-abbrev-table)
       (setq major-mode 'gravity-mode)
       (setq mode-name "Gravity mode")
       (setq buffer-offer-save t)
       (setq timer-tick-length 0.2)
       (setq timer-max-repeats 100)
       (setq gravity-timer nil))

(defun g-distance-vec (x y)
  (if (equal x y)
    (list 0 0)
    (vec-scalar-mult (vec- (go-center y)
			   (go-center x))
		     gravity-distance-scale)))
;;(apply #'g-distance-vec gravity-objects)
;;(g-distance-vec (first gravity-objects) (first gravity-objects))

(defun g-distance (x y)
  (vec-length (g-distance-vec x y)))
;;(apply #'g-distance gravity-objects)

(defun NaN-p (x)
  (not (= x x)))
;;(mapcar #'NaN-p (list 1 1.0 0 0.0 (/ 1.0 0) (/ -1.0 0)))

(defun g-direction-vec (x y)
  (vec-unit (g-distance-vec x y)))
;;(vec-length (apply #'g-direction-vec gravity-objects))
;;(NaN-p (first (g-direction-vec (first gravity-objects) (first gravity-objects))))
;;(NaN-p 1.0)

(defun g-acceleration-scalar (x y)
  "Returns acceleration matrix for all objects X towards all other objects"
  (if (equal x y)
    0
    (/ (* g-constant
	  (go-mass y))
       (sq (g-distance x y)))))
;;(apply #'g-acceleration-scalar gravity-objects)
;;(g-acceleration-scalar (first gravity-objects) (first gravity-objects)) 

(defun g-acceleration (x y)
  "Returns acceleration matrix for all objects X towards all other objects"
  (vec-scalar-mult (g-direction-vec x y)
		   (g-acceleration-scalar x y)))
;;(apply #'g-acceleration gravity-objects)
;;(g-acceleration (first gravity-objects) (first gravity-objects))

(defun g-force (x y)
  (* (go-mass x) (g-acceleration x y)))
;;(/ (* g-constant earth-mass) (sq earth-radius)) ==> 9.8

(defun g-sum-acceleration (x objects)
  "Returns acceleration matrix for all objects X towards all other objects"
  (apply #'vec-sum
	 (loop for y in objects
	       if (not (equal x y))
	       collect (g-acceleration x y))))
;;(g-sum-acceleration (first gravity-objects) gravity-objects)

(defun g-update-object (x objects time)
  (let* ((a (g-sum-acceleration x objects))
	 (velocity-difference (vec-scalar-mult a time))
	 (new-velocity (vec-sum (go-velocity x) 
				velocity-difference))
	 (new-center (vec-sum (vec-scalar-mult velocity-difference (* 0.5 time))
			      (vec-scalar-mult (go-velocity x) time)
			      (go-real-center x))))
    (setf (go-velocity x) new-velocity)
    (setf (go-center x) (go-relative-center new-center))))
;;()

(defun g-update-objects (objects time)
  (loop for x in objects
	do (g-update-object x objects time)))

(defun gravity ()
  (switch-to-buffer gravity-buffer-name)
  (gravity-mode)
  (setq gravity-objects
	(list (make-g-object :type 'satelite
			     :name 'sputnik
 			     :center '(50 20)
			     :mass 1000.0
			     :velocity '(0 2000))
	      (make-g-object :type 'planet
			     :name 'earth
			     :center '(40 20)
			     :mass earth-mass
			     :velocity '(0 0)))))
;;(gravity)

(defun gravity-job ()
  (g-update-objects gravity-objects (* gravity-time-scale timer-tick-length))
  (g-refresh)
  (message "%S" gravity-objects))

(defun gravity-start ()
  (setq gravity-timer (run-at-time "1 sec" timer-tick-length #'gravity-job)))

(defun gravity-stop ()
  (interactive)
  (cancel-timer gravity-timer)
  (setq gravity-timer nil)
  (message "Gravity stopped!"))

(defun gravity-pause ()
  (interactive)
  (if gravity-timer
    (gravity-stop)
    (gravity-start)))

(defun g-insert (pos object)
  "Insert object centered at pos"
  )
