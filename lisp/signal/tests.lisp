(in-package :signal)

(let ((x '(1 10 20)))
  (with-tests (:name "db")
    ;; first check main formula
    (test 0 (db 1))
    (test 20 (db 10))
    (test -20 (db 1/10))
    ;; test illegal unit
    (test-error (db 0) :condition-type 'system::simple-division-by-zero)
    ;; test default unit
    (test (mapcar #'db x) (mapcar (bind #'db :voltage) x) :test #'equal)
    ;; test relation between :power and :voltage (should be a factor of two)
    (test (mapcar (compose (bind #'/ 2) #'db) x) (mapcar (bind #'db :power) x) :test #'equal)
    ;; test default default unit
    (test (let ((*default-db-unit* :power))
	    (mapcar #'db x))
	  (mapcar (bind #'db :power) x)
	  :test #'equal)))

