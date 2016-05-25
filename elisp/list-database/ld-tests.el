(defun make-companies ()
  (let ((cols '((:id :type integer :primary-key t)
		(:name :type string)
		(:president-id :type integer)))
	(data '((1 "Contango" 1)
		(2 "Navita" 3)
		(3 "VIZ" 2))))
    (ld-create-table (ld-make-schema :company :column-definitions cols) data)))
;;;;(setf comps (make-companies))

(defun make-employees ()
  (let* ((cols '((:id :type integer :primary-key t)
		 (:name :type string)
		 (:company-id :type integer)))
	 (data '((1 "Mats" 1)
		 (2 "Birger" 3)
		 (3 "Knut" 2)
		 (4 "Hilde" 1))))
    (ld-create-table (ld-make-schema :employee :column-definitions cols) data)))

(defvar emps (make-employees)) ;;(setf emps (make-employees))
(defvar comps (make-companies)) ;;(setf comps (make-companies))
(defvar deebee nil) ;;(setf deebee (ld-create-database :deebee :tables (list (make-employees) (make-companies))))

(defun ld-example-0 ()
  ;; This works today
  (ld-select (ld-join `(,emps :company-id) `(,comps :id))
	     :where (oddp (:employee :id))
	     :columns ((:employee :name) (:company :name) (sq (:company :president-id)))
	     :format "%s works for %s (squared id is %d)\n"))
;;(ld-example-0)

(defun ld-example-1 ()
  ;; This works today
  (ld-select comps :where (oddp (:company :id))))
;;(ld-example-1)

(defun ld-example-2 ()
  "This will work some day... It requires that 
* ld-join becomes a macro
* column extraction becomes a macro
* possibilti of creating new columns"
  (ld-select (ld-join (list emps :company-id) (list comps :id))
	     :where (and (oddp (:employee :id)) ;;id is ambigous needs two-element list
			 (oddp (:president-id))) ;;president-id is not ambigous
	     :columns ((:employee :name) ;;column extraction must be a macro)
		       (:company :name)
		       ("Squared president ID!" (sq (:president-id)))) ;; TBA: column creation on the fly!
	     :format "%s is an employee in %s (squared id is %d)\n"))
