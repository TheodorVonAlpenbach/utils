;;;; DB constructed entirely of lists
(defconst +ld-repository+ (expand-file-name "ld" *local-data-dir*))

(defvar *current-database* ;;(nilf *current-database*)
  (ld-create-database :empty-db :name "A dummy and emtpy database")
  "The object that is currently used as the source for
operations. Many operations are not dependant on this varialbe.
For instance, if emps is a variable holding a table with
keyword :employees, the following two select operations are
equivalent 

\(ld-select emps :where (oddp :id)\) ;returns every record with odd column id
 <==>
\(ld-select :employee :where (oddp :id)\) ;returns every record with odd column id

given that emps is a table with keyword :employee in
*current-database*.

\(The stuff in the next paragraph is not yet implemented)
You could refer to other databases as well
\(ld-select (:sweden :employee) :where (oddp :id)\) ;returns every record with odd column id
")

(defvar *ld-check-data-p* t
  "Global variable to do some data checks")

(defvar *ld-autogenerate-base* 1)
(defvar *ld-autogenerate-increment* 1)

(provide 'ld-global)
