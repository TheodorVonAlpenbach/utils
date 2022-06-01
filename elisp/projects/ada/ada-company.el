(require 'ada-mysql)

(defun company (id-descriptor)
  (emacsql db [:select * :from company :where (= id $s1)] (id id-descriptor)))
;;(company 2585)

(defun companies ()
  (emacsql db [:select * :from company]))
;;(companies)

(defun company-from-name (name)
  (emacsql db [:select * :from company :where (= name $s1)] (id id-descriptor)))
;;(company 1)
 
(provide 'ada-company)
