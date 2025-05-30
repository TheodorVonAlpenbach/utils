(require 'ada-mysql)

(cl-defun company (id-descriptor)
  (emacsql db [:select * :from company :where (= id $s1)] (id id-descriptor)))
;;(company 2585)

(cl-defun companies ()
  (emacsql db [:select * :from company]))
;;(companies)

(cl-defun company-from-name (name)
  (emacsql db [:select * :from company :where (= name $s1)] (id id-descriptor)))
;;(company 1)
 
(provide 'ada-company)
