(require 'ada-mysql)

(defun component-from-source-id (source-id)
  (car (emacsql db [:select * :from component :where (= source-id $r1)] source-id)))

(defun component-from-uuid (uuid)
  (car (emacsql db [:select * :from component :where (= uuid $r1)] uuid)))
;;(component-from-uuid "1cbb9df2-669c-3b44-8c76-ac7f9c39e103")

(defun component-from-string-id (string-id)
  (if (= (length string-id) 36)
    (component-from-uuid string-id)
    (component-from-source-id string-id)))
;;(component-from-string-id "1cbb9df2-669c-3b44-8c76-ac7f9c39e103")

(defun component-from-id (id)
  (car (emacsql db [:select * :from component :where (= id $s1)] id)))
;;(last-elt (component-from-source-id "628cf6be7e1b91969769be19"))

(defun component (id-descriptor)
  (typecase id-descriptor
    (string (component-from-string-id id-descriptor))
    (number (component-from-id id-descriptor))
    (list (mapcar #'component-from-id id-descriptor))
    (otherwise (component-from-id (id id-descriptor)))))
;;(component "1cbb9df2-669c-3b44-8c76-ac7f9c39e103")
;;(mapcar (compose #'car #'component) (list 15752 "617bac2c4075f937656d9d36" "1cbb9df2-669c-3b44-8c76-ac7f9c39e103"))

(defun component-json (id-descriptor)
  (caar (emacsql db [:select json-id :from component :where (= id $s1)] (id id-descriptor))))
;;(component-json 15752)

(provide 'ada-component)
