(require 'ada-mysql)

(defun ntp-module-codes-1 (user-descriptor)
  (emacsql db
    [:select * :from user-ntp-module-codes :where (= user-id $s1)]
    (id user-descriptor)))
;;(ntp-module-codes 321210)
;;(ada-columns 'user-ntp-module-codes)

(defun ntp-module-codes-2 (user-descriptor gateway-descriptor)
  (emacsql db
    [:select * :from user-ntp-module-codes
      :where (= user-id $s1)
      :and (= gateway-id $s2)]
    (id user-descriptor)
    (id gateway-descriptor)))

(defun ntp-module-codes (user-descriptor &optional gateway-descriptor)
  (if gateway-descriptor
    (ntp-module-codes-2 user-descriptor gateway-descriptor)
    (ntp-module-codes-1 user-descriptor)))
;;(ntp-module-codes 78636 4)
;;(user 78636)

(provide 'ada-ntp-modules)
