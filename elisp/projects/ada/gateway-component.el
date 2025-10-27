(require 'ada-mysql)

(cl-defun gateway-component-ids (gateway-id)
  "Return the ids of all components in gateway with GATEWAY-ID"
  (emacsql db
    (vector :select 'component-id
		 :from 'gateway-components
		 :where '(= gateway-id $s1))
    gateway-id))
;;(ids (gateway-component-ids 4))

(provide 'gateway-component)
