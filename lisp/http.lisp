(defclass http-query ()
  ((fields :initarg :fields ; either a list or a http query string
	   :accessor http-fields
	   :documentation "A list of http fields, where each field is a pair (field-name . field-value)"))
  (:documentation "An implementaion of a HTTP query string class"))
;;(http-fields (make-instance 'http-query-string :fields "foo=bar&qwe=rty"))

(defmethod http-fields ((s string))
  "Parses an HTTP query string to a list of fields.
This overloads the default behaviour of http-fields, which is to read
a list of fields directly.
TODO: handle encodings?"
  (loop for field-value in (regexp:regexp-split "&" s)
	collect (regexp:regexp-split "=" field-value)))
;;(http-fields "foo=bar&qwe=rty")
;;(http-fields (make-instance 'http-query :fields "foo=bar&qwe=rty"))

(defmethod make-http-query ((s string))
  (make-instance 'http-query :fields (http-fields s)))
(defmethod make-http-query ((fields list))
  (make-instance 'http-query :fields fields))
;;(make-http-query "foo=bar&qwe=rty")

(defmethod http-field ((qs http-query) (field-name string))
  "Returns the HTTP field name-value pair matching FIELD-NAME"
  (find field-name (http-fields qs) :key #'first :test #'string=))
;;(http-field (make-http-query "foo=bar&qwe=rty") "foo")

(defmethod http-field-value ((qs http-query) (field-name string))
  "Returns the HTTP field value matching FIELD-NAME"
  (second (http-field qs field-name)))
;;(http-field-value (make-http-query "foo=bar&qwe=rty") "qwe")

(provide "http")

