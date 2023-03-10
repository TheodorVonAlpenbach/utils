(require 'mongo)

;;;; Experiment with emacs mongo

(let* ((result
	(mongo-with-open-database
	    (db :host 'local :port 30101)
	  (mongo-do-request
	   (make-mongo-message-query
	    :flags 0
	    :number-to-skip 0
	    :number-to-return 0
	    :full-collection-name "kafka-connect.state-user-component-170"
	    :query '(("userPseudonym" . "e7efc68b-44e5-4086-a361-adca1be65eb0")))
	   :database db)))
       (docres (mongo-message-reply-documents result)))
  (loop for x in docres
	for uuid = (cdr (assoc-string "componentUuid" x))
	for status = (cdr (assoc-string "componentStatus" (elt (cdr (assoc-string "userComponentStates" x)) 0)))
	collect (list uuid status)))

(provide 'ada-mongo)
