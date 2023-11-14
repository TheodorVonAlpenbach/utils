(require 'mongo)

(defun ada-mongo-count-ucs-tasks (component-uuid user-pseudonym collection &optional env)
  "Return number of tasks for each state in one UserComponentState object"
  (let* ((db (if (string= collection "userComponentState")
	       "portal-state" "kafka-connect"))
	 (full-connection-name (format "%s.%s" db collection))
	 (result
	  (mongo-with-open-database
	      (db :host 'local :port (if env 30101 27017))
	    (mongo-do-request
	     (make-mongo-message-query
	      :flags 0
	      :number-to-skip 0
	      :number-to-return 0
	      :full-collection-name full-connection-name
	      :query `(("_id.componentUuid" . ,component-uuid)
		       ("_id.userPseudonym" . ,user-pseudonym))
	      ;; this will not work, since '(a . (b c)) == '(a b c)
	      ;; the idea of using (a . b) for key value is flawed due to this
	      ;; (and arrays, [b c], are reserved for values, e.g. {a : [b, c, d]})
	      :return-field-selector '(("userComponentStates.taskAnswers" . 1)))
	     :database db)))
	 (docres (mongo-message-reply-documents result)))
    (cl-loop for x in docres
	     for states = (cdr (assoc-string "userComponentStates" (car docres)))
	     for task-answer-lengths = (loop for state across states
					     collect (length (cdr (assoc-string "taskAnswers" state))))
	     collect (list task-answer-lengths component-uuid user-pseudonym))))
;;(ada-mongo-count-ucs-tasks "c19b653d-e606-33cd-8e96-5d9cab42f06a" "d313d833-0b52-4541-8d36-1272a8333db0" "state-user-component-170")
;;(ada-mongo-count-ucs-tasks "c19b653d-e606-33cd-8e96-5d9cab42f06a" "d313d833-0b52-4541-8d36-1272a8333db0" "userComponentState")

(provide 'ada-mongo)
