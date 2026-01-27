(defconst +test-lines+ (file-lines "~/tmp/testlog.txt"))
(defun is-ada-log-line (string)
  (string-match "^[\t ]*at no\\.cd" string))
;;(is-ada-log-line "	at no.cd.ada.portalapi.SelectAc")

(cl-defun ada-prune-dd-log-1 (lines)
  (cl-destructuring-bind (header body)
      (split-at-position lines 2)
    (cl-destructuring-bind (first . rest)
	(split-if #'is-ada-log-line body)
      (append
       header
       (headtail first)
       (cl-loop for x in rest
		collect "\t..."
		append (headtail x 2 1 "\t..."))))))
;;(ada-prune-dd-log-1 +test-lines+)
;;(concat* (ada-prune-dd-log-1 +test-lines+) :in "\n")
;;(nth 4 (ada-prune-dd-log-1 +test-lines+))

(defun ada-prune-dd-log ()
  (interactive)
  (overwrite-region
   (concat* (ada-prune-dd-log-1 (buffer-lines)) :in "\n")
   (point-min) (point-max)))

(defconst +test-log+ "Last feedback from ElasticSearch: [FeedbackResultModel[feedbackId=bb855aaf-4eb7-42b0-8d1c-0495ec1664e0, pageId=66e173d3e027f9bb207b9c47, text=<p>Du er kjempeflink til å lese!!</p>, sticker=no.cd.ada.service.models.feedback.Sticker@59959b83, createdByPseudonym=e4f6cd37-9132-4493-915a-5efaf489809a, pupilPseudonym=9a960769-4ad8-41dd-9d0a-43ba7feecbbd, componentSourceId=66e173d3e027f9bb207b9c40, componentVersion=1, feedbackRead=false, createdTime=1763709438501, updatedTime=1763709438501]]")

(cl-defun parse-last-feedback (&optional (log (clipboard-to-string)))
  (let ((pp (string-match* "pupilPseudonym=\\([^,]+\\)," log :num 1))
	(sid (string-match* "componentSourceId=\\([^,]+\\)," log :num 1)))
    (format "db.userComponentState.find({'_id.userPseudonym': '%s', componentSourceId: '%s'}, {'attempts.feedbacks': 1})"
				     pp sid)))
;;(parse-last-feedback +test-log+)
;;(parse-last-feedback)

;; db.feedback.find({"feedbacks.feedbackId": "feedbackId=22ba4a0d-a1fc-4b01-86dd-bd72048f65c7"})
;; db.userComponentState.find({"_id.userPseudonym": '8197aef6-8467-4c34-8341-df0c2d0bb7eb', componentSourceId: '61373114d132e8723a698166'}, {"attempts.feedbacks": 1})
