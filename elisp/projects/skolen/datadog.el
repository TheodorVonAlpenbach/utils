(defconst DD_API_KEY "328f3465d5024c3fabb0a51fdfdaecd5")
(defconst DD_APP_KEY "064df3c3642bc44c2a2553be974b36de32352cdc")
(defconst datadog-default-interval-ms 600000
  "10 minutes")
(cl-defun datadog-curl-cmd (&optional
			   (msunix1 (- (ms-unix-time) datadog-default-interval-ms))
			   (msunix2 (+ msunix1 datadog-default-interval-ms)))
  (format "curl -s -X POST https://api.datadoghq.eu/api/v2/logs/events/search -H \"Content-Type: application/json\" -H \"DD-API-KEY: %s\" -H \"DD-APPLICATION-KEY: %s\" -d '
{\"filter\":{\"query\":\"service:portal-kafka-consumer CLEAN_GROUP\",\"from\":%d,\"to\":%d
'}}" DD_API_KEY DD_APP_KEY msunix1 msunix2))
;;(string-to-clipboard (datadog-curl-cmd-to-clipboard))

(defun clean-group-cmd (head &rest args)
  (format "%s | jq | parsejson | head -%d" (apply #'datadog-curl-cmd args) head))
;;(string-to-clipboard (clean-group-cmd 1))
;;(ms-unix-time)

(cl-defun clean-group-report (&optional (head 10) (hours 48) (sample-size 20))
  ""
  (let* ((ms-length (* hours 3600 1000))
	 (start-ms-unix-time (- (ms-unix-time) ms-length datadog-default-interval-ms))
	 (sample-length (/ ms-length (1+ sample-size))))
    (concat*
	(loop for i to sample-size
	      for ms-unix-time = (+ start-ms-unix-time (* i sample-length))
	      collect (clean-group-cmd head ms-unix-time))
      :in "\n")))
;;(string-to-clipboard (clean-group-report 1 130 30))

(provide 'datadog)
