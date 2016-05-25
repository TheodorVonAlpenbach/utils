;;; simple query examples

[control-c d control-p] 
;; or equivalently
(dic-show-lookups)

(dic-show-lookups :dic 'all :time *always*)


(dic-show-lookups :dic "it-eng" 
		  :time `(,(period :from (now :week -1) :to (now :day -2))
			  ,(yesterday)
			  ,(today)))
;; or equivalently
(dic-show-lookups :dic "it-eng" 
		  :time (period :from (now :week -1) :to (now)))
