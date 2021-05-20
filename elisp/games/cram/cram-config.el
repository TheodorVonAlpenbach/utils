(defconst +cram-buffer+ "Cramming Tool De Luxe")

(defvar *cram-auto-continue* nil)

(defvar *cram-same-problem-limit* 2
  "The number of problems you will at least get before you
encounter the same problem again")
;;(setf *cram-same-problem-limit* 1)

(require 'glicko)
(defconst +cram-default-rating+ +glicko-init-rating+)
(defconst +cram-default-user-name+ "Mats")
(defconst +cram-default-rating-window+ 200)
(defconst +cram-default-ref-filter+ "^csv-norske-fugler-"
  "A regular expression that must match every drawable problem")
(defvar *cram-ref-filter* "")

(defvar *cram-last-update* (the-creation))
(defvar *cram-match-cache* nil)

(provide 'cram-config)

