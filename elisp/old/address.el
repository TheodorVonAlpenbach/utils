;;;; Adresse mode

;;;; Comment lines starts with character ?% (Currently comments are
;;;; only used for capital dividers, e.g. %A%, %B%, ...)

;;;; adr-buffer     --> adr-record*
;;;; adr-record     --> \! <newline> adr-head-entry adr-entry*
;;;; adr-head-entry --> adr-name-item adr-item*
;;;; adr-entry      --> #<integer> <newline> adr-item*
;;;; adr-name-item  --> NAME : adr-item-body
;;;; adr-item       --> adr-item-tag : adr-item-body
;;;; adr-item-body  --> .* <newline> (<tab> <newline> .*)*
;;;; adr-item-tag   --> NAME, DESCR, EMAIL, ADR, TLF, FAX, VALID, CODE

(defconst adr-item-names '(NAME BORN DESCR EMAIL ADR TLF FAX CODE VALID))

(defconst adr-item-name-regexp
  (concat* (mapcar #'(lambda (x) (format "\\(?:%s\\)" x)) (mapcar #'symbol-name adr-item-names)) :in "\\|"))

(defconst adr-item-regexp
  (format "^\\(%s)\\s-*:\\s-*\\(.*\n\\)*" adr-item-name-regexp))


;;; main
(defun* adr-parse (&optional (buffer (current-buffer)))
  (adr-parse-buffer-string (adr-buffer-string buffer)))
;;(adr-parse "adresser.adr")


;;; buffer level
(defun adr-buffer-string (buffer)
  "Returns the adress buffer string. Comments are removed."
  (string-replace 
   (with-buffer buffer
     (buffer-substring-no-properties 
      (point-min)
      (point-max)))
   "^%.*\n" ))
;;(adr-buffer-string "adresser.adr")

(defun adr-buffer-string-to-record-strings (buffer-string)
  (split-string buffer-string "\\s-*\n?\\\\!\n") )

(defun adr-buffer-to-record-strings (buffer)
  (adr-buffer-string-to-record-strings (adr-buffer-string buffer)))

(defun adr-parse-buffer-string (buffer-string)
  (mapcar #'adr-parse-record-string (adr-buffer-string-to-record-strings buffer-string)))
;;(adr-parse-buffer-string (adr-buffer-string "adresser.adr"))


;;; record level
(defun* adr-record-string-sample (buffer &optional (n 0))
  (nth n (adr-buffer-to-record-strings buffer)))
;;(adr-record-string-sample "adresser.adr" 2)

(defun adr-record-string-to-entry-strings (record-string)
  (split-string record-string "#[0-9]+\\s-*\n"))
;;(adr-record-string-to-entry-strings (adr-record-string-sample "adresser.adr" 2))

(defun adr-parse-record-string (record-string)
  (mapcar #'adr-parse-entry-string (adr-record-string-to-entry-strings record-string)))
;;(adr-parse-record-string (adr-record-string-sample "adresser.adr"))


;;; entry level
(defun* adr-entry-string-sample (buffer &optional (nth-record 0) (nth-entry 0))
  (nth nth-entry
       (adr-record-string-to-entry-strings 
	(adr-record-string-sample buffer nth-record))))
;;(adr-entry-string-sample "adresser.adr" 2 0)

(defun adr-entry-string-to-item-strings (entry-string)
  (rest (split-string* entry-string (format "^\\(%s\\)\\s-*:" adr-item-name-regexp) :right)))
;;(adr-entry-string-to-item-strings (adr-entry-string-sample "adresser.adr" 2 0))

(defun* adr-parse-entry-string (entry-string &optional (clean-item-value t))
  (loop for item-string in (adr-entry-string-to-item-strings entry-string)
	for item-pv-pair = (adr-item-string-to-property-value-pair item-string)
	for item-p = (first item-pv-pair)
	for item-v = (second item-pv-pair)
	when clean-item-value do (setq item-v (string-fill-paragraph item-v))
	collect (list item-p item-v)))
;;(adr-parse-entry-string (adr-entry-string-sample "adresser.adr"))


;;; item level
(defun* adr-item-string-sample (buffer &optional (nth-record 0) (nth-entry 0) (nth-item 0))
  (nth nth-item
       (adr-entry-string-to-item-strings 
	(adr-entry-string-sample buffer nth-record))))
;;(adr-item-string-sample "adresser.adr")

(defun adr-item-string-to-property-value-pair (item-string)
  (string-match* 
   (format "^\\(%s\\)\\s-*:\\s-*\\(\\(?:.*\n\\)*\\)" adr-item-name-regexp)
   item-string '(1 2)))

;;(adr-item-string-to-property-value-pair (adr-item-string-sample "adresser.adr"))

(string-match* (format "\\(%s\\)\\s-*:\\s-*\\(.*\n\\)*" adr-item-name-regexp) s)