(defvar *dic-map* (make-sparse-keymap "Dictionary"))

(require 'dictionary "dict")
(require 'store-norske)

(define-key global-map "\C-cd" *dic-map*)
(define-key *dic-map* "a" (dic-lookup-word "acronym")) ;acronym finder
(define-key *dic-map* "b" (dic-lookup-word "bo"))	;bokmaalsordboka
(define-key *dic-map* "B" (dic-lookup-word "ny"))	;nynorskordboka
(define-key *dic-map* "d" (dic-lookup-word "de-eng2")) ;travlangs
(define-key *dic-map* "D" (dic-lookup-word "de-eng")) ;dwds
;;(define-key *dic-map* "d" (dic-lookup-word "de-eng")) ;ectaco
(define-key *dic-map* "e" (dic-lookup-word "es-eng")) ;collins
(define-key *dic-map* "f" (dic-lookup-word "fr-eng"))
;(define-key *dic-map* "f" (dic-lookup-word "fr-eng2")) ;ectaco
(define-key *dic-map* "g" (dic-lookup-word "lsj"))	;liddel-scott-jones
(define-key *dic-map* "i" (dic-lookup-word "it-eng2")) ;ectaco
(define-key *dic-map* "l" (dic-lookup-word "ls"))	;lewis & short
(define-key *dic-map* "L" (dic-lookup-word "lsmorph")) ;l&s morph analysis
(define-key *dic-map* "n" (dic-lookup-word "ne-eng"))	;ectaco
(define-key *dic-map* "\C-n" (dic-lookup-word "ne-no"))	;travels
(define-key *dic-map* "p" 'dic-lookup-proxy) ;travels
(define-key *dic-map* "s" 'sn-lookup-word) ;store norske
;;(define-key *dic-map* "s" (dic-lookup-word "se-eng")) ;ectaco
;;(define-key *dic-map* "S" (dic-lookup-word "saob")) ;ectaco
(define-key *dic-map* "w" (dic-lookup-word "mw"))	;merriam webster

(define-key *dic-map* "\C-b" #'dic-browse-last-entry)
(define-key *dic-map* "\C-i" #'dic-info)
(define-key *dic-map* "\C-d" #'dic-delete-word)
(define-key *dic-map* "\C-s" #'dic-set-current)
(define-key *dic-map* "\C-p" #'(lambda () (interactive) (dic-show-lookups)))
(define-key *dic-map* "\C-r"
  #'(lambda (word)
      "Forwards results from it-eng to mw."
      (interactive 
       (list (completing-read 
	      "Forward to mw: "
	      (mapcar #'list (dic-parse-output "it-eng")))))
      (dic-lookup word "mw")))

;; interactive maps
(require 'intermap)
(define-key global-map "\C-cm" #'im-show-place)

;; interactive phone number 
(require 'phone)
(define-key global-map "\C-ct" #'ph-search)

(provide 'dic-map)
