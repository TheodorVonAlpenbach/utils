(concat
   "http://www.mapquest.com/cgi-bin/ia_find?link=btwn%2Ftwn-map_results&event=find_search&uid=u55am2lex9e7m0ve%3A8l5ryxua8&SNVData=3mad3-1.fy%25284al06b_dy25g.hqu%253b%2528_%253d%253a%2528RN%253a8l1z7x%253d4znqy%2528l%25241w-u&city="
   address "&country=" (im-encode-district-mapquest district))
;    ("no" (concat "www.fleximap.com/finnadresse/default.asp?adresse=" address))


(defun im-encode-district-mapquest (district)
  (aif (assoc district '(("no" "norway")
			 ("ne" "netherlands")))
    (second it) district))
;;(im-encode-district-mapquest "no")

(defun im-set-current-district-old
(district)
  "Set current DISTRICT. DISTRICT must be a symbol equal to some FIRST
of *im-districts*. The variable *im-current-district* always contains
the current district."
  (interactive 
   `(,(let ((completion-ignore-case t))
	   (completing-read
	    "Which district? "
	    *im-districts* nil t (cons *im-current-district* 0)))))
  (setq *im-current-district* district))

(defmacro im-show-place (district)
  "Shows the sexp at point string in the intermap corresponding to
DISTRICT."
  `(lambda (address)
    (interactive (list (read-string "Show place: " (or (word-at-point) ""))))
    (im-lookup address ,district)))

(defvar *im-map* (make-sparse-keymap "Intermap"))
(define-key global-map "\C-cm" *im-map*)
(define-key *im-map* "\C-s" 'im-set-current-district)
(define-key *im-map* "n" (im-show-place "no"))
(define-key *im-map* "N" (im-show-place "norway"))
(define-key *im-map* "s" (im-show-place "se"))
(define-key *im-map* "d" (im-show-place "dk"))
(define-key *im-map* "t" (im-show-place "germany"))
(define-key *im-map* "f" (im-show-place "france"))
(define-key *im-map* "u" (im-show-place "uk"))
(define-key *im-map* "e" (im-show-place "ireland"))
(define-key *im-map* "i" (im-show-place "italy"))
(define-key *im-map* "h" (im-show-place "ne"))
(define-key *im-map* "b" (im-show-place "belgium"))
