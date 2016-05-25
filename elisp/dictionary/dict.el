(require 'etags)
(require 'lynx)
(require 'mb-utils-regexp)
(require 'mb-utils-time)
(require 'mb-utils-io)

;; TODO: replace *'s with +'s for all constants

;;; proxy db
(defconst *dic-current* "mw")
(defconst *dic-buf* "*Dictionary*")
(defconst *dic-history* nil)

(defconst *dic-db* nil)
(defconst *dict-db-path* (expand-file-name "dic/" *local-data-dir*))
(defconst *dic-db-file* (expand-file-name "dic.el" *dict-db-path*))

(defun dic-backup (tim))

(defun dic-read-db ()
  (setq *dic-db* (read* *dic-db-file*)))

(defun dic-write-db ()
  (with-temp-file *dic-db-file*
    (overwrite-safe (prin1-to-string *dic-db*))))
;;(dic-write-db)

(unless *dic-db* (dic-read-db)) ;(nilf *dic-db*)
(add-hook 'kill-emacs-hook 'dic-write-db)
(push (cons *dic-db-file* 'iso-8859-1) file-coding-system-alist)

;;; Dictionary specials
(defconst *dic-list*
  '(("bo" "Bokmålsorboka" "")
    ("ny" "Nynorskorboka" "")
    ("mw" "Merriam-Webster" "")
    ("lsj" "Liddel-Scott-Jones" "")
    ("ls" "Lewis & Short" "")
    ("lsmorph" "Lewis & Short morphological analysis" "")
    ("ne" "travlangs Dutch-English" "")
    ("se-eng" "Ectaco Swedish-English" "")
    ("it-eng" "Collins Italian-English" "")
    ("it-eng2" "Ectaco Italian-English" "")
    ("de-eng" "Ectaco German-English" "")
    ("de-eng2" "travlangs German-English" "")
    ("ne-eng" "Ectaco Dutch-English" "")
    ("ne-no" "travlangs Dutch-Norweigian" "")
    ("es-eng" "Collins French-English" "")
    ("fr-eng" "French Linguistics" "")
    ("fr-eng1" "Collins French-English" "")
    ("fr-eng2" "Ectaco French-English" "")
    ("it-eng3" "travlangs Italian-English" "")
    ("saob" "Ordbok över svenska språket" "")
    ("acronym" "Acronym Finder" "")
    ("dwds" "das Digitale Wörterbuch der Deutschen Sprache des 20. jahrhunderts" "")
    ("proxy" "Proxy DB" "")))
;;(setf debug-on-error t)

(defun dic-url (word &optional dictionary)
  "Returns correct url to lookup WORD in DICTIONARY (default
\"mw\"). Updated bo-ny 2002-09-12"
  (string-case dictionary
    (("bo" "ny") (concat "http://www.dokpro.uio.no/perl/ordboksoek/ordbok.cgi?OPP="
			 (string-encode word) 
			 "&begge=S%F8k+i+begge+ordb%F8kene&ordbok=bokmaal&alfabet=n&renset=j"))
    ("lsj" (concat "http://www.perseus.tufts.edu/cgi-bin/lexindex?lookup="
		  word "&lang=greek"));;must be beta font
    ("ls" (concat "http://www.perseus.tufts.edu/cgi-bin/lexindex?lookup="
		  word "&lang=Latin"))
    ("lsmorph" (concat "http://www.perseus.tufts.edu/cgi-bin/morphindex?lookup=" word "&lang=la&formentry=1"))
    ("it-eng" (concat "http://www.wordreference.com/it/en/translation.asp?iten=" word))
    ("it-eng2" (concat "http://www-old.ectaco.com/online/diction.php3?lang=6&q=3&pagelang=23&word=" word "&direction=2"))
    ("it-eng3" (concat "http://dictionaries.travlang.com/ItalianEnglish/dict.cgi?query=" word "&max=50"))
    ("es-eng" (concat "http://dictionaries.travlang.com/SpanishEnglish/dict.cgi?query=" word "&max=50"))
    ("se-eng" (concat "http://www.ectaco.com/online/diction.php3?refid=364&lang=21&word=" word "&direction=2"))
    ("ne-eng" (concat
	       "http://www.ectaco.com/online/diction.php3?lang=23&q=2&word="
	       (string-encode word) "&direction=2"))
    ("de-eng" (concat
	       "http://www.ectaco.com/online/diction.php3?refid=364&lang=13&word="
	       (string-encode word) "&direction=2"))
    ("de-eng2" (concat "http://dictionaries.travlang.com/GermanEnglish/dict.cgi?query="
		      word "&max=50"))
    ;;http://dictionaries.travlang.com/GermanEnglish/dict.cgi?query=sowohl&max=50
    ("dwds" (format "http://www.dwds.de/cgi-bin/portalL.pl?search=%%20%s" "Blasinstrument"))
    ("es-eng" (concat "http://www.wordreference.com/es/en/translation.asp?spen=ici" word))
    ("fr-eng" (format "http://www.frenchlinguistics.com/freng.exe?word=%s" word))
    ("fr-eng1" (concat "http://www.wordreference.com/fr/en/translation.asp?fren=ici" word))
    ("fr-eng2" (concat
	       "http://www.ectaco.com/online/diction.php3?refid=364&lang=19&word="
	       (string-encode word) "&direction=2"))
    ("ne" (concat "http://dictionaries.travlang.com/DutchEnglish/dict.cgi?query="
		  word "&max=50"))
    ("ne-no" (concat "http://dictionaries.travlang.com/DutchNorwegian/dict.cgi?query="
		  word "&max=50"))
    ("saob" "http://g3.spraakdata.gu.se/cgi-bin/saob/enkel.cgi")
    ;;  Finnish: 
    ;;  http://www-lexikon.nada.kth.se/skolverket/sve-fin.shtml                     => fast, extensive translation, limited
    ;;  http://efe.scape.net/index.php                                              => ok, but slow
    ;;  http://212.213.217.194/cgi-bin/mofind.exe/dr1?word=tietokone                => too small
    ;;  http://dictionaries.travlang.com/FinnishEnglish/dict.cgi?query=firma&max=50 => too small
    ;;  http://www.freedict.com/cgi-bin/onldicfin.cgi                               => too small
    ;;  http://www.lingnet.org/linkdex/Finnish.htm (newspapers etc)
    ("acronym" (format "http://www.acronymfinder.com/default.asp?Acronym=%s" word))                        
    (t (concat "http://www.merriam-webster.com/dictionary/" word)))) ;mw
;;(dic-url "pmi" "mw")
;;(dic-url "auspicabile" "it-eng")

(defun lynx-output-filter-dic (string dictionary)
  "Filter STRING according to DICTIONARY."
  (string-case dictionary
    ("mw" (or (not-empty
	       (string-replace 
		(substring-intv string 
		  (interval-co "^   Main Entry:" "\n\\s-*.*Learn more"))
		"\n\n" "\n"))
	      (substring-intv string (interval-co "^\\s-*Suggestions for" "\n\n"))))
    (("ne" "ne-no" "it-eng3" "de-eng2")
     (or (not-empty 
	  (substring-intv string (interval-oo "   Result of search for" "\n   \n   Search tip:")))
	 (substring-intv string (interval-oo "   Result of search for" "\n     _+$"))))
    ("es-eng" string)
    ("fr-eng" (substring-intv string (interval-oo "^\\s-*\n" "^\\s-*_+")))
    (("it-eng" "fr-eng1") (substring-intv string (interval-oo "^\\s-*Publishers:\n" "\n.*\n\n")))
    (("se-eng" "it-eng2" "ne-eng" "de-eng" "fr-eng2")
     (substring-intv string (interval-co "Results:" "\\s-*\n\\s-*If ")))
    ;; todo error handling, see 'råtning'
    ("bo" (or (not-empty (substring-intv string (interval-oo "Oppslagsord\n" "\n   \n")))
	      (not-empty (substring-intv string 
			   (interval-oo "^\\s-*TILSLAGSORD ARTIKKEL FRA BOKMÅLSORDBOKA.*\n" "\n\\s-*\n")))
	      (string-match* "   Vi har dessverre .*\n?.*bokmålsdatabasen" string)
	      (string-match* "Vi får dessverre ikke kontakt.*$" string)
	      ;; this is probably obsolete
	      (not-empty
	       (substring-intv string 
		 (interval-co "^   Ordet '.*' er ikke et oppslagsord i Bokmålsordboka" "\n\\s-*$")))))
    ("ny" (or (not-empty
	       (substring-intv string 
		 (interval-oo "^\\s-*TILSLAGSORD ARTIKKEL FRÅ NYNORSKORDBOKA.*\n" "\n\\s-*\n")))

	      (string-match* "   Vi har dessverre .*\n?.*nynorskdatabasen" string)
	      (string-match* "Vi får diverre ikkje kontakt.*$" string)
	      ;; this is probably obsolete
	      (not-empty
	       (substring-intv string 
		 (interval-co "^   Ordet '.*' er ikkje eit oppslagsord i Nynorskordboka"
			      "\n\\s-*$")))))
    ("lsj" (string-replace 
	    (or (not-empty
		 (substring-intv string	;normal entry
		   (interval-oo "^\\s-*Greek Texts.*\\s-*\n"
				"\n\\s-*\nReferences")))
		(not-empty
		 (substring-intv string	;multiple entries
		   (interval-oo "^\\s-*Multiple entries[^_]*_*\n\\s-*\n"
				"\n\\s-*\nReferences")))
		(not-empty
		 (substring-intv string	;if error
		   (interval-co "^  No entry in the dictionary"
				"\n   Maximum Minimum")))
		(error "Unknown lynx output in \"ls\"!"))
	    *lynx-ref-regexp*))
    ("ls" (string-replace 
	   (or (not-empty
		(substring-intv string	;normal entry
		  (interval-oo "^\\s-*Latin Texts.*\\s-*\n"
			       "\n\\s-*\nReferences")))
	       (not-empty
		(substring-intv string	;multiple entries
		  (interval-oo "^\\s-*Multiple entries[^_]*_*\n\\s-*\n"
			       "\n\\s-*\nReferences")))
	       (not-empty
		(substring-intv string	;if error
		  (interval-co "^  No entry in the dictionary"
			       "\n   Maximum Minimum")))
	       (error "Unknown lynx output in \"ls\"!"))
	   *lynx-ref-regexp*))
    ("lsmorph" (string-replace-intv 
		   (string-replace-intv
		       (substring-intv string
			 (interval-oo "Analyze another form.\n\\s-*\n"
				      "\n\nReferences")) 
		       (interval-co (concat "\n\\s-*" *lynx-ref-regexp* "Frequency")
				    "\\(\n\\s-*\n\\|\\s-*\\'\\)"))
		   (interval-cc "Entry in" "L&S")))
    ("saob" (substring-intv (string-replace string "http.*cgi\\?")
	      (interval-oo "References\n\n" "^.*http.*")))
    ("acronym" (string-replace 
		(string-replace
		 (substring-intv string (interval-oo "   Acronym\\s-*meaning\\s-*\n\\s-*" 
						     "\\(If you don't find it here\\)\\|\\(\\s-*\\[.*\\]Next page\\)"))
		 "^.*direct link\\s-*")
		"[\t\n ]*\\[.*\\]search[\t\n ]*Amazon.com"))
    ("dwds" string)
    (otherwise string)))

(defun dic-buffer-name (dic-name) (concat "*" dic-name "*"))

;;; General dictionary functions
(defun dic-output (dictionary string)
  "Prints STRING to dic's output buffer."
  (with-output-to-temp-buffer (dic-buffer-name (second dictionary))
    (princ string)))
;(require 'cl-indent)
;(cl-indent 'dic-output 'aif)

(defun dic-sentinel (dictionary)
  (lexical-let ((dictionary dictionary))
    #'(lambda (process event)
	(let* ((dic-object (assoc dictionary *dic-list*))
	       (string (region-to-string :buffer (get-buffer *dic-buf*)))
	       (res (list (third dic-object)
			  dictionary
			  (lynx-output-filter-dic string dictionary)
			  (now)
 			  (now))))
	  (push res *dic-db*)
	  (kill-buffer *dic-buf*)
	  (dic-output dic-object (third res))))))

(defun dic-start-lynx-process (word dic)
  (set-process-sentinel
   (if (string= dic "saob")
     (lynx-request-post (dic-url nil dic) :args (list (list "string" word)) :name dic :buffer *dic-buf*)
     (lynx-download-to-buffer-lynx *dic-buf* (dic-url word dic) :process-name dic))
   (dic-sentinel dic)))
;(cancel-debug-on-entry 'dic-start-lynx-process)

(defun* dic-lookup (word &optional (dictionary *dic-current*))
  "Look up WORD in DICTIONARY using proxy. This is the mother of all
lookup functions."
  (interactive (dic-lookup-interactive))
  (let ((dic-obj (assoc dictionary *dic-list*))
	(word (downcase word)))
    (setf (third dic-obj) word);;store last lookup word in *dic-list*; but why?
    (aif (find-if #'(lambda (x) (and (string= (first x) word) 
				     (string= (second x) dictionary))) *dic-db*)
      (progn
	(setf (fifth it) (now))
	(dic-output dic-obj (third it)))
      (dic-start-lynx-process word dictionary))))
;(cancel-debug-on-entry 'dic-lookup)
;;(dic-lookup "test" "de-eng2")

(defmacro dic-lookup-word (dictionary)
  "Returns dic-lookup with second argument bound to DICTIONARY."
  `(lambda (word) 
    (interactive (dic-proxy-completion (concat "Look up (" ,dictionary  "): ") ,dictionary))
    (dic-lookup word ,dictionary)))
;;(funcall (dic-lookup-word "de-eng2") "test")

(defun dic-lookup-at-point () (interactive)
  "Look up word at point in current dictionary."
  (dic-lookup (thing-at-point 'word)))

(defun dic-set-current (dictionary)
  "Set current DICTIONARY. DICTIONARY must be a symbol equal to some
FIRST of *DIC-LIST*. The variable *DIC-CURRENT* always contains the
current dictionary."
  (interactive 
   `(,(let ((completion-ignore-case t))
	   (completing-read
	    "Which dictionary? "
	    *dic-list* nil t (cons *dic-current* 0)))))
  (setq *dic-current* dictionary))

(defun dic-sub (dictionary)
  (if (eql dictionary ':all)
      *dic-db*
      (remove-if-not #'(lambda (x) (string= dictionary (second x)))
		 *dic-db*)))
;(mapcar #'first (dic-sub 'lsmorph))

;; What is the following doing?
(mapc #'(lambda (x) 
	 (when (symbolp (second x))
	   (setf (second x) (symbol-name (second x)))))
	*dic-db*)

(defun* dic-lookup-interactive (&optional (dictionary *dic-current*))
  `(,(read-string
      (concat "Look up: ")
      (or (word-at-point) "") 'dic-lookup-history)))

(defun* dic-proxy-completion (string &optional (dictionary ':all))
  `(,(let ((completion-ignore-case t)
	   (w (or (word-at-point) "")))
       (completing-read	string (dic-sub dictionary) nil nil (cons w (length w))))))
;;(dic-proxy-completion "Look up proxy: " ':all)

(defun dic-lookup-proxy (word)
  "Look up WORD entry in proxy only. The interactive part seems soon
ripe for macro."
  (interactive (dic-proxy-completion "Look up proxy: "))
  (dic-output (assoc "proxy" *dic-list*) (third (assoc word *dic-db*))))

(defun* dic-delete-word (word)
  "Delete WORD in current dictionary."
  (interactive (dic-proxy-completion
		(concat "Delete from " *dic-current* ": ") *dic-current*))
  (setq *dic-db* 
	(delete-if #'(lambda (x) 
		       (and (string= (first x) (downcase word)) 
			    (string= (second x) *dic-current*)))
		   *dic-db*)))

(defconst *dic-sep* "
-------------------------------------------------------------------------------")

(defun dic-make-sep (word)
  "Returns the separator string with WORD centered on it."
  (replace (copy-sequence *dic-sep*) word 
	   :start1 (/ (- (length *dic-sep*) (length word)) 2)))
;;(dic-make-sep "qwe")

(defun* dic-show-lookups
    (&key (dic :all) (time (period :from (now :week -1) :to (now))))
  (with-output-to-temp-buffer "*dictionary-lookups*"
    (mapc #'(lambda (x)
	      (princ (format "%s\nDictionary: %s, first looked up: %s, \
lastest lookup: %s\n%s\n" 
			     (dic-make-sep (first x)) (second x)
			     (iso-date (fourth x))
			     (iso-date (fifth x)) (third x))))
	  (remove-if 
	   #'(lambda (y)
	       (not 
		(and (within-time (or (fourth y) *the-creation*) time)
		     (or (eq dic :all) (string= dic (second y))))))
	   (reverse *dic-db*)))))
;;(dic-show-lookups :dic :all :time (period :from (now :hour -2) :to (now)))

(defun* dic-info (&optional (dictionary 'all) &key time-extension)
  "Prints the following information about the entries in DICTIONARY
\(as a proxy\) with dates within TIME-EXTENSION:
* The number of such entries
* The max processing time of an entry retrival (this should be
  significantly lower than the site dictionary request time.)
* The entires."
  (interactive)
  (with-output-to-temp-buffer "*dic-info*"
    (princ
     (format "Dictionary size: %d\nProxy time (ms): %d\nEntries: %s"
	     (length *dic-db*)
	     (first (time-val (assoc (first (first (last *dic-db*))) *dic-db*)))
	     (sort (mapcar #'first *dic-db*) #'string<)))))
;;(current-time (last *dic-db*))

(defun dic-replace-forward-definition (from to)
  "Forwards the definition of FROM to the definition of TO."
  (interactive (nconc
		(dic-proxy-completion
		 (concat "In " *dic-current* ", redirect defintion of: ")
		 *dic-current*)
		(dic-proxy-completion (concat "To: ") *dic-current*)))
  (aif (assoc from *dic-db*)
    (setf (third it) (concat to " <=="))))
;;(nth 4 (third *dic-db*))

(defun dic-parse-output (dic)
  "Parses result from it-eng2. Returns a list of English words."
  (set-difference 
   (nthcdr 2 (split-string		;nthcdr removes 'Results:', '<lookup>'
	      (region-to-string 
	       (dic-buffer-name (second (assoc "it-eng" *dic-list*))))
	      "[ \f\t\n\r\v;]+")) ; string => tokens
   '("N" "V")			  ; remove tokens
   :test #'string=))

(defun dic-last-entry ()
  (let* ((dic-buffer-names (mapcar #'dic-buffer-name (mapcar #'second *dic-list*)))
	 (buffer-names (mapcar #'buffer-name (buffer-list)))
	 (res (find-if #'(lambda (x) (find x dic-buffer-names :test #'string=)) buffer-names)))
    (find res *dic-list* :test #'(lambda (x y) (string= x (dic-buffer-name (second y)))))))

(defun dic-browse-last-entry ()
  "Redirects last shown lookup entry to original url in a Windows browser."
  (interactive)
  (let ((entry (dic-last-entry)))
    (browse-url (dic-url (third entry) (first entry)))))

(defun dic-url-last-entry ()
  "Redirects last shown lookup entry to original url in a Windows browser."
  (interactive)
  (let ((entry (dic-last-entry)))
    (dic-url (third entry) (first entry))))
;;(dic-url-last-entry)"

;;(dic-browse-last-entry)
;;(buffer-list)
;;(assoc "mw" *dic-list*)
;;(second (assoc "mw" *dic-list*))

(provide 'dictionary)
