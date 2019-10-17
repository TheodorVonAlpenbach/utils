;;;; Utils for å oppdatere tabeller i Munkholmserien.
;;;; De viktigste er
;;;; qp-download-table-file, som laster ned siste tabell via ftp, og rett inn i editerbar buffer
;;;; qp-upload-table-file, som laster opp buffer
;;;; qp-customer-results-to-table-buffer, som "vasker" data fra kunden og legger inn i tabellbuffer.

;;;; NB! res_alle.txt er en fil som må lastes opp som iso-latin-1-dos.
;;;; Dette innebærer at data som lastes inn må encodes som
;;;; iso-latin-1-dos. Dette er nå implementert i
;;;; konverteringsfunksjonene. Sette encodingen bufferen til
;;;; iso-latin-1-dos. Dette er også gjort. Trikset er å legge til
;;;; element i file-coding-system-alist, som vist under.

;;;; NB! Siden asp er Windows, så må vi lagre som DOS! Det har skjedd
;;;; at jeg har sittet i linux, endret på ulovlige charaterer fra
;;;; kunde, og så har Emacs på grunn av character-endringen selv
;;;; funnet ut at den skal lagre i en annen encoding.

;;;; NB! igjen: dersom noe skulle gå galt med res_alle.txt, og på en
;;;; eller annen måte blir kodet i utf-8, så kan dette enkelt rettes
;;;; opp med C-x RET c iso-latin-1-dos save-buffer (og touche buffer
;;;; om nødvendig).

;;;; For å vaske forkludrete data som følge av encoding-rot kan man
;;;; evaluere noe sånt som
;;;; (map-strings-region 1 (point-max) *iso-latin1-encoding* 4 0) 

;;;; Noen tips.

;;;; 1. Ikke slett resultatbufferen. Det er ikke noe vits i å laste
;;;;    ned fra FTP hele tiden. Det holder med å synkronisere én vei.

;;;; 2. Legg mail i Brinkster som har med tabelloppdatering i egen
;;;;    katalog. Det er ryddigere, særlig med tanke på all spammen som
;;;;    kommer. Jeg pleier dessuten å flagge de mailene i
;;;;    tabellkatalogen som ikke ennå er blitt lastet opp.

;;;; 3. De fleste resultatene sendes som Excel-filer. En kjapp måte å
;;;;    trekke ut dataene, er som følger
;;;;    * Åpne vedlegg
;;;;    * Ctrl-Home for å komme til A1
;;;;    * Down for å komme til første resultatrad (dette er ikke nødvendig dersom overskriftsrad mangler)
;;;;    * Ctrl-Shift-End for å merke av alle fylte celler
;;;;    * Ctrl-c for å kopiere
;;;;    * Gå til Emacs, åpne buffer *scratch* og yank inn teksten
;;;;    * Kjør qp-customer-results-to-table-buffer og save buffer
;;;;    * Kjør qp-upload-table-file
;;;;    * Lukk vedlegg

;;;; Andre shortcuts
;;;; I Forslagsliste
;;;; C-S-f Filtrer fempoengere
;;;; C-S-q Filtrer til bare valgte
;;;; I Lag quiz
;;;; C-S-f Generer forslagsliste
;;;; C-S-n Neste tag
;;;; C-S-t Overfør til quizark
;;;; C-S-s lagre

;;;; Annet
;;;; Generere nye kataloger
;;;; for i in `seq 33 52`; do mkdir "$iuke" ; done

;;;; Endre antall runder i tabellen
;;;; ==============================
;;;; Siden antall runder i vår- og høstsesongen varierer, må man for
;;;; hver sesong endre antall kolonner for runder i tabellene. Dette
;;;; gjøre som følger

;;;; 1. Last ned TranformUtils.inc:
;;;;    (qp-download :transform-utils)
;;;; 2. Endre cNumRounds og cNumNonCountingRounds etter behov.
;;;; 3. Lagre, og last opp:
;;;;    (qp-upload :transform-utils)
;;;; I elisp:
;;;; 4. Endre qp-season til ny sesong

(defconst +qp-backup-extension+ ".0")

(defun qp-backup-filename (filename)
  "Returns the full path to the backup file FILENAME"
  (concat filename +qp-backup-extension+))

(defun qp-download-brinkster-file (item)
  "Downloads file at URL to LOCAL-PATH, and opens the file at LOCAL-PATH."
  (let ((local-path (qp-local-path item))
	(url (qp-url item)))
    (message "Downloading file at %s..." url)
    (call-process "ncftpget" nil
		  "*ncftp*" nil 
		  "-C" 
		  "-u" "brinkster\\quizpark"
		  "-p" "7#ToskePaveKrem"
		  url
		  (cygpath local-path :unix))
    (message "Finished!")
    (copy-file local-path (qp-backup-filename local-path) t) ;overwrite, no mercy
    (find-file local-path)
    (goto-char (point-max))
    (auto-fill-mode 0)))
;;(qp-download-brinkster-file (qp-res-url) "~/tmp/qwe.txt")

(defun file-size (filename)
  (nth 7 (file-attributes filename)))
;;(file-size "/cygdrive/c/Users/mat_ber/Google Drive/site-lisp/mb-lisp/quiz/qp.el")

(defun qp-compare-new-and-orig (filename)
  "Returns nil iff the new file's size differs considerably from the original."
  (let ((orig-size (file-size (qp-backup-filename filename)))
	(new-size (file-size filename)))
    (if (and (numberp new-size) (plusp new-size))
      (< (abs (1- (/ (float new-size) orig-size))) 0.001)
      (warn "Couldn't upload %s since it is empty or not existing. Did you save it?"
	    filename))))
;;(qp-compare-new-and-orig (qp-res-filename))

(cl-defun qp-upload-brinkster-file (file &optional (ftp-dir "webroot/data") url)
  "Save, i.e. upload FILE to Brinkster via FTP"
  (if url
    (message "Uploading file to %s..." url)
    (message "Uploading file to brinkster directory %s..." ftp-dir))
  (call-process "ncftpput" nil
		"*ncftp*" nil
		"-DD"
		"-u" "brinkster\\quizpark"
		"-p" "7#ToskePaveKrem"
		"-R" "ftps8.brinkster.com"
		ftp-dir
		file)
  (message "Finished!"))
;;(qp-upload-brinkster-file "~/tmp/res_alle.txt_20190401")
;;See ftp://ftps8.brinkster.com/webroot/data/

(defun qp-upload-brinkster-item (item)
  "Save, i.e. upload file specified by ITEM to Brinkster via FTP."
  (let ((local-path (qp-local-path item)))
     (when (or (not (buffer-modified-p))
	    (yes-or-no-p (format "Buffer %s modified; upload anyway? " (buffer-name))))
    (when (or (qp-compare-new-and-orig local-path)
	      (yes-or-no-p "The modified file seems to have been altered considerably. Continue? "))
      (qp-upload-brinkster-file local-path (qp-ftp-dir item) (qp-url item))
      (qp-check-last-upload)))))

;;;; Specializations (currently res_alle.txt and ??)

(defconst +qp-ftp-root+ "ftp://ftps8.brinkster.com")
(defconst +qp-files+
  '((:results "res_alle.txt" "webroot/data")
    (:table "tabell.asp" "webroot") ;head
    (:tables "tabeller.asp" "webroot") ;head
    (:utils "Utils.inc" "webroot") ; general VB utils
    (:transform-utils "TransformUtils.inc" "webroot") ;Some config and DB handling utils
    (:asp-utils "AspUtils.inc" "webroot") ;more utils
    (:generate-table "GenerateHtmlTable.inc" "webroot") ;html generation
    (:html "Html.inc" "webroot") ;html generation
    (:banner "banner.inc" "webroot")
    (:css-old "qpstyleTAB.css" "webroot")
    (:css "qpstyleTAB.css" "webroot/styles")))
(defconst +qp-asp-items+ '(:0101 :0102 :0103 :0104 :0105))
;;(qp-download :generate-table)
;;(qp-upload :generate-table)
;;(qp-download :transform-utils)

(defun qp-asp-item-entry (keyword)
  (when (cl-find keyword +qp-asp-items+)
    (list keyword (format "%s.asp" (downcase (keyword-name keyword))) "webroot")))

(defun qp-item-entry (item)
  (if (keywordp item)
    (or (cl-find item +qp-files+ :key #'first)
	(qp-asp-item-entry item))
    item))
;;(mapcar #'qp-item-entry '(:transform-utils :0101 :0102 :0103 :0104 :0105 :bogus))

(defalias 'qp-item-tag #'first)
(defalias 'qp-item-filename #'second)
(defalias 'qp-item-ftp-dir #'third)
(defun  qp-item-local-dir () "~/tmp")

(defun qp-tag (item) (qp-item-tag (qp-item-entry item)))
(defun qp-filename (item) (qp-item-filename (qp-item-entry item)))
(defun qp-ftp-dir (item) (qp-item-ftp-dir (qp-item-entry item)))
(defun qp-local-dir (item) "~/tmp/")
(defun qp-local-path (item) (expand-file-name (qp-filename item) "~/tmp/"))
(defun qp-url (item) (format "%s/%s/%s" +qp-ftp-root+ (qp-ftp-dir item) (qp-filename item)))
(defun qp-download (item) (qp-download-brinkster-file item))
(defun qp-upload (item) (qp-upload-brinkster-item item))
;;(mapcar #'qp-url (mapcar #'first +qp-files+))
;;(qp-download :0101)
;;(qp-upload :0101)
;;(qp-download :transform-utils)
;;(qp-upload :transform-utils)

(setf file-coding-system-alist
      (append `((,(regexp-quote (qp-local-path :results)) iso-8859-1 . iso-8859-1))
	      file-coding-system-alist))

(defun qp-download-results ()
  "Opens, i.e. downloads buffer from Brinkster via FTP"
  (interactive)
  (qp-download :results))

(defun qp-upload-results ()
  "Saves, i.e. uploads buffer to Brinkster via FTP"
  (interactive)
  (qp-upload :results))


;;;; Table handling

;;; General config
(defconst qp-season '(32 "Allmennquiz"))
(defun qp-season-number () (number-to-string (first qp-season)))
(defun qp-season-name () (second qp-season))


;;; Customer config
(defconst +qp-customers+
  `((burums "Fru Burums (Oslo)" mandag (1 2) "Burums")
    (burums "Fru Burums (Oslo)" mandag (4 5) "Burums")
    (burums "Fru Burums (Oslo)" mandag (3 4) "Burums")
    (burums "Fru Burums (Oslo)" mandag ,(if (eql (emacs-os) :linux) '(0 4)  '(1 5)) "Burums")
    (burums "Fru Burums (Oslo)" mandag (0 1) "Burums") ;Iversen
    (burums "Fru Burums (Oslo)" mandag (1 5) "Burums") ;;highbury style
    (burums "Fru Burums (Oslo)" mandag "\\(.*\\) \\([[:digit:]]+\\)$" "Burums") 
    (vertshuset "Vålerenga Verthus (Oslo)" tirsdag (5 6 0) "Vålerenga")
    (highbury "Highbury (Oslo)" torsdag (0 1) "Highbury")
    (highbury "Highbury (Oslo)" torsdag (1 2) "Highbury")
    (highbury "Highbury (Oslo)" torsdag (0 :last) "Highbury")
    (highbury "Highbury (Oslo)" torsdag
	      ,(if (eql (emacs-os) :linux) '(0 4)  '(1 5)) "Highbury")
    (highbury "Highbury (Oslo)" torsdag "\\(.*\\) \\([[:digit:]]+\\)$" "Highbury") 
    (onkel-oskar-namsos "Onkel Oskar (Namsos)" onsdag (4 5 0) "Onkel Oskar")
    (nellie "Nellie (Hamar)" onsdag (0 1) "Hamar")
    (scotsman "Scotsman (Oslo)" torsdag (4 5 0) "Scotsman")
    (felix "Felix (Lillehammer)" torsdag (0 1) "Felix"))
  "Whats the third number number?
Format is (tag pubstring day columns pubregexp)")

(defun qp-customers (&optional max-column)
  "Returns a list of qp-customers, and only one entry for each pub.
If MAX-COLUMN is an integer, then first delete all entries
calling for a greater number of columns than max-column."
  (cl-remove-duplicates
   (if (integerp max-column)
     (cl-remove-if #'(lambda (x)
		       (and (listp x)
			    (second x) (neql (second x) :last)
			    (> (reduce #'max x) max-column)))
		   +qp-customers+ :key #'qp-customer-columns)
     +qp-customers+)
   :key #'first :from-end t))
;;(qp-customers 3)

(defun qp-customer (tag &optional max-column)
  (find tag (qp-customers max-column) :key #'first))
;;(qp-customer 'burums 2)

(defalias 'qp-customer-pub 'first)
(defalias 'qp-customer-pub-name 'second)
(defun qp-customer-day (customer) (capitalize (symbol-name (third customer))))
(defalias 'qp-customer-columns 'fourth)
(defsubst qp-customer-columns-list (x) (awhen (qp-customer-columns x) (when (consp it) it)))
(defsubst qp-customer-team-column (x) (first (qp-customer-columns-list x)))
(defsubst qp-customer-score-column (x) (second (qp-customer-columns-list x)))
(defsubst qp-customer-round-column (x) (third (qp-customer-columns-list x))) ;optional
(defalias 'qp-customer-pub-regexp 'fifth)
;;(qp-customer-columns (first (qp-customers)))
;;(qp-customer-score-column (first (qp-customers)))
;;(qp-customer-round-column (first (qp-customers)))

;;; Customer entry
(defun qp-customer-entry-from-string (string)
  "Converts STRING to a customer entry"
  (mapcar #'string-trim (split-string string "[\t]")))
;;(qp-customer-entry-from-string "2	Onsdag	Allmennquiz	Onkel Oskar (Namsos)	Purkene	20")

(defun qp-string-filter (string)
  "Replace non-ASCII characters with ASCII counterparts"
  (let ((res (string-replace-map (string-trim string)
	       '(("’" . "'")
		 ("´" . "'")))))
    (aif (string-match "[^[:space:].'a-zA-Z0-9æøåÆØÅä]()" res)
      (error "Unhandled non-ASCII character `%c' at position %d"
	     (char res it) it)
      res)))
;;(qp-string-filter "Kate’s Bush 26")

(defun qp-customer-entries-from-string (string)
  "Converts STRING, i.e. the results entered by a customer, to a
  list of customer-result-entry's."
  (mapcar #'qp-customer-entry-from-string
	  (remove-if #'empty-string-p
	    (string-to-lines (qp-string-filter string)))))

(defun qp-customer-entries-from-region (beg end)
  "Converts region to a list of customer-result-entry's and kills the region.
The killing can be avoided by entering a prefix argument (not
implemented TODO)."
  (prog1 (qp-customer-entries-from-string
	  (buffer-substring-no-properties beg end))
    (unless current-prefix-arg (kill-region beg end))))

(defun qp-customer-data-regexp-based-p (customer)
  "Returns true iff name and scores are extracted using a regexp.
Otherwise it is column based."
  (stringp (qp-customer-columns customer)))

(defun qp-customer-entry-team-name-and-score (customer entry)
  "Returns a pair (TEAM-NAME TEAM-SCORE) for team ENTRY at CUSTOMER"
  (if (qp-customer-data-regexp-based-p customer)
    (string-match* (qp-customer-columns customer) (first entry) :num '(1 2))
    (list (nth (qp-customer-team-column customer) entry)
	  (let ((sc (qp-customer-score-column customer)))
	    (if (eql sc :last)
	      (last-elt entry)
	      (nth sc entry))))))

(defun qp-customer-round (customer entry)
  (let ((column (qp-customer-round-column customer)))
    (when (integerp column) (nth column entry))))
;;(qp-customer-round (car +qp-customers+) '(1))

;;; Table entry
(defun qp-table-entry-to-string (entry)
  "Converts table ENTRY to a string"
  (concat* entry :in "\t"))

(defun qp-table-entires-to-string (entires)
  "Converts table ENTRIES to a string"
  (concat* (mapcar #'qp-table-entry-to-string entries) :in "\n"))

(defun qp-insert-table-entries (entries buffer &optional at-point-p)
  "Converts table ENTRIES a string and inserts it at POINT"
  (with-buffer buffer
    (unless at-point-p
      (goto-char (point-max)))
    (insert (qp-table-entires-to-string entries))))


;;; Conversion between customer entry and table entry
(defun qp-customer-entry-to-table-entry (customer customer-entry round)
  (awhen (qp-customer-entry-team-name-and-score customer customer-entry)
    (destructuring-bind (team-name team-score) it
      (and (not (empty-string-p team-name))	
	 (not (empty-string-p team-score))
	 (not (zerop (string-to-number team-score)))
	 (list (qp-season-number)
	       round
	       (qp-customer-day customer)
	       (qp-season-name)
	       (qp-customer-pub-name customer)
	       (capitalize team-name) team-score)))))
;;(qp-customer-entry-to-table-entry (find 'onkel-oskar-namsos +qp-customers+ :key #'first) (qp-customer-entry-from-string "1	Onsdag	Allmennquiz	Onkel Oskar (Namsos)	Cranium	45") 1)
;;(capitalize "aa bb")

(defun qp-customer-entries-to-table-entries (customer-tag customer-entries round)
  (remove-if #'null
    (mapcar #'(lambda (x) (qp-customer-entry-to-table-entry customer x round))
      customer-entries)))

(defun count-matches-in-string (regexp string &optional allow-overlap-p)
  "Count the number of substrings in STRING that matches regexp.
Iff ALLOW-OVERLAP-P is true then overlapping matches are counted"
  (loop for start = 0 then (if allow-overlap-p (1+ begin) end)
	for (begin end) = (and (string-match regexp string start)
			       (list (match-beginning 0) (match-end 0)))
	while end count 1))
;;(count-matches-in-string "aa" "aaabaaa" nil)

(cl-defun qp-guess-customer-tag (data &optional (limit 5))
  "TODO: collect team-pub relations from res_alle.txt and create
regexp for matching team in a line of DATA. If match, then this
team will be linked to the pub(s) using the generated relation
list. Do this for all data lines, and select the most frequently
detected pub name."
  (let* ((lines (remove-if #'empty-string-p (string-lines data)))
	 (numlines (length lines))
	 (numcols (count 9 (first lines))))
    (loop for (tag x y z regexp) in (qp-customers numcols)
	  if (> (count-matches-in-string regexp data)
		(min limit (/ numlines 2)))
	  return tag)))
;;(qp-guess-customer-tag "Burums Burums Burums Burums Burums Burums")

(cl-defun qp-read-customer-tag ()
  (let ((completion-ignore-case t))
    (intern (completing-read "Customer: " +qp-customers+ nil t nil))))
;;(qp-read-customer-tag)

(cl-defun qp-get-customer-tag (data &optional (limit 5))
  "Returns current customer tag based on DATA.
For LIMIT, see qp-guess-customer-tag."
  (or (qp-guess-customer-tag data) (qp-read-customer-tag)))
;;(qp-get-customer-tag "burums burums burums burums burums")

(defun qp-guess-round (customer)
  "Customer is ..."
  (with-buffer (qp-filename :results)
    (goto-char (point-max))
    (when (re-search-backward
	   (format "%s\t\\([[:digit:]]+\\)\t[^\t]*\t[^\t]*\t%s\t"
	     (qp-season-number) (qp-customer-pub-name customer))
	   nil t)
      (let ((res (string-to-number (match-string 1))))
	(when (plusp res) (number-to-string (1+ res)))))))
;;(qp-guess-round (first (qp-customers)))

(defun qp-set-round (customer-tag customer-entry)
  "Set current round"
  (let ((customer (qp-customer customer-tag customer-entry)))
    (or (qp-customer-round customer customer-entry)
      (qp-guess-round customer)
      (number-to-string (read-minibuffer "Round: ")))))

(defun qp-legal-table-entries-p (tes)
  (and tes (consp tes)
       (every #'consp tes) (every #'(lambda (x) (= (length x) 7)) tes)
       (loop for x in (flatten (project-sequence tes '(0 1 6)))
	     always (integerp (read x)))))
;;(qp-legal-table-entries-p '(("0" "1" "2a" "3a" "4a" "5a" "6") ("0" "1" "2a" "3a" "4a" "5a" "6")))

(defun qp-customer-results-to-table-buffer (beg end)
  "TODO: Fix encoding. The region somehow must be converted from utf8 (?) to iso-8859-1."
  (interactive "r")
  ;; (error "Called with customer '%S' in region from %d to %d" customer beg end)
  (let* ((customer-tag (qp-get-customer-tag
			(buffer-substring-no-properties beg end)))
	 (customer-entries (qp-customer-entries-from-region beg end))
	 (round (qp-set-round customer-tag (first customer-entries)))
	 (table-buffer (qp-filename :results))
	 (table-entries
	  (loop with max-column = (length (first customer-entries))
		with customers = (copy-if (bind #'eql customer-tag)
					  (qp-customers max-column)
					  :key #'first)
		for customer in customers
		for tes = (qp-customer-entries-to-table-entries
			   customer customer-entries round)
		if (qp-legal-table-entries-p tes) return tes)))
    (qp-insert-table-entries table-entries table-buffer)
    (switch-to-buffer table-buffer)))
;;(copy-if (bind #'eql 'burums) +qp-customers+ :key #'first)

(defun qp-copy-mail-body-to-clipboard ()
  (interactive)
  (kill-new "Hei! 

Vedlagt ukens quiz. 

Vennlig hilsen 
Quiz Park
"))

(defun qp-copy-news-and-sudden-death-questions-to-clipboard ()
  "Formats news questions and sudden-death question to a semi-colon separated list
and copies it to clipboard"
  (interactive)
  (let* ((skip-regexp (format "^%s$" (regexp-opt '("man" "tir" "ons" "tor" "fre" ""))))
	 (lines (remove-if #'(lambda (x) (string-match* skip-regexp x))
		  (split-string (thing-at-point 'paragraph) "\n")))
	 (news (loop for x in (butlast lines 2) collect (let ((entries (split-string x "\t")))
							  (if (= (length entries) 2)
							      (push-back nil entries))
							  entries)))
	 (sudden-death (last lines 2)))
    (kill-new (concat* (loop for l in (cons sudden-death news)
			  collect (concat* l :in ";")) :in "\n"))))

(defconst qp-res-map (make-sparse-keymap))
(define-key global-map "\C-cq" qp-res-map)
(define-key qp-res-map "u" #'qp-upload-results)
(define-key qp-res-map "d" #'qp-download-results)
(define-key qp-res-map "t" #'qp-customer-results-to-table-buffer)
(define-key qp-res-map "c" #'qp-copy-mail-body-to-clipboard)
(define-key qp-res-map "n" #'qp-copy-news-and-sudden-death-questions-to-clipboard)

;;; utils for finding the current round number from date
(cl-defun qp-current-round (&optional (date-or-week (week-number))
				      (first-round-week 34))
  "Returns the round number for DATE-OR-WEEK.
This argument parameter is either an integer representing the
current week number. Or it is a string, which must be parseable
by the mb-time library. DATE-OR-WEEK is optional: if it is not
provided, the function returns the round of the current week.

A second optional argument, FIRST-ROUND-WEEK, lets you change the
week number of the first round."
  (if (integerp date-or-week)
    ;; week number
    (- date-or-week first-round-week -1)
    ;; date string
    (qp-current-round (week-number date-or-week))))
;;(mapcar #'qp-current-round (list 38 "2016-09-22" "2016-09-15"))
;;(qp-current-round)

(defun qp-check-last-upload ()
  (when (string= (buffer-name) "res_alle.txt")
    (destructuring-bind (s round day sname pub team points)
	(split-string (last-elt (buffer-lines) 1) "[\t]")
      (let ((url (format
		     ;; TODO util for GET queries
		     "http://quiz-park.com/tabell.asp?SerieType=%s&SerieRunde=%s&GlobalEllerPub=%s"
		   sname round pub)))
	;; (browse-url-emacs url)
	(browse-url url)))))
;;(qp-check-last-upload)

(provide 'quiz-park)
