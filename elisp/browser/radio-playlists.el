(require 'wget)
(require 'mb-utils-xml)

;;;; NRK Alltid Klassisk
(cl-defun ak-temp-buffer-name (tag)
  (format "*NRK Klassisk%s*" (if tag (concat " " tag) "")))
;;(ak-temp-buffer-name)

(cl-defun ak-date (time-designator)
  (cl-destructuring-bind (s mi h d mo y &rest args)
      (parse-time time-designator)
    (format "%02d-%02d-%04d" d mo y)))
;;(ak-date (iso-date))

(cl-defun ak-historic-url (&optional (time-designator (now)))
  (format "http://radio.nrk.no/guide/%s" (ak-date time-designator)))
;;(ak-historic-url)

(cl-defun ak-epg-entries (html-string)
  "Returns a list of summary epg entries for today. News entries are deleted."
  (let* ((ak-xml (substring-intv html-string 
		   (interval-oo "class=\"channel klassisk epg-channel"
				"class=\"channel ")))
	 (nodes (xml-extract-nodes ak-xml "div" '(("class" "epg-entry.*")))))
    (cl-loop for n in nodes
	     for id = (xml-attribute "data-id" n)
	     for time-node = (first (xml-extract-nodes n "time"))
	     for time = (parse-time (xml-attribute "datetime" time-node))
	     if (string-match* "^mkk" id) collect (list id time))))

(cl-defun ak-epg-entry (html-string time)
  "Returns list \(EPG-ID PROGRAM-PERIOD\)"
  (let* ((rest (cl-member-if (bind #'time< time) (nreverse (ak-epg-entries html-string)) :key #'second))
	 (id (first (first rest))))
    (list id (period :from (second (first rest)) 
		     :to (or (second (second rest)) (midnight time))))))

(cl-defun ak-historic-html-entries (epg-id)
  (xml-extract-nodes (wget-to-string (format "http://radio.nrk.no/programplaylist/%s/html" epg-id))
		     "li" nil nil t))

(cl-defun ak-parse-historic-entry (html-entry)
  (cl-destructuring-bind (artist composer work)
      (string-match* "\\([^-]*\\) - \\([^:]*\\): \\(.*\\)" html-entry :num '(1 2 3))
    (mapcar #'string-trim (list composer work artist))))
;(setq html-details (sr-parse-entry html-entry))

(cl-defun ak-historic-entries (epg-id)
  (mapcar #'ak-parse-historic-entry (ak-historic-html-entries epg-id)))

(cl-defun ak-format-entry-long (ak-entry)
  "Formats AK-ENTRY, which is a list: (TIME COMPOSER WORK ARTIST)"
  (concat (format "Time (est): %s\n" (first ak-entry))
	  (format "Composer:   %s\n" (second ak-entry))
	  (format "Work:       %s\n" (third ak-entry))
	  (format "Artist:     %s\n" (fourth ak-entry))))

(cl-defun ak-format-entry-short (ak-entry)
  "Formats AK-ENTRY, which is a list: (TIME COMPOSER WORK ARTIST)"
  (format "%s: %s: %s\n" (iso-time :time (first ak-entry)) (second ak-entry) (third ak-entry)))

(cl-defun ak-historic-converter (html-string time)
  (let* ((epg-entry (ak-epg-entry html-string time))
	 (track-entries (ak-historic-entries (first epg-entry)))
	 (n (length track-entries)))
    (concat* (cl-loop for track-entry in track-entries
		      for i from 1
		      for est-time = (interpolate-time (float i) (second epg-entry) :a 1 :b n)
		      collect (ak-format-entry-long track-entry est-time))
      :pre (format "Tracks in period %s\n" (period-to-string (second epg-entry)))
      :in "\n")))

;; AK today
(cl-defun ak-url (&optional time)
  "radio.nrk.no/direkte/klassisk")

(require 'json)
(cl-defun ak-entries (html-string &optional n)
  (let* ((json (substring-intv html-string
		 (interval-oo "nrk.state.initState(\"nowNextElements\", " " );")))
	 ;; jsons is a vector
	 (jsons (json-read-from-string json)))
    (nreverse
     (cl-loop for entry across (subseq jsons (- (or n (length jsons))))
	      for time = (cdr (assoc 'st entry))
	      for artist = (cdr (assoc 't entry))
	      for (composer work) = (string-match* "^\\([^:]*\\): \\(.*\\)" (cdr (assoc 'd entry)) :num '(1 2))
	      collect (list time composer work artist)))))

(cl-defun ak-latest-converter (html-string n)
  (concat* (ak-entries html-string n) :suf "\n" :key #'ak-format-entry-short))

(cl-defun ak-now-converter (html-string)
  (ak-format-entry-long (first (ak-entries html-string 1))))

(cl-defun ak-at-converter (html-string time)
  (ak-format-entry-long (find time (ak-entries html-string)
			       :key #'first :test #'string>=)))


;; AK general
(cl-defun ak-to-temp-buffer (ak-converter url tag &rest args)
  (wget url 'utf-8 (apply #'html-to-temp-buffer-sentinel
			       (ak-temp-buffer-name tag)
			       ak-converter args)))

;; AK UI
(cl-defun ak-historic (&optional (time-designator (now :day -1)))
  (interactive)
  (let ((time (parse-time time-designator)))
    (ak-to-temp-buffer #'ak-historic-converter
		       (ak-historic-url time)
		       (format "AK at %s" (iso-date-and-time :time time)) 
		       (parse-time time))))
;;(ak-historic "2016-01-05 11:03")

(cl-defun ak-latest (&optional (n 10)) (interactive)
  (ak-to-temp-buffer #'ak-latest-converter (ak-url) (format "%d siste" n) n))

(cl-defun ak-now () (interactive)
  (ak-to-temp-buffer #'ak-now-converter (ak-url) "spilles nå"))

(cl-defun ak-at (time)
  (ak-to-temp-buffer #'ak-at-converter (ak-url) "spilles nå" time))

(cl-defun ak-open-browser () (interactive)
  (w32-shell-execute "open" (ak-url)))

;;;; SR Klassisk
(cl-defun sr-temp-buffer-name (tag)
  (format "*SR Klassiskt%s*" (if tag (concat " " tag) "")))
;;(sr-temp-buffer-name)

(cl-defun sr-url (&optional (iso-date (iso-date)))
  (format "http://sverigesradio.se/sida/latlista.aspx?programid=1603"))

;;; file parsing
(cl-defun sr-html-entries (html-string &optional (n most-positive-fixnum))
  (xml-extract-nodes html-string "li" '(("class" "track-list-item")) n))

(defconst +sr-track-detail-names+
  '("Artist" "Album" "Ensemble/Orkester" "Dirigent" "Etikett")
  "Order is significant")

(cl-defun sr-parse-track-detail-old (track-detail)
  "item is one of Artist, Album, Ensemble/Orkester Dirigent Etikett"
  (let ((detail-name (first (xml-extract-nodes track-detail "span" () 1 t))))
    (list (substring* detail-name 0 -1) ; skip last semicolon
	  (string-match* (format "<span>%s</span> \\(.*\\)" detail-name) track-detail :num 1))))
;;(sr-parse-track-detail "            <span>Etikett:</span> Sterling")

(cl-defun sr-parse-track-detail (track-detail)
  "Split track-detail into a pair.
\"Solist: Mr. Holtermann\" -> \(\"Solist\" \"Mr. Holtermann\"\)."
  (awhen track-detail
    (mapcar #'string-trim (split-string it ":"))))
;;(sr-parse-track-detail "Solist: Mr. Holtermann")

(cl-defun sr-parse-time (sr-time)
  "Convert SR time to ISO short time format HH:MM."
  (substitute ?: ?. sr-time))
;;(sr-parse-time "12.00")

(cl-defun sr-parse-entry (html-entry)
  "Return a list (time artist title detail1 detail2 ...)."
  (let ((sr-time (xml-inner (first (xml-extract-nodes html-entry "div"
				  '(("class" "track-list-item__time-wrapper"))))))
	(title (xml-inner (first (xml-extract-nodes html-entry "div"
				  '(("class" "track-list-item__title"))))))
	(track-details
	 (xml-extract-nodes html-entry "div"
	   '(("class" "track-list-item__detail")) most-positive-fixnum t)))
    (cl-destructuring-bind (artist work &rest details)
	(mapcar #'xml-inner track-details)
      (list (list (sr-parse-time sr-time) artist title)
	    (mapcar #'sr-parse-track-detail details)))))

(cl-defun sr-entries (html-string &optional n)
  (mapcar #'sr-parse-entry (sr-html-entries html-string n)))

(cl-defun sr-format-entry (sr-entry &optional (with-details t))
  (let* ((main-line-entry (first sr-entry)))
    (if with-details
      (let* ((time-line-entry (list "Time" (first main-line-entry)))
	     (artist-line-entry (list "Artist" (second main-line-entry)))
	     (rest-lines-entries (first (rest sr-entry)))
	     (line-entries (append (list time-line-entry artist-line-entry) rest-lines-entries)))
	(concat* line-entries 
	  :in "\n" 
	  :key #'(lambda (x) (format "%-18s: %s" (first x) (second x)))))
      ;; else just format main-line-entry
      (format "%s:  %s\n" (first main-line-entry) (third main-line-entry)))))
;;(format "%s: %s\n" "17:07" "Mats")

(cl-defun sr-latest-converter (html-string &optional (n 10))
  (concat* (cl-loop with es = (sr-entries html-string n)
		    for e in es
		    collect (sr-format-entry e nil))))

(cl-defun sr-now-converter (html-string)
  (sr-format-entry (first (sr-entries html-string 1))))

(cl-defun sr-at-converter (html-string time)
  ""
  (sr-format-entry (find time (sr-entries html-string)
			 :key #'(lambda (x) (first (first x)))
			 :test #'string>= :from-end nil)))

(cl-defun sr-to-temp-buffer (sr-converter tag &rest args)
  (wget (sr-url) nil (apply #'html-to-temp-buffer-sentinel
			(sr-temp-buffer-name tag)
			sr-converter args)))

;;; SR UI
(cl-defun sr-latest (&optional (n 10)) (interactive)
  (sr-to-temp-buffer #'sr-latest-converter (format "%d sista" n) n))

(cl-defun sr-now () (interactive)
  (sr-to-temp-buffer #'sr-now-converter "just nu"))

(cl-defun sr-at (time)
  (sr-to-temp-buffer #'sr-at-converter "just nu" time))

(cl-defun sr-open-browser () (interactive)
  (w32-shell-execute "open" (sr-url)))

;;; General UI
(cl-defun radio-playlists-is-latest-buffer-p ()
  (string-match "sist" (buffer-name)))

(cl-defun radio-playlists-current-display ()
  (let* ((title (buffer-name))
	 (channel (if (string-match "SR" title) 'sr 'ak))
	 (type (if (string-match "sist" title) 'latest 'now)))
    (list channel type)))

(cl-defun google-url (search-string)
  (format "https://www.google.no/webhp?sourceid=chrome-instant&ie=UTF-8&ion=1#hl=no&output=search&sclient=psy-ab&q=%s&oq=&gs_l=&pbx=1&fp=aec8b35b704dcc84&ion=1&bav=on.2,or.r_gc.r_pw.r_qf.&biw=1680&bih=959"
    search-string))

(cl-defun wikipedia-en-url (search-string)
  (format "http://en.wikipedia.org/w/index.php?title=Spesial:S%C3%B8k&search=%s"
    search-string))

;; The next statement is a hack to prevent a succeeding invocation of
;; help-mode to auto-load over the following definition
;; How should this be done otherwise?
;; Probably the best thing is to avoid help mode alltogether...
(require 'help-mode) 
(cl-defun help-follow ()
  (interactive)
  "Overrides original help follow. Must take care not doing anything if format strings doesn't match"
  (let ((display (radio-playlists-current-display)))
    (if (eq (second display) 'latest)
      (let ((time (save-excursion
		    (move-beginning-of-line 1)
		    (time-at-point))))
	(other-window 1)
	(if (eq (first display) 'ak)
	  (ak-at (string-trim time))
	  (sr-at (string-trim time))))
      ;;else is now buffer
      (let* ((info (maptree #'string-trim
		     (parse-csv-string (buffer-string-no-properties) ":")))
	     (n (line-number-at-pos))
	     (search-string (concat* (mapcar #'second (rest (head n info))) :in " ")))
	(browse-url (google-url search-string))))))
;;(help-follow)

(let ((radio-playlist-functions '(sr-now sr-latest ak-now ak-latest))
	      (current-playlist 'ak-latest)
	      (current-now 'ak-now))
  (cl-defun radio-read-playlist ()
    (intern-soft
     (completing-read
      "Select radio-channel: "
      (mapcar #'symbol-name radio-playlist-functions)
      nil t)))

  (cl-defun radio-playlist (prefix)
    (interactive "P")
    "Can be called with F9"
    (when prefix
      (setq current-playlist
	    (intern-soft (completing-read "Select radio-channel: "
					  (mapcar #'symbol-name radio-playlist-functions)
					  nil t))))
    (funcall current-playlist))
  (cl-defun radio-now (prefix)
    (interactive "P")
    "Can be called with F9"
    (when prefix
      (setq current-now (radio-read-playlist)))
    (funcall current-now)))

(provide 'radio-playlists)
