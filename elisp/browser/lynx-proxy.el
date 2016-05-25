(require 'mb-utils-div)
(require 'mb-sequences)

(defstruct (lynx-proxy-db (:conc-name lpdb-))
  "ENTRIES is a hash table of LPEs (se below). Max-index should be
autoincremented each time new entry is added to db."
  (entries (make-hash-table :test #'equal))
  (path *lynx-proxy-dir*)
  (max-index 0))

(defstruct (lynx-proxy-entry (:conc-name lpe-))
  "Entrys 'key' is the URL. INDEX is a unique integer within a LPDB.
POINT is the emacs buffer's point value when the entry site was last
visited."
  (url :read-only t)
  (index (lpdb-get-unique-index) :read-only t)
  (point 0))

(cl-defun lpdb-get-unique-index (&optional (db *lynx-proxy-db*))
  "Returns an integer that is not the index value of any entry in DB.
Assumes that the entry with the highest index slot value is first in
entry list. This will always be so when the entries (like now) are
pushed on the list."
  (prog1 
      (lpdb-max-index db)
    (incf (lpdb-max-index db))))

(defun external-url-p (url)
  "Returns URL if it does not refers within local network."
  (if (not (local-url-p url)) url nil))
;(external-url-p "file://localhost/language.html")

(defun local-url-p (url)
  "Returns true if URL refers within local network."
  (awhen (string-match "\\(file://localhost\\)\\|\\([a-zA-z]:\\)" url)
    (zerop it)))
;(local-url-p "file://localhost/language.html")

(defun lpe-local-p (entry)
  "Returns T if entrys html file is stored locally."
  (< (lpe-index entry) 0))
;;(lpe-local-p [cl-struct-lynx-proxy-entry "c:/unix/doc/HyperSpec/Body/typspe_and.html" -1 0])

(defun lpe-file-exists-p (entry)
  "Returns T iff file in entry exists. If no this suggests download."
  (file-exists-p (lpe-abs-filename entry)))
;;(lpe-file-exists-p *lynx-proxy-db*)

(defun urlp (url)
  "Returns nil iff URL does not take a recognizable url value"
  (not (null url)))
;;(urlp nil)

(cl-defun lpdb-entry (url &optional (lpdb *lynx-proxy-db*))
  "Returns the entry in LPDB corresponding to URL. Returns a new one
if none existed."
  (let ((entry (gethash url (lpdb-entries lpdb))))
    (or entry
	(and (urlp url)
	     (lpdb-new-entry url lpdb)))))

(cl-defun lpdb-new-entry (url &optional (lpdb *lynx-proxy-db*))
  "Adds URL to LPDB. Returns for url dump. TODO: initalize point value
according to commercial garbage, so if (subseq-p 'aftenposten url)
then set pos to some other value than 0."
  (setf (gethash url (lpdb-entries lpdb))
	(if (local-url-p url)
	  (make-lynx-proxy-entry :url url :index -1)
	  (make-lynx-proxy-entry :url url))))

(defun url-name (url)
  "Returns the biggest suffix of url that can be used as a win32
filename. Some specialcases. New! Using url-file-encode!"
  (cond
    ((string-match "aftenposten.*\\(articleID=[0-9]+\\)" url)
     (match-string 1 url))
    ((string-match "www\\.elpais\\.es" url) "el-pais")
    (nil (url-file-encode url))
    (t (first (last (split-string url "[?/]"))))))
;;(url-name "http://www.aftenposten.no/ntb/innenriks/article.jhtml?articleID=172555")
;;(url-name "http://www.elpais.es/diario/espana/index.html?d_date=/articulo.html?d_date=20011023&xref=20011023elpepinac_2&type=Tes&anchor=elpepinac")

(cl-defun lpe-filename (entry &optional (lpdb *lynx-proxy-db*))
  "Returns ENTRY's filename"
  (format "%s-%s" (lpe-index entry) (url-name (lpe-url entry))))

(defun lpe-abs-filename (entry)
  "Where to store html."
  (if (local-url-p (lpe-url entry))
    (lpe-url entry)
    (expand-file-name (lpe-filename entry) *lynx-proxy-dir*)))
;;(gethash "http://www.aftenposten.no" (lpdb-entries *lynx-proxy-db*))
;;(lpe-abs-filename [cl-struct-lynx-proxy-entry "http://www.aftenposten.no" 0 427])

(defun hash-table-print (hash-table)
  "Converts HASH-TABLE to a printing format, ie a list with format
\(ht-test ht-weakness ht-size ht-rehash-threshold ht-rehash-size ((k1
v1) (k2 v2)...)\).See #'MAKE-HASH-TABLE and #'HASH-TABLE-READ."
  (lexical-let ((list ())) ; hash-table is implemented as a cons
    (maphash #'(lambda (k v) (push (cons k v) list))
	     hash-table)
    (list (hash-table-test hash-table)
	  (hash-table-weakness hash-table)
	  (hash-table-size hash-table)
	  (hash-table-rehash-threshold hash-table)
	  (hash-table-rehash-size hash-table)
	  (nreverse list))))
;;(hash-table-print (lpdb-entries *lynx-proxy-db*))

(cl-defun hash-table-read (list)
  "Converts list to a HASH-TABLE. For :TEST and :SIZE see
#'MAKE-HASH-TABLE. ADDITIONAL-SIZE is the expected size of the created
hash table less the size of the LIST."
  (let ((hash-table (make-hash-table :test (first list) 
				     :weakness (second list)
				     :size (third list)
				     :rehash-threshold (fourth list)
				     :rehash-size (fifth list))))
    (loop for x in (sixth list)
	  do (setf (gethash (first x) hash-table) (rest x)))
    hash-table))
;;(hash-table-read (hash-table-print (lpdb-entries *lynx-proxy-db*)))

(cl-defun list-to-hash-table (list &key (test (first list))
				 (additional-size 0)
				 (size (+ additional-size (length list))))
  "Converts list to a HASH-TABLE. For :TEST and :SIZE see
#'MAKE-HASH-TABLE. ADDITIONAL-SIZE is the expected size of the created
hash table less the size of the LIST."
  (let ((hash-table (make-hash-table :test test :size size)))
    (dolist (x (rest list) hash-table)
      (setf (gethash (first x) hash-table) (rest x)))))
;(list-to-hash-table (hash-table-to-list qwe) :additional-size 10)

(cl-defun lynx-proxy-save-db (&optional (db *lynx-proxy-db*) (file *lynx-proxy-db-file*))
  (interactive)
  (print* (list (lpdb-path db)
		(lpdb-max-index db)
		(hash-table-print (lpdb-entries db))) 
	  file))
;(lynx-proxy-save-db)

(cl-defun lynx-proxy-read-db (&optional (file *lynx-proxy-db-file*))
  (let ((list (if (file-exists-p *lynx-proxy-db-file*) 
		(read* *lynx-proxy-db-file*)
		())))
    (make-lynx-proxy-db :path (first list)
			:max-index (second list)
			:entries (hash-table-read (third list)))))
;(lynx-proxy-read-db)

(defun delete-file* (file)
  "No warning if FILE does not exist."
  (when (file-exists-p file)
    (delete-file file)))

(defun lynx-proxy-clear-all ()
  (setq *lynx-proxy-db* (make-lynx-proxy-db)))
;;(lynx-proxy-clear-all)

(cl-defun lynx-proxy-clear-db (&key (time (now :week -1)) 
				  (regexp "")
				  (db *lynx-proxy-db*))
  "Deletes all entries in lpdb DB and corresponding files created
before TIME. TODO: delete backup files."
  (let* ((entries (lpdb-entries db)) 
	 (delete-urls
	  ;; loop through entries and delete old files and collect
	  ;; corresponding indices
	  (loop for url being the hash-keys of entries
		using (hash-values entry)
		for file = (lpe-abs-filename entry)
		for time-created = (nth 5 (file-attributes file))
		for match-p = (or (not time-created)
				  (and (string-match regexp (lpe-url entry)) 
				       (time< (decode-time time-created) time)))
		when match-p collect url)))
    ;; now delete entries
    (loop for url in delete-urls
	  do (remhash url entries))
    ;; now delete appropriate files in lynx-proxy directory
    (lynx-proxy-delete-files :time time)
    db))
;;(lynx-proxy-clear-db)
;;(lynx-proxy-clear-db :regexp "www\\.aftenposten\\.no/nyheter/[a-z/]*/$" :time (now :day -1))
;;(lynx-proxy-clear-db :regexp "www\\.aftenposten\\.no$" :time (now :hour -1))
;;(lynx-proxy-clear-db :time (now :hour -1))
;;(string-match "www\\.aftenposten\\.no/nyheter/[a-z/]*$" "http://www.aftenposten.no/nyheter/iriks/")

(cl-defun lynx-proxy-delete-files (&key (time (now :week -1)) 
				      (dir *lynx-proxy-dir*))
  (let ((files (directory-files dir t))
	(tabus (list (concat *lynx-proxy-dir* ".proxy-db") 
		     (concat *lynx-proxy-dir* "lynx-favorites"))))
    (loop for file in files
	  for time-created = (nth 5 (file-attributes file))
	  for decoded-time-created = (decode-time time-created)
 	  when (and 
 		(not (file-directory-p file))
		(time< (decode-time time-created) time)
		(find file tabus :test-not #'string=)
		)
	  do (delete-file file))))
;;(lynx-proxy-delete-files :time (now))
;;(find ".proxy-db" '(".proxy-db" "lynx-favorites") :test #'string=)
;;(time< (now :week -1) '(18 41 19 6 11 2002 3 nil 3600))

;;(decode-time (nth 5 (file-attributes "c:/unix/data/lynx-proxy/102-articleID=198257")))

(defun lynx-proxy-clear-all ()
  (interactive)
  (lynx-proxy-clear-db :time (now))
  (lynx-update-proxy-refs-regexp)
  (font-lock-fontify-block))
;;(lynx-proxy-clear-all)

;;;; scratch files
(cl-defun lynx-proxy-find-file-scratch (&optional (url *lynx-current-url*))
  "Returns scratch file associated with URL"
  (interactive)
  (error "This doesn't work for the new hashtable db version")
  (find-file-other-window
   (concat (second (assoc url *lynx-proxy-db*)) ".scratch"))
  (shrink-window 15))
;(cancel-debug-on-entry 'lynx-proxy-find-file-scratch)

(provide 'lynx-proxy)
