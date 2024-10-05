(require 'json)

(cl-defun wi-format-proplist (&rest proplist)
  (url-encode-url
   (concat* (cl-loop for (p v) in (cut proplist)
		  collect (format "&%s=%s" p v)))))
;;(wi-format-proplist "titles" "Johan Halvorsen" "prop" "langlinks")

(cl-defun wi-api-query-url (&rest proplist)
  (format "https://en.wikipedia.org/w/api.php?action=query%s"
    (apply #'wi-format-proplist proplist)))
;;(wi-api-query-url "titles" "Johan Halvorsen" "prop" "langlinks" "format" "json")

(cl-defun wi-api-query-raw (&rest proplist)
  (wget-to-string (apply #'wi-api-query-url proplist)))
;;(wi-api-query-raw "titles" "Johan Halvorsen" "prop" "langlinks" "format" "json")

;;(wget-to-string "https://en.wikipedia.org/w/api.php?action=query&titles=Johan%20Halvorsen&prop=langlinks&lllimit=500&format=json")
;;(wget-to-string "https://en.wikipedia.org/w/api.php?action=query&pageids=27385152|30144012&prop=langlinks&lllimit=500&format=json")

(cl-defun wi-api-query (&rest proplist)
  (json-read-from-string
   (apply #'wi-api-query-raw (append '("format" "json") proplist))))
;;(wi-api-query "titles" "Johan Halvorsen" "prop" "langlinks" "format" "json")

(cl-defun wi-langlinks-info (target &optional (target-type "pageids"))
  "Return (NUM-LANGUAGES TITLE PAGEID) for wpage with ARTICLE-TITLE
See https://www.mediawiki.org/wiki/API:Langlinks for API doc."
  (destructuring-bind ((langlinkstag . langlinks)
		       (titletag . title)
		       (nstag . ns)
		       (pageidtag . pageid))
      (rest (second (second (car (wi-api-query target-type target
					       "prop" "langlinks"
					       "lllimit" "500")))))
    (list (length langlinks) title pageid)))

(cl-defun wi-langlinks-raw (target &optional (target-type "pageids"))
  "Return (NUM-LANGUAGES TITLE PAGEID) for wpage with ARTICLE-TITLE.
TODO: handle the limit of 50 pageids.
See https://www.mediawiki.org/wiki/API:Langlinks for API doc."
  (wi-api-query target-type target "prop" "langlinks" "lllimit" "500"))
;;(setf asd (wi-langlinks-raw "27385152|30144012"))
;;(setf asd (copy-tree qwe))

(cl-defun wi-num-langlinks (ejson &optional (target-type "pageids"))
  "Return (NUM-LANGUAGES TITLE PAGEID) for wpage with ARTICLE-TITLE
See https://www.mediawiki.org/wiki/API:Langlinks for API doc."
  ;; (cl-loop for x in (cadar (wi-api-query target-type target "prop" "langlinks" "lllimit" "500")) collect x))
  (cl-loop for (id (lt . ll) (tt . title)) in (cdadar ejson)
	collect (list (length ll) title (substring (sstring id) 1))))
;;(setf ewq (wi-num-langlinks (wi-langlinks-raw "27385152|30144012")))
;;(string-to-number (substring (sstring (third (first ewq))) 1))
;;(wi-num-langlinks "27385152|30144012")
;;(wi-langlinks-info "Johan Svendsen" "titles")

(cl-defun wi-category-members (category-name &key (limit 100))
  (wi-api-query "list" "categorymembers"
		"cmtitle" (format "Category:%s" category-name)
		"cmlimit" (sstring limit)
		"cmtype" "page"))
;;(setf res (wi-category-members "Norwegian classical composers"))

(cl-defun wi-category-member-ids (category-name &key (limit 100))
  (cl-loop for x across (cdadar res)
	collect (cdr (third x))))
;;(wi-category-member-ids "Norwegian classical composers")
;;(length (cdadar res))

(cl-defun wi-rank-category (category-name &key (limit 100))
  (cl-sort (wi-num-langlinks
	    (wi-langlinks-raw
	     (concat* (wi-category-member-ids category-name :limit limit)
	       :in "|" :key #'sstring)))
    #'> :key #'first))
;;(setf items (wi-rank-category "Norwegian classical composers"))

;;(length (cdr (second (second (cadar asd)))))

;;; Retrieve category members
;;api.php?action=query&list=categorymembers&cmtitle=Category:Physics&cmlimit=20 [try in ApiSandbox]

;;https://en.wikipedia.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Norwegian%20classical%20composers&cmlimit=100&cmtype=page
