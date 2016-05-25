;;;; Labrador API (lab-api)
;;;; Prototype for reading lab-api query strings and converting them to Solr filters

;;;; Definitions:

;;;; query string:     The URL substring following the '?' character
;;;;                   Ex: http://qwe.com?id=1&debug=1

;;;; field-value-pair: A query string substring separated by '&' (or the first '?') characters
;;;;                   Ex: id:1&debug=1 (two field-value-pairs in this example)
;;;;                       ^^^^ +++++++

;;;; query field:      Left hand side of a field-value-pair
;;;; query value:      Right hand side of a field-value-pair

;;;; Solr filter:      a query value with Solr query syntax.
;;;; Solr filter atom: A Solr filter expression on the form solr-field:solr-value
;;;; Solr field:       Left hand side of a Solr filter atom

;;;; Solr value:       Right hand side of a Solr filter atom. Can be a
;;;;                   number or string (atoms) or a composite of other values using
;;;;                   boolean expression (v1 AND v2), (NOT v3), (NOT v4 OR v5) etc.
;;;; Solr period       A Solr value represting a time period.

;;;; lab-api           The labrador API. Every Solr definition above has a corresponding
;;;;                   API definition: api filter, api filter atom, api field, api value.
;;;;                   The syntax is also similar.
;;;; API               Abbrevation of lab-api where no ambigiuties should be present
;;;; API date field    A lab-api field that takes lab-api period (strictly) as value
;;;;                   Note that in the current versions composite values are not allowed.

;;;; Quasi namespace for this module: LAB
;;;; (Note that if a symbol concerns lab-api, its symbol name is not prefixed with api,
;;;; e.g. not lab-lab-api-date, instead only lab-api-date 


(defconst lab-api-query-test
  "section:nyheter id:qwe tag:ewq published:(2013-04-22T12:08:34 2013-04-22T12:08:34)/DATE"
  "A test query string consiting of API filters" )

(defconst lab-api-date-fields
  '(published solr_created))

(require 'lab-api-period-to-solr)
(defun lab-field-date-p (field)
  (find (intern field) lab-api-date-fields))
;;(mapcar #'lab-api-field-date-p (mapcar #'sstring '(published solr_created qwe)))

(require 'lab-api-period-to-solr)
(require 'solr)
(defun lab-api-date-filter-regexp ()
  (format "%s\\(%s\\)" solr-field-regexp (lab-api-period-regexp)))
;;(string-match* (lab-api-date-filter-regexp) "id:[a b)/day" :num 1)

(defun lab-api-field-to-solr-1 (name &optional type)
  "Helper function for `lab-api-field-to-solr'. Uses a syntax
  that is more convenient for calculation."
  (if (or (null type) (empty-string-p type))
    (string-case name
      (("title" "subtitle" "body") 
       (lab-api-field-to-solr-1 name "field"))
      (("section" "css" "person" "geotag") 
       (lab-api-field-to-solr-1 name "primarytag"))
      (("state" "status" "tag" "type" "id")
       name)
      (t 
       (lab-api-field-to-solr-1 name "field")))
    
    (string-case type
      (("field")
       (lab-api-field-to-solr-1 name "field_text"))
      (("attribute")
       name)
      (t (format "%s__%s" type name)))))

(defun lab-api-field-to-solr (field)
  "Converts an API field to the corresponding Solr field. Examples:
\"title\" -> \"field_text__title\"
\"attribute.id\" -> \"id\"
\"luther.blisset\" -> \"luther__blisset\""
  (apply #'lab-api-field-to-solr-1 (split-string field "\\.")))
;;(lab-api-field-to-solr "publishhidden")

(defconst lab-api-test-fields
  (mapcar #'sstring
	  '(somefieldtype.somefieldname
	    publishhidden
	    section css person geotag
	    tag status id
	    title subtitle body
	    field.something))
  "A list of legal API fields useful for testing purposes")
;;(mapcar #'lab-api-field-to-solr lab-api-test-fields)

(defun lab-api-query-to-solr (query-string)
  "Converts a lab-api query to an equivalent Solr expression."
  (let* ((regexp (lab-api-date-filter-regexp))
	 (rep-field #'(lambda (s) (lab-api-field-to-solr (match-string 1 s))))
	 (rep-macro-identifier #'(lambda (s) (lab-api-expand-period-identifier (match-string 1 s))))
	 (rep-period #'(lambda (s)
			 "Local function to convert period subexpression. Note the very important handling of the match-data"
			 (let ((md (match-data)))
			   (prog1
			       (lab-api-period-to-solr (match-string 2 s))
			     (set-match-data md)))))
	 ;; first: replace fields
	 (tmp1 (replace-regexp-in-string solr-field-regexp rep-field query-string nil nil 1))
	 ;; then: replace non-intervallic period macros
	 (tmp2 (replace-regexp-in-string api-period-macro-identifier-regexp rep-field tmp1 nil nil 1)))
    ;; then: replace dates
    (replace-regexp-in-string regexp rep-period tmp2 nil nil 2)))
;;(lab-api-query-to-solr lab-api-query-test)
;;(lab-api-query-to-solr "(id:123)")

(provide 'lab-api-to-solr)
