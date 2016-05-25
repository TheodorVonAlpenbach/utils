(require 'mb-utils-strings)

(defconst xml-test-node "<tr itemprop=\"albums\" itemscope itemtype=\"http://schema.org/MusicAlbum\"><a name=\"6159\"></a>Some inner text<a></a></tr>")

(defun xml-attribute-regexp (name &optional value)
  (let ((wildcard-name "[^=[:space:]]+")
	(wildcard-value "[^\"]*"))
    (when (string= name "*") (setf name wildcard-name))
    (when (string= value "*") (setf value wildcard-value))
    (if value
      (format "%s=\"\\(%s\\)\"" name value)
      name)))
;;(xml-attribute-regexp "qwe" "123222")
;;(xml-attribute-regexp "qwe")
;;(xml-attribute-regexp "*" "*")

(defun xml-attribute (name xml)
  (string-match* (xml-attribute-regexp name "*") xml :num 1))
;;(xml-attribute "itemprop" xml-test-node)

(defun xml-attributes-regexp (attributes &optional exact-p)
  "Converts list designator ATTRIBUTES to a regular expression
for matching the attribute part in an XML node. ATTRIBUTES is a
list where each element is either a pair of strings, (NAME
VALUE), or just a STRING. Both NAME and VALUE should be regular
expressions for an attribute name and attribute value
respectively. VALUE can also be set to the string \"*\" for
matching all attribute values. Specifying STRING is just a
shortcut for (STRING *).

 If EXACT-P is true, then ATTRIBUTES describes the whole
attribute list. Otherwise, an ending wildcard is appended to the
resulting string."
  (if (or (null attributes) 
	  (and (stringp attributes)
	       (string= attributes "*")))
    "\\([^>]*\\)"
    (concat* (loop with wildcard-attribute-name = "[^=]+"
		   with wildcard-attribute-value = "[^\"]*"
		   for a in (if (listp attributes) attributes (list attributes))
		   collect (apply #'xml-attribute-regexp (if (listp a) a (list a "*"))))
	     :pre "[[:space:]]+" :in "[[:space:]]+" :suf (if exact-p "" "[^>]*")))) 
;;(xml-attributes-regexp '(("qwe" "123222") "itemprop" "*"))
;;(mapcar #'xml-attributes-regexp '("*" nil "qwe"))
	     
(cl-defun xml-node-regexp (node-name &optional attributes)
  "Returns all xml nodes if N is nil. If N is negative, extract from end"
  (let ((attribute-regexp (xml-attributes-regexp attributes)))
    (concat "<" (format "\\(%s\\)" node-name) attribute-regexp ">"
	    "\\([[:space:][:print:]]*?\\)"
	    "</\\1>")))
;;(xml-node-regexp "tr")

(cl-defun xml-extract-nodes (xml-string node-name &optional attributes n node-content-only)
  "Returns all xml nodes if N is nil. If N is negative, extract from end"
  (let ((node-regexp (xml-node-regexp node-name attributes))
	(n (or n most-positive-fixnum)))
    (string-matches-exact node-regexp xml-string :count (abs n) :num (if node-content-only 3 0) :from-end (< n 0))))
;;(xml-extract-nodes xml-test-node "[^>[:space:]]*" "*" 1 t)

(cl-defun xml-inner (xml &optional (node-name "[^>[:space:]]*"))
  (string-match* (xml-node-regexp node-name "*") xml :num 3))
;;(xml-inner xml-test-node)

(cl-defun xml-inner-text (xml &optional (node-name "[^>[:space:]]*"))
  "Same as `xml-inner' but discards child nodes"
  (let ((string (xml-inner xml node-name)))
    (while (string-match (xml-node-regexp "[^>[:space:]]*" "*") string)
      (setf string (replace-match "" nil t string)))
    string))
;;(xml-inner-text xml-test-node)

(defconst *xml-special-chars*
  '(("quot" "\"")
    ("amp" "&")))

(require 'mb-locale)
(cl-defun xml-decode-characters (string &optional (encoding *iso-latin1-encoding*))
  (loop for x in encoding
	for ascii-code = (fourth x)
	for xml-code = (format "&#%d;" ascii-code) ;
	for char = (first x)
	do (setq string (string-replace string xml-code char))
	finally return string)

  (loop for x in *xml-special-chars*
	for xml-code = (format "&%s;" (first x))
	for char = (second x)
	do (setq string (string-replace string xml-code char))
	finally return string))

(defun html-to-temp-buffer (html-converter html-string buffer-name &rest args)
  "HTML-CONVERTER is a method taking one argument, an
HTML-STRING, and returns a string to be output in a temp buffer
named BUFFER-NAME. HTML-STRING is the HTML content of the file
that wget just did dowdload."
  (with-output-to-temp-buffer buffer-name
    (temp-buffer-resize-mode)
    (princ (xml-decode-characters (apply html-converter html-string args)))))

(cl-defun html-to-temp-buffer-sentinel (temp-buffer-name html-string-converter &rest args)
  "Creates a method that takes a string argument. The argument
 should be an HTML string and the resulting method will convert
 the string according to HTML-STRING-CONVERTER taking arguments
 ARGS, and send the result to a temporary buffer named
 TEMP-BUFFER-NAME."
  (lexical-let ((html-string-converter html-string-converter)
		(temp-buffer-name temp-buffer-name)
		(args args))
    #'(lambda (html-string)
	(apply #'html-to-temp-buffer html-string-converter html-string temp-buffer-name args))))


(provide 'mb-utils-xml)
