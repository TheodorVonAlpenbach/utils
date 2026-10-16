;;; mb-utils-xml.el --- Raw XML and HTML fragment helpers -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'xml)
(require 'mb-utils-strings)

;;; Code:

(cl-defun xml-attribute-regexp (name &optional value)
  "Return a regexp matching attribute NAME and optional VALUE.
NAME and VALUE are raw regexps.  The string `*' matches any name or value."
  (let ((name (if (string= name "*") "[^=[:space:]>]+" name))
	(value (if (string= value "*") "[^\"']*" value)))
    (if value
	(format "%s[[:space:]]*=[[:space:]]*[\"']\\(?:%s\\)[\"']"
		name value)
      name)))

(defun mb-xml--parse-attributes (opening-tag)
  "Return attributes parsed from OPENING-TAG as an alist."
  (let ((position (string-match "[[:space:]/>]" opening-tag))
	(regexp
	 "\\([[:alnum:]_:.][-[:alnum:]_:.]*\\)\\(?:[[:space:]]*=[[:space:]]*\\(?:\"\\([^\"]*\\)\"\\|'\\([^']*\\)'\\|\\([^[:space:]>]+\\)\\)\\)?")
	attributes)
    (while (and position (string-match regexp opening-tag position))
      (push (cons (match-string 1 opening-tag)
		  (or (match-string 2 opening-tag)
		      (match-string 3 opening-tag)
		      (match-string 4 opening-tag)))
	    attributes)
      (setq position (match-end 0)))
    (nreverse attributes)))

(defun mb-xml--full-match-p (regexp string)
  "Return non-nil when REGEXP matches all of STRING."
  (string-match-p (concat "\\`\\(?:" regexp "\\)\\'") string))

(defun mb-xml--attribute-spec-match-p (specification attribute)
  "Return non-nil when SPECIFICATION matches ATTRIBUTE."
  (let* ((pair (if (listp specification)
		   specification
		 (list specification "*")))
	 (name (cl-first pair))
	 (value (or (cl-second pair) "*")))
    (and (or (string= name "*")
	     (mb-xml--full-match-p name (car attribute)))
	 (or (string= value "*")
	     (and (cdr attribute)
		  (mb-xml--full-match-p value (cdr attribute)))))))

(defun mb-xml--attributes-match-p (specifications opening-tag)
  "Return non-nil when OPENING-TAG satisfies SPECIFICATIONS."
  (or (null specifications)
      (equal specifications "*")
      (let ((attributes (mb-xml--parse-attributes opening-tag)))
	(cl-every
	 (lambda (specification)
	   (cl-some
	    (lambda (attribute)
	      (mb-xml--attribute-spec-match-p specification attribute))
	    attributes))
	 (if (and (listp specifications)
		  (not (stringp (cl-first specifications))))
	     specifications
	   (list specifications))))))

(cl-defun xml-attribute (name xml)
  "Return the value of attribute NAME in XML, or nil."
  (cdr (assoc-string name (mb-xml--parse-attributes xml) t)))

(cl-defun xml-attributes-regexp (attributes &optional exact-p)
  "Return a regexp approximating an XML attribute list.
ATTRIBUTES contains names or (NAME VALUE) pairs expressed as raw regexps.
When EXACT-P is nil, allow additional attributes after those requested."
  (if (or (null attributes) (equal attributes "*"))
      "\\(?:[[:space:]][^>]*\\)?"
    (concat
     "[[:space:]]+"
     (mapconcat
      (lambda (attribute)
	(apply #'xml-attribute-regexp
	       (if (listp attribute) attribute (list attribute "*"))))
      (if (and (listp attributes)
	       (not (stringp (cl-first attributes))))
	  attributes
	(list attributes))
      "[[:space:]]+")
     (unless exact-p "\\(?:[[:space:]][^>]*\\)?"))))

(cl-defun xml-node-regexp (node-name &optional attributes)
  "Return an approximate regexp for NODE-NAME with ATTRIBUTES.
Use `xml-extract-nodes' when nested nodes of the same name are possible."
  (concat "<\\(" node-name "\\)"
	  (xml-attributes-regexp attributes)
	  ">"
	  "\\([[:space:][:print:]]*?\\)"
	  "</\\1>"))

(defun mb-xml--matching-close (xml name start)
  "Return (CLOSE-BEGIN CLOSE-END) for NAME opened before START in XML."
  (let ((regexp (format "</?%s\\(?:[[:space:]][^>]*\\)?>" (regexp-quote name)))
	(depth 1)
	close)
    (while (and (cl-plusp depth) (string-match regexp xml start))
      (setq start (match-end 0))
      (if (eq (aref xml (1+ (match-beginning 0))) ?/)
	  (progn
	    (cl-decf depth)
	    (when (zerop depth)
	      (setq close (list (match-beginning 0) (match-end 0)))))
	(unless (eq (aref xml (- (match-end 0) 2)) ?/)
	  (cl-incf depth))))
    close))

(cl-defun xml-extract-nodes
    (xml-string node-name &optional attributes n node-content-only)
  "Return matching NODE-NAME fragments from XML-STRING.
ATTRIBUTES accepts names or (NAME VALUE) regexp pairs.  Return all matches
when N is nil, the first N when positive, and matches from the end in reverse
document order when negative.  Return only node contents when
NODE-CONTENT-ONLY is non-nil."
  (let ((opening-regexp
	 "<\\([[:alpha:]_][[:alnum:]_.:-]*\\)\\(?:[[:space:]][^>]*\\)?>")
	(position 0)
	nodes)
    (while (and (or (null n) (cl-minusp n) (< (length nodes) n))
		(string-match opening-regexp xml-string position))
      (let* ((opening-begin (match-beginning 0))
	     (opening-end (match-end 0))
	     (opening-tag (match-string 0 xml-string))
	     (name (match-string 1 xml-string)))
	(setq position opening-end)
	(when (and (not (string-suffix-p "/>" opening-tag))
		   (mb-xml--full-match-p node-name name)
		   (mb-xml--attributes-match-p attributes opening-tag))
	  (let ((close (mb-xml--matching-close xml-string name opening-end)))
	    (when close
	      (push (if node-content-only
			(substring xml-string opening-end (cl-first close))
		      (substring xml-string opening-begin (cl-second close)))
		    nodes)
	      (setq position (cl-second close)))))))
    (setq nodes (nreverse nodes))
    (if (and n (cl-minusp n))
	(let ((reversed (reverse nodes)))
	  (cl-subseq reversed 0 (min (abs n) (length reversed))))
      nodes)))

(cl-defun xml-inner (xml &optional (node-name "[^>[:space:]]+"))
  "Return the contents of the first NODE-NAME fragment in XML."
  (cl-first (xml-extract-nodes xml node-name nil 1 t)))

(cl-defun xml-inner-text (xml &optional (node-name "[^>[:space:]]+"))
  "Return NODE-NAME contents after discarding child nodes."
  (let ((string (xml-inner xml node-name)))
    (when string
      (let ((children (xml-extract-nodes string "[^>[:space:]]+")))
	(dolist (child children string)
	  (setq string
		(replace-regexp-in-string
		 (regexp-quote child) "" string t t)))))))

(cl-defun xml-decode-characters (string &optional _encoding)
  "Decode XML entity and numeric character references in STRING."
  (xml-substitute-special string))

(cl-defun html-to-temp-buffer
    (html-converter html-string buffer-name &rest args)
  "Convert HTML-STRING with HTML-CONVERTER and display it in BUFFER-NAME.
Pass ARGS to HTML-CONVERTER after HTML-STRING."
  (with-output-to-temp-buffer buffer-name
    (temp-buffer-resize-mode)
    (princ
     (xml-decode-characters
      (apply html-converter html-string args)))))

(cl-defun html-to-temp-buffer-sentinel
    (temp-buffer-name html-string-converter &rest args)
  "Return a callback displaying converted HTML in TEMP-BUFFER-NAME.
HTML-STRING-CONVERTER receives the callback's HTML string followed by ARGS."
  (lambda (html-string)
    (apply #'html-to-temp-buffer
	   html-string-converter html-string temp-buffer-name args)))

(provide 'mb-utils-xml)

;;; mb-utils-xml.el ends here
