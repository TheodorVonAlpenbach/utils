;;;; Not so useful module at the moment...
;;;; But it may change (and it was bigger, once)

(defconst solr-identifier-regexp
  "[[:alpha:]][[:alnum:]_]*"
  "Only alpha-numeric characters, after an initial alphabetic
character" )

(defconst solr-expression-delimiters
  "[[:space:]()&|]")

(defconst solr-field-regexp
  (format "\\(?:[([:space:]]\\|^\\)\\(%s\\):" solr-identifier-regexp)
  "Note the empty expression matching whitespace and,
  importantly, the left parenthesis. The the latter were not
present, the following expression would not match:
\(tag:value\)")

(defconst api-period-macro-identifier-regexp
  (format "%s\\(%s\\)\\(%s\\|$\\|[-+\/]\\)" solr-field-regexp solr-identifier-regexp solr-expression-delimiters))

(provide 'solr)
