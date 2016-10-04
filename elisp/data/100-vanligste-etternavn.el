;;;; This module downloads the 100 most common surnames in Norway. It
;;;; also provide a util to determine the most common non-sen name.
;;;; To test
;;;; 1. Eval this buffer (M-x eval-buffer)
;;;; 2. Eval (100-vanligste-etternavn)
;;;; 3. Eval (ve-sans-sen), see doc below.
;;;;
;;;; Oslo, 2016-10-03
;;;; Mats Bergstrøm
(require 'wget)
(require 'mb-utils-xml)

(defun 100ve-html ()
  (first (xml-extract-nodes 
	  (wget-to-string "https://www.ssb.no/a/navn/alf/etter100.html")
	  "table")))
;;(100ve-html)

(defun 100ve-rows (html)
  ;; rest: skip header row
  (rest (xml-extract-nodes html "tr")))
;;(100ve-rows (100ve-html))

(defun 100ve-entries (rows)
  (loop for x in rows
	collect (xml-extract-nodes x "td")))
;;(100ve-entries (100ve-rows (100ve-html)))

(defun 100-vanligste-etternavn ()
  "Returns a tree (ROW1 ROW2 ...), where ROW is a triplet \(RANK SURNAME NUMBER\)."
  (let ((es (100ve-entries (100ve-rows (100ve-html)))))
    (loop for row in (maptree #'xml-inner es)
	  collect (list (string-to-number (nth 0 row))
			(string-trim (nth 2 row))
			(string-to-number (nth 3 row))))))
;;(100-vanligste-etternavn)

(defalias '100ve #'100-vanligste-etternavn)

(defun sen-name-p (name)
  (and (> (length name) 3) (string= "sen" (substring* name -3))))
;;(mapcar #'sen-name-p '("Hansen" "sen" "Berg"))

(defun ve-sans-sen ()
  "Return the most commeon non-sen surnames in Norway
as of 2013 (source: SSB)"
  (cl-remove-if #'sen-name-p (100ve) :key #'second))
;;(mapcar #'second (ve-sans-sen))
