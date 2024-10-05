(defconst norwegian-alphabet "abcdefghijklmnopqrstuvwxyz���")

(cl-defun norwegian-names-url (letter gender &optional (number-of-names 3))
  (format "http://www.norskenavn.no/%s.php?bokstav=%s&antallnavn=%d"
    (if (eq gender 'male) "guttenavn" "jentenavn")
    (string-case (upcase letter)
      ("�" "%C6")
      ("�" "%D8")
      ("�" "%C5")
      (t (upcase letter)))
    number-of-names))
;;(norwegian-names-url "�" 'male)
;;(mapcar #'(lambda (x) (norwegian-names-url x 'male)) (split-string norwegian-alphabet "" t))

(cl-defun norwegian-names-with-first-letter-extract (letter gender &optional (number-of-names 3))
  (xml-extract-nodes 
   (substring-intv 
       (wget-to-string (norwegian-names-url letter gender number-of-names)) 
     (interval-co "Lista viser " "Lista over viser ")) "a" '("*") nil t))
;;(norwegian-names-extract "�" 'male)

(cl-defun norwegian-names-extract-gender (gender &optional (number-of-names 3))
  (cl-loop for letter in (split-string norwegian-alphabet "" t)
	append (norwegian-names-with-first-letter-extract letter gender number-of-names)))
;;(length (norwegian-names-extract-gender 'male))

(cl-defun norwegian-names-extract-all (&optional (number-of-names 3))
  (cl-loop for gender in '(male female)
	collect (list gender (norwegian-names-extract-gender gender))))

(cl-defun norwegian-names-to-csv-file (&optional (filename "norwegian-names.csv") (number-of-names 3))
 (let ((names (mapcar #'flatten (norwegian-names-extract-all number-of-names))))
   (string-to-file (csv-string names) filename t)))
;;(norwegian-names-to-csv-file)
