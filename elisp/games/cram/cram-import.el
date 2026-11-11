;;; cram-import.el --- Import Cram problems -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'cram-config)
(require 'cram-db-ui)
(require 'mb-utils-io)
(require 'mb-utils-strings)

(defun cram-import--field-value (field)
  "Return FIELD, converting an empty string to nil."
  (unless (string-empty-p field)
    field))

(defun cram-import--problem-arguments (row)
  "Convert a seven-field CSV ROW to Cram problem arguments."
  (unless (= (length row) 7)
    (user-error "Expected 7 CSV fields, received %d in row %S"
                (length row) row))
  (pcase-let ((`(,source-id ,question ,answer ,picture
                  ,alternatives ,hints ,_difficulty)
               row))
    (list
     (cram-import--field-value source-id)
     (cram-import--field-value question)
     (cram-import--field-value answer)
     (cram-import--field-value picture)
     (cram-import--field-value alternatives)
     (cram-import--field-value hints)
     (copy-tree +cram-default-rating+))))

(defun cram-import--problem (arguments)
  "Insert or update a problem from ARGUMENTS.
Use the first argument as the unique source identifier."
  (let ((source-id (car arguments)))
    (if (car
         (ld-select :problem
           :where (string= :source-id source-id)))
        (ld-update :problem
          (string= :source-id source-id)
          (butlast (cdr arguments))
          (:question :answer :picture :alternatives :hints))
      (cram-db-insert-problem arguments))))

(defun cram-import-csv (path)
  "Import Cram problems from the semicolon-separated CSV file PATH.
The first row is a header.  Insert new source identifiers and
update the descriptive fields of existing problems.  Return the
inserted or updated rows."
  (mapcar
   (lambda (row)
     (cram-import--problem
      (cram-import--problem-arguments row)))
   (cdr (parse-csv-file path))))

(defun cram-format-question (string)
  "Format comma-separated names in STRING as a Norwegian list."
  (andcat (split-string string ", ") ", " " og " " og "))

(defun cram-format-picture (string)
  "Convert a space-separated scientific name STRING to a JPG name."
  (pcase-let ((`(,first . ,rest) (split-string string " " t)))
    (concat
     (string-join (cons (capitalize first) rest) "_")
     ".jpg")))

(cl-defun cram-parse-planter
    (&optional (start-index 1)
               (buffer "plantescratch")
               (reference-prefix "csv-planter-"))
  "Convert tab-separated plant data in BUFFER to Cram CSV rows.
Number source identifiers from START-INDEX and prefix them with
REFERENCE-PREFIX."
  (mapconcat
   #'identity
   (cl-loop
    for (municipalities latin-name plant) in
    (parse-csv-string
     (buffer-string-no-properties buffer) "\t")
    for index from start-index
    collect
    (string-join
     (list
      (format "%s%d" reference-prefix index)
      (format "Hvilken plante er kommuneblomst i %s"
              (cram-format-question municipalities))
      plant
      (cram-format-picture latin-name)
      "" "" "3")
     ";"))
   "\n"))

(defun cram-format-norsk-fugl-ancestors (ancestors)
  "Join nonempty taxonomic ANCESTORS."
  (string-join
   (cl-remove-if #'string-empty-p ancestors)
   " - "))

(defun cram-format-norsk-fugl
    (number order family subfamily genus species norwegian-name)
  "Format one Norwegian bird as a Cram CSV row."
  (format
   "csv-norske-fugler-%d;%s - %s %s;%s;norske-fugler/%s_%s.jpg;;;3"
   number
   (cram-format-norsk-fugl-ancestors
    (list order family subfamily))
   genus species norwegian-name genus species))

(cl-defun cram-format-norske-fugler (&optional (start-index 1))
  "Format bird columns pasted into `*scratch*' as Cram CSV rows.
Number source identifiers from START-INDEX."
  (mapconcat
   #'identity
   (cl-loop
    for line in
    (split-string
     (buffer-string-no-properties "*scratch*") "\n" t)
    for fields = (split-string line "\t")
    for order = (nth 0 fields)
    for family = (nth 4 fields)
    for subfamily = (nth 5 fields)
    for genus = (nth 6 fields)
    for species = (nth 7 fields)
    for norwegian-name = (nth 9 fields)
    for number from start-index
    collect
    (cram-format-norsk-fugl
     number order family subfamily genus species norwegian-name))
   "\n"))

(provide 'cram-import)

;;; cram-import.el ends here
