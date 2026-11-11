;;; quiz-csv.el --- Export quiz buffers as CSV -*- lexical-binding: t; -*-

;;; Code:

(require 'subr-x)
(require 'quiz-format)

(defgroup quiz-csv nil
  "CSV export for quiz buffers."
  :group 'applications)

(defcustom quiz-csv-directory
  (expand-file-name "quiz-csv/" user-emacs-directory)
  "Directory used for exported quiz CSV files."
  :type 'directory)

(defun quiz-csv--buffer (buffer-designator)
  "Return the live buffer specified by BUFFER-DESIGNATOR."
  (or (get-buffer buffer-designator)
      (user-error "Quiz buffer does not exist: %S"
                  buffer-designator)))

(defun quiz-csv--field (value)
  "Escape VALUE as one semicolon-separated CSV field."
  (let ((string (or value "")))
    (if (string-match-p "[;\"\n\r]" string)
        (concat
         "\""
         (replace-regexp-in-string "\"" "\"\"" string t t)
         "\"")
      string)))

(defun quiz-csv--row (fields)
  "Serialize FIELDS as one semicolon-separated CSV row."
  (mapconcat #'quiz-csv--field fields ";"))

(defun quiz-trim-q-item-string (item-string)
  "Trim ITEM-STRING and collapse internal whitespace."
  (string-trim
   (replace-regexp-in-string
    "[[:space:]]+" " " item-string)))

(defun quiz-parse-q (question-string)
  "Parse QUESTION-STRING into its ordered quiz item texts."
  (mapcar
   #'quiz-trim-q-item-string
   (split-string
    question-string
    (format "^[%s][0-9]+:[ \t]*"
            (regexp-quote quiz-item-prefixes))
    t)))

(defun quiz-collect-qs (buffer-designator)
  "Return question strings from BUFFER-DESIGNATOR."
  (with-current-buffer
      (quiz-csv--buffer buffer-designator)
    (split-string
     (buffer-substring-no-properties (point-min) (point-max))
     "\n[ \t]*\n+"
     t
     "[[:space:]]+")))

(defun quiz-parse-list (buffer-designator)
  "Return parsed question rows from BUFFER-DESIGNATOR."
  (mapcar
   #'quiz-parse-q
   (quiz-collect-qs buffer-designator)))

(defun quiz-convert-to-csv
    (buffer-designator &optional remove-source)
  "Convert quiz BUFFER-DESIGNATOR to semicolon-separated CSV.
When REMOVE-SOURCE is non-nil, remove the final item from each
question row."
  (mapconcat
   (lambda (row)
     (quiz-csv--row
      (if remove-source
          (butlast row)
        row)))
   (quiz-parse-list buffer-designator)
   "\n"))

(defun quiz-csv-path (buffer-designator)
  "Return the CSV export path for BUFFER-DESIGNATOR.
BUFFER-DESIGNATOR may also be a source file name."
  (let* ((buffer (get-buffer buffer-designator))
         (source-name
          (if buffer
              (or (buffer-file-name buffer)
                  (buffer-name buffer))
            (if (stringp buffer-designator)
                buffer-designator
              (user-error
               "Invalid quiz buffer: %S"
               buffer-designator)))))
    (expand-file-name
     (concat
      (file-name-base source-name)
      ".csv")
     quiz-csv-directory)))

(defun quiz-save-as-csv
    (buffer-designator &optional remove-source)
  "Export BUFFER-DESIGNATOR as CSV and return the output path.
When REMOVE-SOURCE is non-nil, omit the final item in each row."
  (let ((path (quiz-csv-path buffer-designator)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert
       (quiz-convert-to-csv
        buffer-designator remove-source)))
    path))

(defun quiz-insert-into-excel
    (buffer-designator &optional remove-source)
  "Export BUFFER-DESIGNATOR for spreadsheet import.
When REMOVE-SOURCE is non-nil, omit the final item in each row."
  (quiz-save-as-csv buffer-designator remove-source))

(defun quiz-insert-current-buffer-into-excel ()
  "Export the current quiz buffer without source fields."
  (interactive)
  (quiz-insert-into-excel (current-buffer) t))

(provide 'quiz-csv)

;;; quiz-csv.el ends here
