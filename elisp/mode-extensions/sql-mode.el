;; postgres
;; TODO: move this somewhere else
(setq split-width-threshold nil)
(defun sql-send-expression ()
  "Sends one-line expressions to the SQL output buffer. 
It handles also lines commented with --.
Examples:
--select * from mytable; (sends \"\nselect * from mytable;\")
select * from mytable;   (sends \"\nselect * from mytable;\")

\(Note the important prefixed NEWLINE, so correct aligning not displaced by
the output buffer prompt.)"
  (interactive)
    (let* ((regexp "^\\(.*--\\)?\\(.*\\)")
	   (s (string-trim (string-match* regexp (current-line-as-string) :num 2))))
      (sql-send-string (format "\n%s" s))))
(define-key sql-mode-map (kbd "C-c C-e") 'sql-send-expression)

