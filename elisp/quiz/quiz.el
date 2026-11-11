;;; quiz.el --- Edit legacy quiz files -*- coding: utf-8; -*-

;;; TODO
;;; * this file should only contain settings and mappings, maybe also other
;;;   methods that do not belong to other files
;;; * this file should control including the other quiz lisp files (with
;;;   'require etc)

;;; o mail function (with mail list)
;;; o fix fill prefix thing (maybe the best thing is to provide a quiz mode)
;;; o coloring (set appropriate faces)

;;; * make a re-number-items method (sometimes a q-question must be deleted, and
;;;   then...)
;;; * base methods on "objects": 
;;;   · q-file/q-buffer (quiz buffer or summary quiz buffer)
;;;   · q-list (entire quiz-buffer or region of summary buffer)
;;;   · q-question (entity containing at least a q-item)
;;;   · q-item (either Q, A, S by now; later I might add C for comment, T: for
;;;     the durations of the validity)

;; see legenda.txt for categories

(require 'cl-lib)
(require 'subr-x)
(require 'quiz-format)
(require 'quiz-navigation)
(require 'quiz-mode)
(require 'quiz-config)
(require 'quiz-edit)
(require 'quiz-layout)
(require 'quiz-csv)
;;;;;;;;;;;;;;;; UTILS ;;;;;;;;;;;;;;;;;;

(cl-defun buffer-replace (string1 string2)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward string1 nil t)
    (replace-match string2 t))))

(cl-defun buffer-replace-multi (strings1 strings2)
  (cl-loop for string1 in strings1
	for string2 in strings2
	do (buffer-replace string1 string2)))

(cl-defun quiz-clean-up-mule-shit ()
  (interactive)
  (buffer-replace-multi
   '("Ã¥" "Ã¸" "Â«" "Â»" "Ã¶" "Ã¦")
   '("å" "ø" "«" "»" "ö" "æ")))

;;;;;;;;;;;;;;; BASIC ;;;;;;;;;;;;;;

(cl-defun quiz-parse-buffer (&optional (buffer (current-buffer)))
  (let ((quiz-item-prefixes "QAE"))
    (save-excursion
      (goto-char (point-min))
      (cl-loop for beg = (quiz-beginning-of-question)
	    for end = (quiz-end-of-question 1)
	    for item-string = (string-trim (buffer-substring-no-properties beg end))
	    collect (quiz-parse-item item-string)
	    until (not (quiz-forward-question 1))))))

(defconst +quiz-ignore-chars-regexp+ "[^[:alnum:])»\"]")

;;; Editing
(cl-defun quiz-super-save ()
  "Cleans Q, saves buffer and return to other window.
TODO: factorize this and clean answer as well."
  (interactive)
  (awhen (marked-text-region)
    (apply #'quiz-move-active-region-to-a it))
  (quiz-beginning-of-item "Q")
  (eol)
  (quiz-end-of-item "Q")
  ;; Remove junk at end
  (while (looking-back +quiz-ignore-chars-regexp+)
    (backward-delete-char 1))
  ;; Add ? if necessary
  (unless (looking-back "?")
    (insert ??))
  ;; Clean paragraph
  (fill-paragraph)
  (quiz-backward-item)
  (forward-word 1)
  (capitalize-word 1)
  ;; Clean answer paragraph
  (quiz-forward-item)
  (capitalize-word 1)
  (fill-paragraph)
  (quiz-end-of-item "A")
  (while (looking-back +quiz-ignore-chars-regexp+)
      (backward-delete-char 1))
  ;; Save and return to other buffer
  (save-buffer)
  (other-window 1))

(cl-defun quiz-move-active-region-to-a (beg end)
  (interactive "r")
  (cl-assert (quiz-current-item-p :question))
  (let ((s (string-trim (delete-and-extract-region beg end))))
    (just-one-space)
    (save-excursion
      (quiz-forward-item)
      (insert (string-trim* s "[^[:alnum:]()«»]")))))

(cl-defun quiz-remove-A ()
  "Removes next A item"
  (interactive)
  (when (re-search-forward "^A" nil t)
    (beginning-of-line)
    (set-mark-command nil)
    (re-search-forward "^A.*\n\\([^QAS\n].*\n\\)*")
    (kill-region (point) (mark))))

(cl-defun quiz-remove-S ()
  "Removes next S item"
  (interactive)
  (when (re-search-forward "^S" nil t)
    (beginning-of-line)
    (set-mark-command nil)
    (re-search-forward "^S.*\n\\([^QAS\n].*\n\\)*")
    (kill-region (point) (mark))
    t))  

(cl-defun add-iso-day (iso-date &optional n)
  "Returns a date that is ISO-DATE + N. Default value for N is 1. Both
string ISO-DATE and the returned string are in iso date format. TODO:
why is this method but here?"
  (iso-date (add-time iso-date :day (or n 1))))
;;(add-iso-day "2004-03-23")

(cl-defun quiz-remove-A-buffer ()
  "Obsolete?"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (quiz-remove-A))))

(cl-defun quiz-remove-S-buffer ()
  "Obsolete?"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (quiz-remove-S))))

(cl-defun quiz-question-to-string (&optional (delimiter ";"))
  "Converts current question to csv-string"
  (save-excursion
    (let* ((q-a (quiz-beginning-of-item "Q"))
	   (q-b (quiz-end-of-item "Q"))
	   (a-a (quiz-beginning-of-item "A"))
	   (a-b (quiz-end-of-item "A")))
      (region-to-string :start q-a :end q-b))))
;;(quiz-question-to-string)

(cl-defun quiz-compile (&key (iso-from (iso-date))
			   (iso-to (add-iso-day iso-from))
			   (items quiz-item-prefixes))
  "Generates all quiz available in the interval [ISO-FROM ISO-TO).
Optional argument controls if the answers and source field
respectively are to be included for each question record."
  (unless (not-empty items)
    (error "Items string cannot be empty!"))
  (let* ((buffer-name (if (string= iso-to (add-iso-day iso-from))
			(format "mq-%s-%s" iso-from items)
			(format "mq-%s-to-%s-%s" iso-from iso-to items)))
	 (buffer (get-buffer-create buffer-name)))
    (with-buffer buffer
      (kill-region (point-min) (point-max))
      (cl-loop for iso-d = iso-from then (iso-date (add-time iso-d :day 1))
	    for filename = (format "mq-%s" iso-d)
	    for path = (quiz-file-path (format "%s.qz" filename))
	    until (string= iso-d iso-to)
	    if (file-exists-p path) do 
	    (insert filename)
	    (insert-underline "=")
	    (newline 2)
	    (insert-file path)
	    (goto-char (point-max))
	    (delete-blank-lines)
	    (newline 2))
      (delete-blank-lines)
      (goto-char (point-min))
      (and (not (cl-find ?Q items)) (quiz-remove-Q-buffer))
      (and (not (cl-find ?A items)) (quiz-remove-A-buffer))
      (and (not (cl-find ?S items)) (quiz-remove-S-buffer))
      (quiz-mode))
    (switch-to-buffer buffer)
    (set (make-local-variable 'fill-paragraph-function) #'quiz-fill-item)))

(cl-defun quiz-summary (n items)
  (quiz-compile :iso-from (add-iso-day (iso-date) (- 1 n))
		:iso-to (add-iso-day (iso-date))
		:items items))

(cl-defun quiz-summary-print (n)
  (interactive "P")
  (quiz-summary (or n 14) "QA"))

(cl-defun quiz-summary-mail (n)
  (interactive "P")
  (quiz-summary (or n 7) "QS"))

;;; Mail functionality
(defconst quiz-default-mail-group "Hovedstyret av 1914")
(defconst quiz-mail-groups
  (list (list "Hovedstyret av 1914"
	      (list "Tore van Dahl <toredah@student.matnat.uio.no>"
		    "Per Stovne <perstovne@hotmail.com>"
		    "Truls Flatberg <Truls.Flatberg@sintef.no>"
		    "Ole Martin Halck <omhalck@online.no>"
		    "Truls Flatberg <trulsf@ifi.uio.no>"
		    "Thomas Bergkirk <thomas.bergkirk@rubicontv.no>"))))

(cl-defun purify-address (address)
  (string-match* "<?\\([^ >]*@[^ >]*\\)>?" address 1))

(cl-defun quiz-mail-buffer (refresh)
  "Sends current quiz buffer as mail to recipients"
  (interactive "P")
  (if refresh (mb-refresh-addresses))
  (let* ((group quiz-default-mail-group)
	 (to (concat* (second (assoc group quiz-mail-groups))
	       :in ", "
	       :key #'purify-address))
	 (subject (buffer-name))
	 (body (buffer-string)))
    (gnus-msg-mail "" subject)
    (auto-fill-mode -1)
    (message-goto-to)
    (insert to)
    (message-goto-body)
    (insert body)))
;;(quiz-mail-buffer 1)
;;(concat* (second (assoc quiz-default-mail-group quiz-mail-groups)) :in ", ")

(cl-defun quiz-summary-edit-question-1
    (number &optional (date (midnight)) (path quiz-directory))
  "Opens the file with the requested question defined by NUMBER and
DATE. TODO: coordinate this with goto methods, ie. goto-Q etc."
  (interactive "P")
  (find-file (quiz-get-file date path))
  (goto-char (point-min))
  (re-search-forward (format "Q%d: " number)))
;;(quiz-summary-edit-question 2 "2005-02-21")

(cl-defun quiz-summary-edit-question (&optional (path quiz-directory))
  "Opens the file with the requested question defined by number and date."
  (interactive)
  (let (n iso-date) 
    (save-excursion
      (end-of-line)
      (re-search-backward (format "[%s]\\([0-9]+\\)" quiz-item-prefixes))
      (setq n (string-to-integer (match-string 1)))
      (re-search-backward (format "mq-\\(%s\\)" *iso-date*))
      (setq iso-date (match-string 1))
      (quiz-summary-edit-question-1 n iso-date path))))

(require 'mb-utils-buffer)

(cl-defun quiz-convert-aftenposten-nyhetsquiz-question-to-qz-question
    (beg end n &optional s)
  "STRING is aftenposten question string. Returns a string question in
qz format. The question is numbered N."
  (let* ((string (buffer-substring beg end))
	 (q (string-match* "^[0-9]+\\.\\s-*\\(.*\\)$" string 1))
	 (a (string-match* "^Riktig svar:\\s-*\\(.*\\)$" string 1))
	 s)
    (kill-region beg end)
    (goto-char beg)
    (insert (quiz-item-q n q))
    (insert (quiz-item-a n a))
    (when s (insert (quiz-item-s n s)))))

(cl-defun quiz-convert-aftenposten-nyhetsquiz-to-qz-file ()
  (interactive)
  (let ((n 0)) 
    (buffer-do-regions beg end ("\\(.*\n.*\n\\)\\s-*\n" 1)
      ;;(message (buffer-substring beg (+ beg 20)))
      (quiz-convert-aftenposten-nyhetsquiz-question-to-qz-question
       beg end (cl-incf n)))))

(provide 'quiz)

;; For referering til IDLs hjemmesider:
;; http://www.mylder.no/idl/views/640&ref0=291

;; Kirkeinndelinger og rettslige inndelinger:
;; http://www.ssb.no/aarbok/tab/t-000010-001.html

;; http://www.kongehuset.no/ 
