;;; TODO
;;; * this file should only contain settings and mappings, maybe also other methods that do not belong to other files
;;; * this file should control including the other quiz lisp files (with 'require etc)

;;; o mail function (with mail list)
;;; o fix fill prefix thing (maybe the best thing is to provide a quiz mode)
;;; o coloring (set appropriate faces)

;;; * make a re-number-items method (sometimes a q-question must be deleted, and then...)
;;; * base methods on "objects": 
;;;   · q-file/q-buffer (quiz buffer or summary quiz buffer)
;;;   · q-list (entire quiz-buffer or region of summary buffer)
;;;   · q-question (entity containing at least a q-item)
;;;   · q-item (either Q, A, S by now; later I might add C for comment, T: for the durations of the validity)

;; see legenda.txt for categories


;;;;;;;;;;;;;;;; UTILS ;;;;;;;;;;;;;;;;;;

(defun buffer-replace (string1 string2)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward string1 nil t)
    (replace-match string2 t))))

(defun buffer-replace-multi (strings1 strings2)
  (loop for string1 in strings1
	for string2 in strings2
	do (buffer-replace string1 string2)))

(defun quiz-clean-up-mule-shit ()
  (interactive)
  (buffer-replace-multi
   '("Ã¥" "Ã¸" "Â«" "Â»" "Ã¶" "Ã¦") 
   '("å" "ø" "«" "»" "ö" "æ")))

;;;;;;;;;;;;;;; BASIC ;;;;;;;;;;;;;;


(defconst *quiz-buffer* "*quiz*" "")
(defconst *quiz-dir* (concat *shared-data-dir* "quiz/") "")
(defconst *quiz-without-source-dir* (concat *quiz-dir* "without-source/") "")
(defconst *quiz-scratch-buffername* "scratch.qz")
(defconst quiz-popup-buffer-size 12)

(defconst *quiz-items-long*
  '((:question "Q")
    (:answer "A")
    (:source "S")
    (:comment "C") 
    (:difficulty "D")
    (:alternatives "E")))

(defconst *quiz-items-short*
  (apply #'concat (mapcar #'second *quiz-items-long*))) ; ==> "QASCDE"

(cl-defun quiz-set-vars (&optional (items-long *quiz-items-long*))
  "TODO: Disallow Q0000 etc as label format. Currently, this method is obsolete"
  (setq *quiz-items-short* (apply #'concat (mapcar #'second items-long))))


(defun quiz-item-prefix (item) (second (assoc item *quiz-items-long*)))
;;(quiz-item-prefix :answer)

(cl-defun quiz-item (item n &optional (string ""))
  (format "%s%d: %s\n" (quiz-item-prefix item) n string))
;;(quiz-item :question 1 "What's the capital of Transylvania?")

;;short-cuts
(cl-defun quiz-item-q (n &optional (string "")) (quiz-item :question n string))
(cl-defun quiz-item-a (n &optional (string "")) (quiz-item :answer n string))
(cl-defun quiz-item-s (n &optional (string "")) (quiz-item :source n string))
(cl-defun quiz-item-e (n &optional (string "")) (quiz-item :alternatives n string))
;;(quiz-item-e 10 "qwe\n")

(cl-defun quiz-regexp-item-start (&optional (items-short *quiz-items-short*) (forward t))
  "Returns a regular expression that matches the beginning of a
q-item. If FORWARD is nil it returns instead an expression that
matches the end of a q-item. ITEMS specifies which types of q-items
that are recognized."
  (if forward 
    (format "^[%s][0-9]+: " items-short)
    (format "\n*[%s][0-9]+: " items-short)))
;;(quiz-regexp-item-start :forward nil)


;;;;;;;;;;;;;;;;;;;; quiz-navigation ;;;;;;;;;;;;;;;;;;;


;;;   · q-file/q-buffer (quiz buffer or summary quiz buffer)
;;;   · q-list (entire quiz-buffer or region of summary buffer)
;;;   · q-question (entity containing at least a q-item, separated by at least one blank line)
;;;   · q-item (either Q (mandatory), A, S by now; later I might add C
;;;     for comment, T: for the durations of the validity)
;;;     q-item <-- q-item-header q-item-header-space q-item-text
;;;   · q-item-text (section after q-item-header)

;;; q-list
(defun quiz-beginning-of-list ()
  "Beginning of list is a header"
  (goto-char (point-min)))

(defun quiz-end-of-list ()
  "End of list is before next header or end of buffer."
  (error "Not implemented"))

(cl-defun quiz-list-set-numbers (&optional (base 1))
  (quiz-beginning-of-list)
  (while (quiz-question-set-numbers base)
    (incf base)))

;;; q-question
(cl-defun quiz-forward-question (&optional (n 1))
  "Moves cursor to the next q-question beginning."
  (interactive "p")
  (end-of-line)
  (while (and (> n 0)
	      (re-search-forward (quiz-regexp-item-start "Q" t) nil t))
    (decf n))
  (if (zerop n)
    ;; if movement was actually performed, return point, else nil
    (progn (beginning-of-line) (point))
    (progn (goto-char (point-max)) nil)))

(cl-defun quiz-backward-question (&optional (n 1))
  (interactive "p")
  (quiz-forward-question (-n)))

(cl-defun quiz-beginning-of-question (&optional (n 1))
  (next-line 1)
  (if (re-search-backward "\n\n" nil t n)
    (beginning-of-line 2)
    (goto-char (point-min)))
  (point))

(cl-defun quiz-end-of-question (&optional (n 1))
  (previous-line 0)
  (if (re-search-forward "\n\n" nil t n)
    (backward-char 2)
    (progn (goto-char (point-max))
	   (when (bolp)
	     (backward-char 1))))
  (point))

(defun quiz-goto-question (n)
  "Starts at beginning of q-list and moves cursor to the beginning of
the first q-question labeled N"
  (error "Not implemented"))

(defun quiz-question-set-numbers (n)
  "Returns new point or nil if last question"
  (quiz-beginning-of-question 1)
  (let* ((point (point))
	 (end-point (quiz-end-of-question)))
    (quiz-beginning-of-question)
    (while (and point
		(< point end-point))
      (quiz-item-set-number n)
      (setq point (quiz-forward-item)))
    point))

;;; q-item
(cl-defun quiz-item-beg-regexp (&optional (items-short *quiz-items-short*)) 
  (format "^[%s][0-9]+: " items-short))
;;(quiz-item-beg-regexp)

(cl-defun quiz-forward-item-1 (&optional (n 1) (items-short *quiz-items-short*))
  "TODO: finish n < 0"
  (error "Who is invoking!?")
  (cond
   ((> n 0)
    (beginning-of-line 2)
    (forward-char 1)
    (while (and (> n 0)
		(re-search-forward (quiz-regexp-item-start items-short) nil t))
      (decf n))
    (beginning-of-line)
    (re-search-backward "\\S-"))
   ((< n 0))))

(cl-defun quiz-forward-item (&optional (items-short *quiz-items-short*))
  (re-search-forward (quiz-regexp-item-start items-short t) nil t))

(cl-defun quiz-backward-item (&optional (items-short *quiz-items-short*))
  (re-search-backward (quiz-regexp-item-start items-short nil) nil nil))

(cl-defun quiz-beginning-of-item (&optional (items-short *quiz-items-short*))
  "Move point to beginning of current item."
  (interactive)
  (end-of-line)
  (quiz-backward-item items-short))
;;

(cl-defun quiz-end-of-item (&optional (items-short *quiz-items-short*))
  ""
  (quiz-current-item-region-end items-short))

(cl-defun quiz-beginning-of-item-text (&optional (items-short *quiz-items-short*))
  (quiz-beginning-of-item items-short)
  (re-search-forward (quiz-item-beg-regexp)))

(defun quiz-item-set-number (n)
  (quiz-beginning-of-item)
  (forward-char 1)
  (kill-word 1)
  (insert (number-to-string n)))

(defun quiz-item-insert ()
  (error "Not implemented"))
;;;;;;;;

(cl-defun quiz-current-item-region-start (&optional (items-short *quiz-items-short*))
  (save-excursion
    (end-of-line)
    (re-search-backward (quiz-regexp-item-start items-short t) nil t)))

(cl-defun quiz-current-item-region-end-old (&optional (items-short *quiz-items-short*))
  (save-excursion
    (if (re-search-backward (quiz-regexp-item-start items-short nil) nil t)
      (re-search-forward (quiz-regexp-item-start *quiz-items-short* t) nil t)
      (point-max))))

(cl-defun quiz-current-item-region-end (&optional (items-short *quiz-items-short*))
  (re-search-forward (quiz-regexp-item-start *quiz-items-short* t) nil t)
  (eol :offset -1))

(defun quiz-goto-question-end ()
  "Moves point to end of current question section."
  (re-search-forward "\n\\s-*$" nil t 1))

(cl-defun quiz-question-end (&optional (point (point)))
  (quiz-save-excursion point
    (quiz-goto-question-end)))

(cl-defun quiz-beginning-of-item-1 (position &optional (item ""))
  "Leaps to beginning of the given ITEM text in the current question
section. Returns the result position or nil if ITEM does not exists in
the current q-question. If POSITION is nil point is set at beginning
of the given ITEM text. Else if POSITION is :NUMBER, point is set at
first item number char. Else point is set at beginning of ITEM. If
ITEM is not given or \"\", the item at point is used."
  (interactive "P")
  (error "Who is using this!?")
  (and (quiz-goto-item-start item)
       (if (not position) 
	 (forward-char (quiz-hanging-column))
	 (and (eq position :number)
	      (< (point) (point-max))
	      (forward-char 1)))))
;;(quiz-beginning-of-item :number)

;;; Insert
(cl-defun quiz-insert-empty-question ()
  (interactive)
  (quiz-insert-question "" "" "" (current-buffer))
  (quiz-forward-item))

(cl-defun quiz-insert-question-base (question answer source &optional (point (point)))
  "TODO: get behaviour from #'LYNX-INSERT-REGION-IN-QUIZ-OUTLINE"
  (goto-char point)
    (let ((n (quiz-read-n-prev-question)))
      (delete-blank-lines)
      (unless (<= n 1) (newline))
      (insert (quiz-item-q n question))
      (insert (quiz-item-a n answer))
      (insert (quiz-item-s n source)))
    (newline)
    (point))

(cl-defun quiz-insert-question (question answer source &optional (buffer (quiz-get-buffer)) (point (point-max)))
  "TODO: get behaviour from #'LYNX-INSERT-REGION-IN-QUIZ-OUTLINE"
  (with-buffer buffer
    (quiz-insert-question-base question answer source point)))
;;(quiz-insert-question "qwe" "ewq" "")


;;;; Quiz mode and GUI

(defun quiz-mode () "Major mode for editing .qz files.
 \\{quiz-mode-map}
 \\<quiz-mode-map>"
       (interactive)
       (text-mode)
       (kill-all-local-variables)
       (use-local-map quiz-mode-map)
       (setq local-abbrev-table text-mode-abbrev-table)
       (setq major-mode 'quiz-mode)
       (setq mode-name "Quiz mode")
       (setq buffer-offer-save t)	;but why?
       (set (make-local-variable 'font-lock-defaults) '(quiz-font-lock-keywords))
       (set (make-local-variable 'fill-paragraph-function) #'quiz-fill-item)
       (activate-input-method 'norwegian-keyboard)
;;       (add-hook 'quiz-mode-hook)
       (run-hooks 'text-mode-hook 'quiz-mode-hook))

(defconst quiz-font-lock-keywords
  (purecopy
   (list (list "Q[1-9][0-9]*:" 0 font-lock-function-name-face 'append)
	 (list "A[1-9][0-9]*:" 0 font-lock-variable-name-face 'append)
	 (list "S[1-9][0-9]*:" 0 font-lock-type-face 'append)
	 (list "C[1-9][0-9]*:" 0 font-lock-builtin-face 'append)
	 (list "E[1-9][0-9]*:" 0 font-lock-constant-face 'append))) ;;font-lock-constant-face
  "Additional expressions to highlight in Lynx mode.")

;;(defconst quiz-mode-map global-map)
(defconst quiz-mode-map
  (let ((map (make-sparse-keymap))
	(local-prefix-map (make-sparse-keymap))
	(summary-map (make-sparse-keymap))
	(insert-map (make-sparse-keymap))
	(edit-map (make-sparse-keymap)))

    ;; maps
    (define-key map "\C-\\" local-prefix-map) ;disables #'toggle-input-method (some MULE shit)
    (define-key local-prefix-map "s" summary-map)
    (define-key local-prefix-map "i" insert-map)
    (define-key local-prefix-map "e" edit-map)

    (define-key local-prefix-map "m" #'quiz-mail-buffer)
    (define-key local-prefix-map "\C-\\" #'quiz-super-save)

    ;; insert map
    (define-key insert-map "e" #'quiz-insert-empty-question)

    ;; summary map
    (define-key summary-map "m" #'quiz-summary-mail)
    (define-key summary-map "p" #'quiz-summary-print)

    ;; edit map
    (define-key edit-map "m" #'quiz-move-active-region-to-a)

    ;; other keys
    (setq quiz-mode-map map)) "Keymap used in quiz mode.")

;;; Queries
(defun quiz-current-item ()
  (save-excursion
    (quiz-beginning-of-item)
    (first (find (buffer-substring* :start (point) :length 1)
		 *quiz-items-long* :key #'second :test #'string=))))

(defun quiz-current-item-p (item)
  (eql (quiz-current-item) item))

(cl-defun quiz-get-buffer (&optional (buffer-name (format "mq-%s.qz" (iso-date))))
  "Returns the standard quiz buffer. If the underlying file does not
exist, it is created, and the buffers mode is set to TEXT-MODE and
other local variables are set."
  (or (get-buffer buffer-name)
      (let ((buffer (find-file-noselect (concat *quiz-dir* buffer-name))))
	(with-buffer (buffer-name buffer)
	  (quiz-mode))
	buffer)))

(cl-defun quiz-read-n-prev-question (&optional (point (point)))
  (interactive)
  (if (quiz-current-item-number)
    (1+ (string-to-int (match-string 1))) 
    1))

(defconst quiz-hanging-space 1)

(cl-defun quiz-current-item-number (&optional (point (point)) (items *quiz-items-short*))
  (save-excursion
    (goto-char point)
    (end-of-line)
    (if (re-search-backward (format "^[%s]\\([0-9]+\\):" items) nil t)
      (string-to-int (match-string 1))
      ;; point is in front of first item if there is any
      (if (re-search-forward (format "^[%s]\\([0-9]+\\):" items) nil t)
	(string-to-int (match-string 1))))))
;;(quiz-current-item-number)

(cl-defun quiz-paragraph-indent (&optional (point (point)) (items *quiz-items-short*))
  "3 or 4"
  (+ 2 (if (< (quiz-current-item-number point) 10)
	 1 2)))

(cl-defun quiz-hanging-column (&optional (point (point)) (items *quiz-items-short*))
  "4 or 5"
  (+ quiz-hanging-space (quiz-paragraph-indent point)))

(defun quiz-fill-item (&optional justify)
  "Fills schedule paragraphs. Too macroish implemented."
  (interactive "P")
  (save-excursion
    (let ((fill-prefix (make-string (quiz-hanging-column) 32)))
      (fill-region-as-paragraph (quiz-current-item-region-start)
				(quiz-current-item-region-end) 
				justify))))

(cl-defun quiz-parse-buffer (&optional (buffer (current-buffer)))
  (let ((*quiz-items-short* "QAE"))
    (save-excursion
      (goto-char (point-min))
      (loop for beg = (quiz-beginning-of-question)
	    for end = (quiz-end-of-question 1)
	    for item-string = (string-trim (buffer-substring-no-properties beg end))
	    collect (quiz-parse-item item-string)
	    until (not (quiz-forward-question 1))))))

(defconst +quiz-ignore-chars-regexp+ "[^[:alnum:])»\"]")

;;; Editing
(defun quiz-super-save ()
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
  (assert (quiz-current-item-p :question))
  (let ((s (string-trim (delete-and-extract-region beg end))))
    (just-one-space)
    (save-excursion
      (quiz-forward-item)
      (insert (string-trim s "[^[:alnum:]()«»]")))))

(defun quiz-remove-A ()
  "Removes next A item"
  (interactive)
  (when (re-search-forward "^A" nil t)
    (beginning-of-line)
    (set-mark-command nil)
    (re-search-forward "^A.*\n\\([^QAS\n].*\n\\)*")
    (kill-region (point) (mark))))

(defun quiz-remove-S ()
  "Removes next S item"
  (interactive)
  (when (re-search-forward "^S" nil t)
    (beginning-of-line)
    (set-mark-command nil)
    (re-search-forward "^S.*\n\\([^QAS\n].*\n\\)*")
    (kill-region (point) (mark))
    t))  

(defun add-iso-day (iso-date &optional n)
  "Returns a date that is ISO-DATE + N. Default value for N is 1. Both
string ISO-DATE and the returned string are in iso date format. TODO:
why is this method but here?"
  (iso-date (add-time iso-date :day (or n 1))))
;;(add-iso-day "2004-03-23")

(defun quiz-remove-A-buffer ()
  "Obsolete?"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (quiz-remove-A))))

(defun quiz-remove-S-buffer ()
  "Obsolete?"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (quiz-remove-S))))

(cl-defun quiz-get-file (&optional (date (midnight)) (path *quiz-dir*))
  (format "%smq-%s.qz" path (iso-date (parse-time date))))
;;(quiz-get-file "2005-02-22")

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
			   (items *quiz-items-short*))
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
      (loop for iso-d = iso-from then (iso-date (add-time iso-d :day 1))
	    for filename = (format "mq-%s" iso-d)
	    for path = (format "%s%s.qz" *quiz-dir* filename)
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
      (and (not (find ?Q items)) (quiz-remove-Q-buffer))
      (and (not (find ?A items)) (quiz-remove-A-buffer))
      (and (not (find ?S items)) (quiz-remove-S-buffer))
      (quiz-mode))
    (switch-to-buffer buffer)
    (set (make-local-variable 'fill-paragraph-function) #'quiz-fill-item)))

(defun quiz-summary (n items)
  (quiz-compile :iso-from (add-iso-day (iso-date) (- 1 n))
		:iso-to (add-iso-day (iso-date))
		:items items))

(defun quiz-summary-print (n)
  (interactive "P")
  (quiz-summary (or n 14) "QA"))

(defun quiz-summary-mail (n)
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

(defun purify-address (address) (string-match* "<?\\([^ >]*@[^ >]*\\)>?" address 1))

(defun quiz-mail-buffer (refresh)
  "Sends current quiz buffer as mail to recipients"
  (interactive "P")
  (if refresh (mb-refresh-addresses))
  (let* ((group quiz-default-mail-group)
	 (to (concat* (second (assoc group quiz-mail-groups)) :in ", " :key #'purify-address))
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

(cl-defmacro quiz-save-excursion (point &rest body)
  "What is the point of this?"
  `(save-excursion
     ,@body))

(cl-defun quiz-question-set-number-1 (n)
  "Sets item numbers of current question to N. Destructive verson of
quiz-question-set-number."
  (error "Under construction!")
  (dolist (item (split-string *quiz-items-short* ""))
    (quiz-beginning-of-item :number item)
    (kill-word 1)
    (insert-string n)))

(cl-defun quiz-question-set-number (n &optional (point (point)))
  "Sets item numbers of question at POINT to N. Assumption: POINT is
at the beginning of the q-question."
  (quiz-save-excursion point
    (quiz-question-set-number-1 n)))

(defun quiz-reset-question-numbers ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((n 1))
      (while (re-search-forward (quiz-regexp-item-start "Q" t) nil t)
	(quiz-question-set-number-1 n)
	(incf n)))))

(cl-defun quiz-summary-edit-question-1 (number &optional (date (midnight)) (path *quiz-dir*))
  "Opens the file with the requested question defined by NUMBER and
DATE. TODO: coordinate this with goto methods, ie. goto-Q etc."
  (interactive "P")
  (find-file (quiz-get-file date path))
  (goto-char (point-min))
  (re-search-forward (format "Q%d: " number)))
;;(quiz-summary-edit-question 2 "2005-02-21")

(cl-defun quiz-summary-edit-question (&optional (path *quiz-dir*))
  "Opens the file with the requested question defined by number and date."
  (interactive)
  (let (n iso-date) 
    (save-excursion
      (end-of-line)
      (re-search-backward (format "[%s]\\([0-9]+\\)" *quiz-items-short*))
      (setq n (string-to-int (match-string 1)))
      (re-search-backward (format "mq-\\(%s\\)" *iso-date*))
      (setq iso-date (match-string 1))
      (quiz-summary-edit-question-1 n iso-date path))))

(require 'mb-utils-buffer)

(defun quiz-convert-aftenposten-nyhetsquiz-question-to-qz-question (beg end n &optional s)
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

(defun quiz-convert-aftenposten-nyhetsquiz-to-qz-file ()
  (interactive)
  (let ((n 0)) 
    (buffer-do-regions beg end ("\\(.*\n.*\n\\)\\s-*\n" 1)
      ;;(message (buffer-substring beg (+ beg 20)))
      (quiz-convert-aftenposten-nyhetsquiz-question-to-qz-question beg end (incf n)))))

(require 'quiz-csv)
(provide 'quiz)

;;For referering til IDLs hjemmesider: http://www.mylder.no/idl/views/640&ref0=291
;;Kirkeinndelinger og rettslige inndelinger: http://www.ssb.no/aarbok/tab/t-000010-001.html
;;http://www.kongehuset.no/ 
