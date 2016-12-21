(defun prefix-p (prefix string)
  "Returns T iff prefix is a prefix of string"
  (awhen (string-match prefix string)
    (zerop it)))
;;(prefix-p "qwe" "qwerty")

(defun in-directory-p (filename directory)
  (prefix-p (expand-file-name directory)
	    (expand-file-name filename)))
;;(in-directory-p "~/bin/mbtags.sh" "~/")

(cl-defun chess-file-p (buffer &optional (chess-dir "~/projects/chess/libHsmsCore"))
  (or (in-directory-p (buffer-file-name buffer) chess-dir)
      (in-directory-p (buffer-file-name buffer) "~/sources/CHESS/Integrate")
      (in-directory-p (buffer-file-name buffer) "~/sources/CHESS/SimFBGA3Imp")))
;;(chess-file-p (get-buffer "ChServerSocket.h"))
;;(chess-file-p (current-buffer))

(c-add-style
 "qt-gnu"
 '("gnu" 
   (c-access-key .
    "\\<\\(signals\\|public\\|protected\\|private\\|public slots\\|protected slots\\|private slots\\):")
   (c-basic-offset . 4)
   (c-cleanup-list
    brace-else-brace
    one-liner-defun)
   (defun-open . 0)))

(defun chess-hook ()
  (when (chess-file-p (current-buffer))
    (setf c-default-style "qt-gnu")
    (setf c-electric-brace t)
    (setf evil-symbol-word-search t)
    (setf indent-tabs-mode nil)
    (setf parens-require-spaces nil)
    (linum-mode 1)))

(add-hook 'c-mode-common-hook #'chess-hook)
;;(nilf c-mode-common-hook)

(defun clean-chess-code-test-string-ops ()
  (append (split-string "=+-*/" "" t)
	  (loop for op2 in '(= + - * / % & | ^ < > !)
		  collect (format "%S=" op2))))
;;(clean-chess-code-test-string-ops)

(defun clean-chess-code-test-string ()
  (loop for op in (clean-chess-code-test-string-ops)
	append (list (format "a%sb" op)
		     (format "a %sb" op)
		     (format "a  %sb" op)
		     (format "a%s b" op)
		     (format "a%s  b" op)
		     (format "a %s b" op)
		     (format "a  %s b" op)
		     (format "a %s  b" op)
		     (format "a %s b" op))))
;;(clean-chess-code-test-string)
;;(insert (concat* (clean-chess-code-test-string) :in "\n"))

(defun clean-chess-code ()
  "Note that - should be put at the end in the [] construct"
  (interactive)
  (let ((substitutions
	 `(("([[:space:]]+" "(")
	   ("[[:space:]]+)" ")")
	   ("\\[[[:space:]]+" "[")
	   ("[[:space:]]+\\]" "]")
	   ("(void)" "()")
	   ("\t" "    ")
	   ;; ensure one space after comma (except at EOL)
	   (",\\([^[:space:]]\\)" ", \\1")
	   ;; ensure spaces around = operator
	   ("\\([^=+*/%&|^<>!-]\\)=\\([^=]\\)" "\\1 = \\2")
	   ;; ensure spaces around ==, =+, =! operators
	   ("\\([=+\\*/%&|^<>!-]=\\)" " \\1 ")
	   ;; ensure spaces around +, / (but not * and -; too difficult at the moment)
	   ("\\([])[:alnum:]]\\)[[:space:]]*\\([+/]\\)[[:space:]]*\\([[:alnum:]([]\\)"
	    "\\1 \\2 \\3")
	   ;; remove double spaces
	   ("[ \t]+" " ")
	   ("{\\([^}[[:space:]\n]\\)" "{ \\1")
	   ("\\([^{[[:space:]\n]\\)}" "\\1 }")
	   ("{\n\n" "{\n")
	   ("\n\n}" "\n}")
	   ("^[[:space:]]*\n" "\n")
	   ("\n\n\n+" "\n\n")
	   ("\\(while\\|if\\)(" "\\1 (")
	   ("){" ") {")
	   (,(format "\\(%s\\)[[:space:]]*:[[:space:]]*\n*"
		     (regexp-opt '("private" "public")))
	     "\\1:\n")
	   ("^[\t]+" " ") ;; remove tab indents; this will be indented properly later
	   ("{[[:space:]]+}" "{}"))))
    (save-excursion
      (loop for (re s) in substitutions
	    for i from 0
	    do (goto-char (point-min))
	    do (while (re-search-forward re nil t)
		 ;; (message "Replace using %dth expression (%s)" i re)
		 (replace-match s)))
      (indent-region (point-min) (point-max)))))
;;(re-search-forward "^[\t]+" nil t)

(pushnew "~/.CTAGS" tags-table-list)

(defun qt-help ()
  (interactive)
  (let ((tag (string-trim (find-tag-default) "'")))
    (browse-url (format "http://doc.qt.io/qt-5/%s.html" (downcase tag)))))

(defun chess-goto-error ()
  (interactive)
  (destructuring-bind (fn linum)
      (string-match* "\\(/.*\\.\\(?:h\\|cpp\\)\\):\\([0-9]*\\): "
	(line-string) :num '(1 2))
    (find-file-other-window fn)
    (goto-line (string-to-number linum))))
;;(chess-goto-error)

(defun chess-insert-error ()
  (interactive)
  (just-one-blank-line 2)
  (insert "** ")
  (evil-paste-before 1)
  (bol)
  (kill-line)
  (bol :offset -2)
  (re-search-forward "error: " nil t)
  (backward-char 1)
  (newline)
  (insert " ")
  (bol :offset 3)
  (delete-blank-lines)
  (newline)
  (org-kill-line))

(defun find-qt3-brother ()
  (interactive)
  (find-file-read-only
   (replace-regexp-in-string
    "/libHsmsCore" "/libChExtra/"
    (replace-regexp-in-string
     "/chess/" "/qt4-chess/"
     (buffer-file-name)))))
;;(find-qt3-brother)

(defun bow ()
  (awhen (bounds-of-thing-at-point 'word)
    (goto-char (car it))))

(defun eow ()
  (awhen (bounds-of-thing-at-point 'word)
    (goto-char (cdr it))))

(defun qt-latin1 ()
  "Append the common toLatin1().data() string to a QString variable."
  (interactive)
  (eow)
  (insert ".toLatin1().data()"))

(defun yes-or-no-p* (string &rest objects)
  "A variant of `yes-or-no-p' with `format' arguments."
  (yes-or-no-p (apply #'format string objects)))

(defun replace-symbols (old new)
  "Replace all symbols OLD with NEW in current buffer"
  (let ((regexp (format "\\_<%s\\_>" old)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(replace-match new t t)))))

(cl-defun chess-memberize-init (&optional (prefix "m_") no-query-p)
  "Prepend 'm_' on all identifiers in buffer equal to identifier at point.
If no-query-p is nil, the function will ask for confirmation
before carrying out its actions."
  (interactive)
  (let* ((old (substring-no-properties (symbol-name (symbol-at-point))))
	 (new (format "%s%s" prefix old)))
    (when (or no-query-p
	      (yes-or-no-p* "Rename variable '%s' to '%s'? " old new))
      (list old new))))

(cl-defun chess-memberize (&optional (prefix "m_") no-query-p)
  "Prepend 'm_' on all identifiers in buffer equal to identifier at point."
  (interactive)
  (apply #'replace-symbols (chess-memberize-init prefix no-query-p)))

(cl-defun chess-memberize-no-query (&optional (prefix "m_"))
  "Prepend 'm_' on all identifiers in buffer equal to identifier at point."
  (interactive)
  (chess-memberize prefix t))

(cl-defun chess-memberize-class (&optional (prefix "m_") no-query-p)
  (interactive)
  (save-excursion
    (destructuring-bind (old new) (chess-memberize-init prefix no-query-p)
      (replace-symbols old new)
      (smart-swap)
      (replace-symbols old new)
      (smart-swap))))

(cl-defun chess-memberize-class-no-query (&optional (prefix "m_"))
  (interactive)
  (chess-memberize-class prefix t))

(define-key evil-normal-state-map "ga" 'what-cursor-position)

(let ((qt-map (make-sparse-keymap)))
  (evil-key-chord-define '(normal motion) global-map "gh" qt-map)
  (define-key qt-map "c" #'clean-chess-code)
  (define-key qt-map "f" #'find-qt3-brother)
  (define-key qt-map "g" #'chess-goto-error)
  (define-key qt-map "h" #'qt-help)
  (define-key qt-map "i" #'chess-insert-error)
  (define-key qt-map "m" #'chess-memberize-class-no-query)
  (define-key qt-map "M" #'chess-memberize)
  (define-key qt-map "l" #'qt-latin1))

(provide 'qt-chess)
