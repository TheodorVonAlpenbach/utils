(defun prefix-p (prefix string)
  "Returns T iff prefix is a prefix of string"
  (awhen (string-match prefix string)
    (zerop it)))
;;(prefix-p "qwe" "qwerty")

(defun in-directory-p (filename directory)
  (prefix-p (expand-file-name directory)
	    (expand-file-name filename)))
;;(in-directory-p "~/bin/mbtags.sh" "~/")

(cl-defun chess-file-p (buffer &optional (chess-dir "~/projects/chess"))
  (or (in-directory-p (buffer-file-name buffer) chess-dir)
      (in-directory-p (buffer-file-name buffer) "~/sources/CHESS/RemoteAcceleration")
      (in-directory-p (buffer-file-name buffer) "~/sources/CHESS/Tail")
      (in-directory-p (buffer-file-name buffer) "~/sources/CHESS/Integrate")
      (in-directory-p (buffer-file-name buffer) "~/sources/CHESS/SimFBGA3Imp")))
;;(chess-file-p (get-buffer "FIRFilter.cpp"))
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
    (setq-local parens-require-spaces nil)
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

(defun qt3-p ()
  (string-match "sources/CHESS" (file-name-directory (buffer-file-name))))
;;(qt3-p)

(defun clean-chess-code ()
  "Note that - should be put at the end in the [] construct"
  (interactive)
  (let ((substitutions
	 `(("\\<TRUE\\>" "true")
	   ("\\<FALSE\\>" "false")
	   ("\\<latin1\\>" "toLatin1().data" 5)
	   ("([[:space:]]+" "(")
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
	   ("\\([])[:alnum:]]\\)[[:space:]]*\\([+]\\)[[:space:]]*\\([[:alnum:]([]\\)"
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
	   ("{[[:space:]]+}" "{}")
	   ("\\_<ConfigKey\\_>" "configKey"))))
    (save-excursion
      (loop for (re s) in (if (qt3-p)
			    ;; remove substitutions tagged with v > 3
			    ;; see for instance the latin1 substitution
			    (remove-if #'(lambda (x)
					   (and (third x) (> (third x) 3)))
			      substitutions)
			    substitutions)
	    for i from 0
	    do (goto-char (point-min))
	    do (while (re-search-forward re nil t)
		 ;; (message "Replace using %dth expression (%s)" i re)
		 (replace-match s t)))
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
      (let ((case-fold-search nil))
	(while (re-search-forward regexp nil t)
	 (replace-match new t t))))))

(cl-defun chess-memberize-init (&optional (prefix "m_") no-query-p)
  "Prepend 'm_' on all identifiers in buffer equal to identifier at point.
If no-query-p is nil, the function will ask for confirmation
before carrying out its actions."
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

(defun qt-align-line (indent)
  (save-excursion
    (let ((a (bol)))
      (when (re-search-forward "=" (line-end-position) t 1)
	(let ((b (point)))
	  (let ((diff (- indent (- b a))))
	    (re-search-backward "[[:space:]]")
	    (if (plusp diff)
	      (insert (make-string diff 32))
	      (delete-backward-char (- diff)))))))))

(cl-defun qt-align-pro-file (&optional (min-space 2))
  "Automatically align all the assigment operators in a .pro file.
Default minimum space before assigment is 2. It can be overruled
by optional argument MIN-SPACE.

Note that the function takes into account that there might always
be operators like '+=' and '-='. Still, the function aligns the
'=' character, so in a .pro file with only '=' assigments, it
might seem like there is a space too much before the assigment
operators.

Consider move this functionality to a makefile-mode extension module"
  (interactive)
  (let ((max-lhs
	 (loop for l in (buffer-lines)
	       for s = (string-match* "^[[:space:]]*\\([[:alpha:]_]*\\)"
			 l :num 1)
	       maximize (length (sstring s)))))
    (save-excursion
      (bob)
      (loop for i below (1- (length (buffer-lines)))
	    do (qt-align-line (+ max-lhs min-space 2))
	    do (forward-line 1)))))

(defun swap-emacs-and-qtcreator-paths (emacs->qtcreator-p start end)
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let ((from (if emacs->qtcreator-p "\\.\\./" "\\.\\./\\.\\./"))
	    (to (if emacs->qtcreator-p "../../" "../")))
	(while (re-search-forward from nil t)
	  (replace-match to))))))

(defun emacs->qtcreator-paths (start end)
  (interactive "r")
  (swap-emacs-and-qtcreator-paths t start end))

(defun qtcreator->emacs-paths (start end)
  (interactive "r")
  (swap-emacs-and-qtcreator-paths nil start end))

(defvar *qmake-program* "/home/mbe/Qt/5.7/gcc_64/bin/qmake")

(defun qmake ()
  (interactive)
  (let ((pro-files (directory-files "." nil "\\.pro$")))
    (if (= (length pro-files) 1)
      (compile (format "%s -r -spec linux-g++ CONFIG+=debug %s"
		 *qmake-program* (first pro-files)))
      (error "Was expecting one and one only .pro file in current directory!"))))

(setf compilation-read-command nil)
(defun chess-compile ()
  (interactive)
  (compilation-start "make -k"))

(defun chess-grep ()
  "Greps in this project and in the whole of chess"
  (interactive)
  (mb-grep-basic :directories "~/projects/chess/lib*/src/" :types "{h,cpp}"))

(defun chess-lookup-qtlog ()
  (interactive)
  (awhen (thing-at-point 'symbol)
    (switch-to-buffer-other-window "qtportlog.org")
    (goto-char (point-min))
    (re-search-forward (substring-no-properties it))))

(let ((qt-map (make-sparse-keymap)))
  (evil-key-chord-define '(normal motion) global-map "gh" qt-map)
  (define-key qt-map "a" #'qt-align-pro-file)
  (define-key qt-map "c" #'chess-compile)
  (define-key qt-map "f" #'chess-lookup-qtlog)
  (define-key qt-map "F" #'find-qt3-brother)
  (define-key qt-map "e" #'chess-goto-error)
  (define-key qt-map "g" #'mb-c++-grep)
  (define-key qt-map "G" #'chess-grep)
  (define-key qt-map "h" #'qt-help)
  (define-key qt-map "i" #'chess-insert-error)
  (define-key qt-map "l" #'qt-latin1)
  (define-key qt-map "m" #'chess-memberize-class-no-query)
  (define-key qt-map "M" #'chess-memberize)
  (define-key qt-map "n" #'next-error)
  (define-key qt-map "q" #'qmake)
  (define-key qt-map "s" #'emacs->qtcreator-paths)
  (define-key qt-map "S" #'qtcreator->emacs-paths)
  (define-key qt-map "v" #'clean-chess-code)) ;memo: vask!

(provide 'qt-chess)
