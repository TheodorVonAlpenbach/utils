(defun prefix-p (prefix string)
  "Returns T iff prefix is a prefix of string"
  (awhen (string-match prefix string)
    (zerop it)))
;;(prefix-p "qwe" "qwerty")

(cl-defun chess-file-p (&optional (buffer (current-buffer))
				  (chess-dir "~/git/chess"))
  (or (in-directory-p (buffer-file-name buffer) chess-dir)
      (in-directory-p (buffer-file-name buffer) "~/sources/CHESS/RemoteAcceleration")
      (in-directory-p (buffer-file-name buffer) "~/sources/CHESS/Tail")
      (in-directory-p (buffer-file-name buffer) "~/sources/CHESS/Integrate")
      (in-directory-p (buffer-file-name buffer) "~/sources/CHESS/Differentiate")
      (in-directory-p (buffer-file-name buffer) "~/sources/CHESS/SimFBGA3Imp")
      (in-directory-p (buffer-file-name buffer) "~/sources/CHESS/SimFBGA3")))
;;(chess-file-p (get-buffer "FIRFilter.cpp"))
;;(chess-file-p (current-buffer))

(c-add-style
 "qt-gnu"
 '("gnu" 
   (c-access-key .
    (format "\\<\\(%s\\):"
      (regexp-or "signals" "public" "protected" "private" "public"
		 "slots" "protected slots" "private slots")))
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
    (linum-mode 1)
    (chess-kbd-maps)))

(defun chess-pro-hook ()
  "Set local qt key map for .pro files (Makefile mode)"
  (when (chess-file-p)
    (chess-kbd-maps)))

(add-hook 'c-mode-common-hook #'chess-hook)
(add-hook 'makefile-mode-hook #'chess-hook)
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

(defun qt-eob ()
  "Same as `eob', but ignores the section after #endif declarations."
  (or (re-search-forward "^#endif" nil t)
      (point-max)))

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
	   ;; ensure one space after comment
	   ("//\\([^![:space:]]\\)" "// \\1")
	   ;; ensure one space after doc comment
	   ("// *!\\([^[:space:]]\\)" "//! \\1")
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
	    ;; We must narrow the buffer to avoid cleaning the doc part in .h files
	    do (save-restriction
		 (narrow-to-region (bob) (qt-eob))
		 (bob)
		 (while (re-search-forward re nil t)
		   ;; (message "Replace using %dth expression (%s)" i re)
		   (replace-match s t))))
      ;; Narrow to avoid cleaing doc part
      (indent-region (bob) (qt-eob)))))
;;(re-search-forward "^[\t]+" nil t)

(pushnew "~/.CTAGS" tags-table-list)

(defun qt-help ()
  (interactive)
  (let ((tag (string-trim* (find-tag-default) "'")))
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
      (while (re-search-forward regexp nil t)
	(replace-match new t t)))))

(defun chess-replace-symbols (old new query-p)
  "Replace all symbols OLD with NEW in current buffer"
  (let ((case-fold-search nil))
    (case query-p
      ((nil :never :replacement) (replace-symbols old new))
      (t (save-excursion
	   (goto-char (point-min))
	   (query-replace-regexp old new))))))

(cl-defun chess-memberize-init (&optional query-p (prefix "m_"))
  "Prepend 'm_' on all identifiers in buffer equal to identifier at point.
If query-p is non-nil, the function will ask for confirmation
before carrying out its actions."
  (let* ((old (substring-no-properties (symbol-name (symbol-at-point))))
	 (old-symbol (format "\\_<%s\\_>" old))
	 (new (format "%s%s" prefix old)))
    (when (member query-p '(:all :replacement))
      (let ((res (read-from-minibuffer
		  (format "Replace %s with (default is %s): "
		    old-symbol new))))
	(unless (empty-string-p res)
	  (setf new res))))
    (list old-symbol new)))

(cl-defun chess-memberize (&optional query-p (prefix "m_"))
  "Prepend 'm_' on all identifiers in buffer equal to identifier at point."
  (destructuring-bind (old new) (chess-memberize-init query-p prefix)
    (chess-replace-symbols old new query-p)))

(cl-defun chess-memberize-class (&optional query-p (prefix "m_"))
  (save-excursion
    (destructuring-bind (old new) (chess-memberize-init query-p prefix)
      (chess-replace-symbols old new query-p)
      (smart-swap)
      (chess-replace-symbols old new query-p)
      (smart-swap))))

(defun qt-align-line (indent)
  (save-excursion
    (let ((a (bol)))
      (if (re-search-forward "=" (line-end-position) t 1)
	(let ((b (point)))
	  (let ((diff (- indent (- b a))))
	    (re-search-backward "[[:space:]]")
	    (if (plusp diff)
	      (insert (make-string diff 32))
	      (delete-backward-char (- diff)))))
	(when (not (blank-line-p))
	  (back-to-indentation)
	  (delete-backward-char (- (point) a)) ;; delete existing indent
	  (insert (make-string (1+ indent) 32)))))))

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
  (if (string= (file-name-extension (buffer-file-name)) "pro")
    (let ((max-lhs
	  (loop for l in (buffer-lines)
		for s = (string-match* "^[[:space:]]*\\([[:alpha:]_]*\\)"
			  l :num 1)
		maximize (length (sstring s)))))
     (save-excursion
       (bob)
       (loop for i below (1- (length (buffer-lines)))
	     do (qt-align-line (+ max-lhs min-space 2))
	     do (forward-line 1))))
    (message "Current buffer is not a .pro file")))

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

(defvar *qmake-program* "/home/mbe/Qt/5.11.2/gcc_64/bin/qmake")

(defun qmake ()
  (interactive)
  (let ((pro-files (directory-files "." nil "\\.pro$")))
    (if (= (length pro-files) 1)
      (compile (format "%s -r -spec linux-g++ CONFIG+=debug %s"
		 *qmake-program* (first pro-files)))
      (error "Was expecting one and one only .pro file in current directory!"))))

(setf compilation-read-command nil)

(defun chess-compile (&optional with-clean-p)
  (interactive)
  (when with-clean-p
    (call-process* "make" "clean"))
  (compilation-start "make -k"))

(defun chess-grep ()
  "Greps in this project and in the whole of chess"
  (interactive)
  (mb-grep-basic :directories "~/git/chess/lib*/src/" :types "{h,cpp}"))

(defun chess-lookup-qtlog ()
  (interactive)
  (awhen (thing-at-point 'symbol)
    (switch-to-buffer-other-window "qtportlog.org")
    (goto-char (point-min))
    (re-search-forward (substring-no-properties it))))

(defun qt-for2while ()
  "Convert old-fashion while construct with iterators to a for loop."
  (interactive)
  (let ((var (symbol-at-point)))
    (destructuring-bind (beg . end)
	(bounds-of-thing-at-point 'list)
      (delete-region (1+ beg) (1- end))
      (insert (format "auto it = %S.begin(); it != %S.end(); ++it" var var))
      (up-list -1)
      (backward-kill-word 1)
      (insert "for "))))

(defun qt-used-modules-1 (dir)
  "Return a list of used modules used in project under DIR"
  (cl-remove-duplicates
      (cl-sort (loop for l in (string-lines
			       (string-trim
				(call-process-shell-command*
				 "gfind.sh" "-D" dir "^start_prog" "sh")))
		     collect (second (split-string l)))
	#'string<)
    :test #'string=))
;;(qt-used-modules-1 "/home/mbe/cvs/systems/17_001_Marshal_CandoII")

(defun qt-used-modules ()
  "For all projects, return a list of modules used"
  (mapcar #'qt-used-modules-1 (directory-files "~/cvs/systems" t "^[1][7-9]")))
;;(qt-used-modules)

(defun qt-count-modules ()
  "For all projects, return a list of modules used"
  (loop for p in (partition (flatten (qt-used-modules)) :test #'equal)
	collect (list (first p) (length p))))
;;(qt-count-modules)

(defun qt-module-usage ()
  "Return a string displaying CHESS module usage"
  (flet ((pr (x)
	   (format "%-20s  %S%s" (first x) (second x) (or (third x) ""))))
    (concat* (cl-sort (qt-count-modules) #'> :key #'second)
      :pre (pr '("Module" Projects "\n")) :in "\n" :key #'pr)))
;;(qt-module-usage)

(defun chess-kbd-maps ()
  (let ((qt-map (make-sparse-keymap))
	(make-map (make-sparse-keymap))
	(substitute-map (make-sparse-keymap))
	(find-map (make-sparse-keymap)))
    (key-chord-define evil-normal-state-local-map "gh" qt-map)

    (define-key qt-map "m" make-map)
    (define-key make-map "q" #'qmake)
    (define-key make-map "c" #'chess-compile)
    (define-key make-map "C" (interactivate (chess-compile t)))
    (define-key make-map "n" #'next-error)
    (define-key make-map "p" #'previous-error)

    (define-key qt-map "s" substitute-map)
    (define-key substitute-map "q" #'emacs->qtcreator-paths)
    (define-key substitute-map "Q" #'qtcreator->emacs-paths)
    (define-key substitute-map "l" #'qt-latin1)
    (define-key substitute-map "R" (interactivate (chess-memberize :replacement)))
    (define-key substitute-map "Q" (interactivate (chess-memberize :on-replace)))
    (define-key substitute-map "A" (interactivate (chess-memberize :all)))
    (define-key substitute-map "N" (interactivate (chess-memberize nil)))
    (define-key substitute-map "r" (interactivate (chess-memberize-class :replacement)))
    (define-key substitute-map "q" (interactivate (chess-memberize-class :on-replace)))
    (define-key substitute-map "a" (interactivate (chess-memberize-class :all)))
    (define-key substitute-map "n" (interactivate (chess-memberize-class nil)))

    (define-key qt-map "f" find-map)
    (define-key find-map "f" #'chess-lookup-qtlog)
    (define-key find-map "F" #'find-qt3-brother)
    (define-key find-map "G" #'chess-grep)
    (define-key find-map "e" #'chess-goto-error)
    (define-key find-map "g" #'mb-c++-grep)
    (define-key find-map "h" #'qt-help)

    ;; Miscellanous
    (define-key qt-map "a" #'qt-align-pro-file)
    (define-key qt-map "i" #'chess-insert-error)
    (define-key qt-map "c" #'clean-chess-code)))

(provide 'qt-chess)
