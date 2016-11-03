(defun prefix-p (prefix string)
  "Returns T iff prefix is a prefix of string"
  (awhen (string-match prefix string)
    (zerop it)))
;;(prefix-p "qwe" "qwerty")

(defun in-directory-p (filename directory)
  (prefix-p (expand-file-name directory)
	    (expand-file-name filename)))
;;(in-directory-p "~/bin/mbtags.sh" "~/")

(cl-defun chess-file-p (buffer &optional (chess-dir "~/projects/chess/"))
  (in-directory-p (buffer-file-name buffer) chess-dir))
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
    (setf indent-tabs-mode nil)
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
	   ;; ensure spaces around = operator
	   ("\\([^=+*/%&|^<>!-]\\)=\\([^=]\\)" "\\1 = \\2")
	   ;; ensure spaces around ==, =+, =! operators
	   ("\\([=+\\*/%&|^<>!-]=\\)" " \\1 ")
	   ;; ensure spaces around +, -, / (but not *, too difficult at the moment)
	   ("\\([])[:alnum:]]\\)[[:space:]]*\\([-+/]\\)[[:space:]]*\\([[:alnum:]([]\\)"
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
	   ("{[[:space:]]+}" "{}"))))
    (save-excursion
      (loop for (re s) in substitutions
	    for i from 0
	    do (goto-char (point-min))
	    do (while (re-search-forward re nil t)
		 ;; (message "Replace using %dth expression (%s)" i re)
		 (replace-match s)))
      (indent-region (point-min) (point-max)))))

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
    (find-file fn)
    (goto-line (string-to-number linum))))
;;(chess-goto-error)

(defun chess-insert-error ()
  (interactive)
  (bol)
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
  (newline 3))

(defun find-qt3-brother ()
  (interactive)
  (find-file-read-only
   (replace-regexp-in-string "/chess/" "/qt4-chess/" (buffer-file-name))))
;;(find-qt3-brother)

(define-key evil-normal-state-map "ga" 'what-cursor-position)

(let ((qt-map (make-sparse-keymap)))
  (define-key evil-normal-state-map "gh" qt-map)
  (define-key qt-map "g" #'chess-goto-error)
  (define-key qt-map "i" #'chess-insert-error)) 

(provide 'qt-chess)
