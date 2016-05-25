;; where to put?
(defconst +scilab-identifier-regexp+ "%?[a-zA-Z0-9#_]+")
(defconst +scilab-argument-regexp+ "%?[a-zA-Z0-9#_]+")

(defconst scilab-valid-variable-name "\\<[A-Za-z$#_%][A-Za-z0-9$#_]*\\>"
  "Regexp describing all valid variable names")

(defun scilab-function-call-regexp ()
  "Ok, this is much more complicated than I thought. 
In addition to identifiers, all valid expressions should accepted as well, i.e.
1, [], x + y, (expt(3, 4) + 1)/myfun(123, %T)
This is indeed not feasable, so just check for the presence of parentheses"
  (let* ((lhs (format "\\(\\[[^]]*\\]\\|%s\\)" +scilab-identifier-regexp+))
	 (lhs* (format "\\(%s[[:blank:]]*=[[:blank:]]*\\)?" lhs))
	 (name (format "\\(%s\\)" +scilab-identifier-regexp+))
	 ;;(args (format "\\((\\(.*\\))[[:space:]]*;?[[:blank:]]*$\\)"))) dropping the $ because it fails for comments
	 (args (format "\\((\\(.*\\))[[:space:]]*;?[[:blank:]]*\\)")))
    (format "%s[[:space:].]*%s[[:blank:]]*%s" lhs* name args)))
;;(string-match* (scilab-function-call-regexp) "[a, b] = qwe(c, [], %T, d);" :num '(1 2 3 4 5))
;;(string-match* (scilab-function-call-regexp) "// FFITimeSeries(3, \"20010101_0000\", \"20990101_2359\")" :num '(1 2 3 4 5))


(defun scilab-function-head-regexp (name)
  (format "^[[:space:]]*function[[:space:].]*\\(\\(\\[[^]]*\\]\\|%s\\)[[:blank:]]*=[[:blank:]]*\\)?[[:space:].]*\\(%s\\)"
    +scilab-identifier-regexp+ name))
;;(re-search-forward (scilab-function-head-regexp "mbfct"))

(defun scilab-parse-statement (string)
  "Parses a scilab statement on the form LHS = NAME(ARGS);
and returns the list (LHS NAME ARGS), where
LHS is the left hand side of an assignment (or nil if the statement is not an assigment)
NAME is the name of a function if called
ARGS is a list of the arguments to NAME"
  (destructuring-bind (garbage1 lhs name garbage2 args-string)
      (string-match* (scilab-function-call-regexp) string :num '(1 2 3 4 5))
    (list lhs name (split-string args-string "[ \t\n,]") )))
;;(scilab-parse-statement "[a, b] = qwe(c, [], %T, d);")

(defcustom scilab-highlight-builtin  scilab-font-lock-mode
  "There is no regular expressions for builtins. The names are taken from the
custom `scilab-builtin-list'. So it can work slowly. You can disable this option"
  :group 'scilab
  :group 'scilab-setup
  :type 'boolean)

(defun scilab-which-function()
  "Gets the name of the current function"
  (interactive)
  (let ((name nil))
    (save-excursion
      (scilab-beginning-of-defun)
      (forward-line 0)
      (re-search-forward scilab-function-head-regexp nil t)
      (setq name (match-string-no-properties 3)))
    (message name)))

(defun scilab-beginning-of-defun ()
  "This overrides the scilab.el definition"
  (interactive)
  (end-of-line)
  (if (re-search-backward scilab-defun-regex nil t)
    (match-beginning 0)
    (progn (goto-char (point-min)) nil)))

(defun scilab-end-of-defun ()
  "This overrides the scilab.el definition"
  (interactive)
  (re-search-forward scilab-endfun-regex nil t))

(defun scilab-defun-region ()
  "Return the region of current function"
  (save-excursion
    (let ((beg (scilab-beginning-of-defun))
	  (end (scilab-end-of-defun)))
      (and beg end (list beg end)))))

(defun scilab-defun-lines ()
  "Return the region of current function"
  (string-to-lines (apply #'buffer-substring-no-properties (scilab-defun-region))))

(defun scilab-statement-p (line)
  "Return true iff line is neither blank or a comment"
  (let ((trimmed-line (string-trim line)))
    (nor (empty-string-p trimmed-line) 
	 (string-match "^//" trimmed-line))))
;;(mapcar #'scilab-statement-p '("  // something " "   "))

(defun mbscilab-fn-start (name &optional filename)
  "Returns point of start of function NAME"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "[[:space:]]*function.*%s" name))))

(cl-defun mbscilab-current-function (&optional (point (point)))
  "Returns info about the current function"
  (let (name beg end)
    (save-excursion
      (setf beg (scilab-beginning-of-defun))
      (forward-line 0)
      (re-search-forward scilab-function-head-regexp nil t)
      (setf name (match-string-no-properties 3))
      (setf end (scilab-end-of-defun)))
    (list :name name :region (list beg end))))

(cl-defun scilab-fn-info-proposal ()
  "Returns info about the current function
TODO: make clear if the input argument is buffer POINT or function NAME.
The function should take exclusive keyword args :buffer :point :line :fn-name.
:point, :line and :fn-name are exclusive arguments. If :buffer is
given, then the other args expects to search for result in that
buffer. If not, then for :point and :line the current buffer is
used as value for buffer, where for :fn-name, one could do a
project search, or, again, just provide the current buffer."
  (error "Not implemented"))

(cl-defun scilab-fn-info (&optional (point (point)))
  "Returns info about the function containing POINT."
  (let* ((fn-info (mbscilab-current-function point))
	 (lines (mapcar #'line-number-at-pos (getf fn-info :region)))
	 (current-line (1+ (- (line-number-at-pos point) (first lines)))))
    (nconc fn-info (list :file (buffer-file-name)
			 :buffer (buffer-name)
			 :lines lines
			 :current-line current-line))))
;;(scilab-fn-info)

(cl-defun scilab-fn-info-from-name (function-name)
  "Returns info about the function FN-NAME."
  (with-buffer (first (scilab-function-buffer function-name))
    (scilab-fn-info (point))))
;;(scilab-fn-info-from-name "GenerateFFIAOG")

;;; Equation for next to fns is
;;; global-line-number = relative-line-number + fn-start-line - 1
(defun scilab-relative-linum (linum fn-info)
  (1+ (- linum (first (getf fn-info :lines)))))

(defun scilab-global-linum (relative-linum fn-info)
  (1- (+ relative-linum (first (getf fn-info :lines)))))

(cl-defun scilab-last-line-p (fn-info &optional (point (point)))
  "Returns info about the current function"
  (= (line-number-at-pos point) (second (getf fn-info :lines))))
;;(mbscilab-last-line-p (scilab-fn-info))

(cl-defun scilab-function-at-line (&optional (line (current-line-as-string)))
  (second (scilab-parse-statement line)))

(defun scilab-function-path (function)
  "Returns the pair (PATH-TO-FUNCTION-DEFINITION START-LINE) corresponding to FUNCTION."
  (with-buffer (scilab-tags-buffer)
    (goto-char (point-min))
    (if (re-search-forward (format "\\(%s\\)\\([[:digit:]]+\\)," function) nil t)
      (let ((line (match-string 2)))
	(backward-paragraph 1)
	(re-search-forward "^\\([^,]*\\),*" nil t)
	(list (match-string 1) line)))))
;;(scilab-function-path "mbfct")

(defun scilab-function-buffer (function-name &optional open-buffer-file-p)
  "Returns the pair (BUFFER START-LINE) corresponding to FUNCTION-NAME.
BUFFER is nil then path to FUNCTION-NAME is not in any buffer. If
open-buffer-file-p is true, then the file containing
function-name is opened (if it is not yet opened) and BUFFER is
set to the file's buffer. See also `scilab-function-path'"
  (destructuring-bind (path start-line)
      (scilab-function-path function-name)
    (if path
      (list (find-file-noselect path) start-line)
      (message "scilab-function-buffer: Couldn't find buffer for function '%s'. Consider re-running etags"
	       function-name)
      nil)))
;;(scilab-function-buffer "GenerateFFIAOG")

(defun scilab-function-start-line (function)
  "TODO: Should go back to point if search fails.
Note! This only applies to current buffer."
  (with-buffer (first (scilab-function-buffer function))
    (bob)
    (and (re-search-forward (scilab-function-head-regexp function) nil t)
	 (line-number-at-pos))))
;;(scilab-function-start-line "GenerateFFIAOG")

(defun scilab-goto-function (name &optional function-linum)
  "FUNCTION-LINUM is relative to function [1 num-lines-function]"
  (interactive)
  (awhen (scilab-function-path name)
    (destructuring-bind (path line*) it
      (when path
	(switch-to-buffer it)
	(goto-line (scilab-function-start-line name))
	(when function-linum
	  (forward-line (1- function-linum)))
	(current-buffer)))))
;;(scilab-goto-function "mbfct" 3)

(provide 'mbscilab-syntax)
