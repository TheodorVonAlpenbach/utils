;;;; Main differences between Octave and MATLAB can be found here
;;;; https://en.wikibooks.org/wiki/MATLAB_Programming/Differences_between_Octave_and_MATLAB
;;;;
;;;; My additions:
;;;;
;;;; 1. MATLAB is more sensitive with variables that share a name with a function. E.g.
;;;;   [min, imin] = min (varargin{:});
;;;; works in Octave, but not in MATLAB. This does:
;;;;   [mi, imin] = min (varargin{:});

(loop with pdir1 = "~/projects/matlab/ls"
      with pdir2 = "~/git/utils/octave"
      for dir in (mapcar #'sstring '(lscommon lsbin div time file string))
      for p = (list (expand-file-name dir pdir1) (expand-file-name dir pdir2))
      do (push-unique p *version-swaps* #'equal))

(defun o2m-validate-buffer ()
  (string-match "projects/matlab/ls" (buffer-file-name)))
;;(o2m-validate-buffer)

(defun o2m-forward-multiassign ()
  "Find next multiassign line in buffer and return indent."
  (when (re-search-forward "=[^<>;=\n]+=" nil t)
    (back-to-indentation)
    (current-column)))
;;(o2m-forward-multiassign)

(cl-defun o2m-split-multiassign-string (&optional s (indent 2) (trim "[ ;]+"))
  "Convert \"a = b = c ... d = v; to
  d = v
  ...
  b = c;
  a = b;
  ..."
  (let ((line-prefix (blanks indent)))
    (concat* (loop for (l r) in (nreverse (pairs (split-string s "=" t trim)))
		   collect (format "%s = %s;" l r))
      :in "\n"
      :indent-string line-prefix)))
;;(o2m-split-multiassign-string "  a = b = c = v; " 0)

(defun o2m-split-multiassign ()
  "Next: split on =. Reassemble"
  (save-excursion
    (bob)
    (loop for p = (o2m-forward-multiassign)
	  while p
	  do (region-replace-raw
	      (o2m-split-multiassign-string (line-string) p)
	      (line-region)))))
;;(o2m-split-multiassign)

(defconst +o2m-ends-regexp+
  (concat*
      (loop for s in '(function for switch while if unwind_protect_ try_catch_)
	    collect (concat "end" (sstring s)))
    :in "\\|"))

(defun o2m-convert-ends ()
  "Substitue Octave ending keywords with 'end'"
  (assert (o2m-validate-buffer))
  (save-excursion
    (bob)
    (while (re-search-forward +o2m-ends-regexp+ nil t)
      (replace-match "end"))))
;;(o2m-convert-ends)

(defun o2m-convert-defun-names ()
  "Convert _defun_name to defun_name_."
  (assert (o2m-validate-buffer))
  (save-excursion
    (bob)
    (while (re-search-forward "\\_<_\\([_[:word:]]+\\)\\_>" nil t)
      (unless (commented-line-p)
	(replace-match "\\1_")))))
;;(o2m-convert-defun-names)

(defun o2m-convert-NA ()
  "Convert defun_name_ to defun_name_."
  (assert (o2m-validate-buffer))
  (save-excursion
    (bob)
    (while (re-search-forward "\\_<\\(NA\\)\\_>" nil t)
      (replace-match "NaN" t))
    (bob)
    (while (re-search-forward "\\_<\\(isna\\)\\_>" nil t)
      (replace-match "isnan" t))))
;;(o2m-convert-NA)

(defun o2m-convert-empty-curls ()
  "Substitute dubious Octave empty curls, {} with the more explicit {:}.
The function avoids commented lines."
  (assert (o2m-validate-buffer))
  (save-excursion
    (bob)
    (while (re-search-forward "{}" nil t)
      (unless (commented-line-p)
	(replace-match "{:}")))))
;;(o2m-convert-empty-curls)

(defun o2m-comments-buffer ()
  "Convert octave style comments (## / #) to MATLAB style (% / %)"
  (assert (o2m-validate-buffer))
  (save-excursion
    (bob)
    (while (re-search-forward "##" nil t)
      (replace-match "%"))
    (bob)
    (while (re-search-forward "#" nil t)
      (unless (nth 3 (syntax-ppss))
	(replace-match "%")))))
;;(o2m-comments-buffer)

(defun o2m-not-ify-buffer ()
  "Convert octave style not (!) to MATLAB style (~)."
  (assert (o2m-validate-buffer))
  (save-excursion
    (bob)
    (while (re-search-forward "\!" nil t)
      (unless (or (nth 3 (syntax-ppss))
		  (= (char-before (1- (point))) ?%)) 
	(replace-match "~")))))

(defun o2m-convert-colon-string (s prefix)
  (destructuring-bind (l r)
      (split-string s "=" t " +")
    (let ((pos (1+ (string-match ")(:)" r)))
	  (ltmp (format "tmp_%s_" l)))
      (destructuring-bind (rl rr) (split-at-position r pos)
	(format "%s%s = %s;\n%s%s = %s(:);"
	  prefix ltmp rl
	  prefix l ltmp)))))
;;(o2m-convert-colon-string "  foo = barcode (x)(:);" "  ")

(defun o2m-convert-colon ()
  "Split condense colon expression.
E.g. replace
v = foo (x)(:)
with
v_tmp_ = foo (x);
v = v_tmp_(:);"
  (assert (o2m-validate-buffer))
  (save-excursion
    (bob)
    (while (re-search-forward ")(:)" nil t)
      (unless (nth 3 (syntax-ppss)) 
	(back-to-indentation)
	(let ((prefix (blanks (current-column))))
	  (bol)
	  (region-replace-raw
	   (o2m-convert-colon-string (line-string) prefix)
	   (line-region)))))))
;;(o2m-convert-colon)

(defun o2m-sign-outer-region ()
  "Return the outer signature parenthesis region.
It assumes that point is on the signature line in the current
buffer."
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (re-search-forward "(")
    (backward-char 1)
    (sexp-region)))

(defun o2m-sign-inner-region ()
  "As `o2m-sign-outer-region' but without the parenthesis characters."
  (destructuring-bind (a b) (o2m-sign-outer-region)
    (list (1+ a) (1- b))))

(cl-defun o2m-inner-sign-string (&optional (region (o2m-sign-inner-region)))
  "Return the inner signature parenthesis as a string.
It assumes that point is on the signature line in the current
buffer."
  (apply #'buffer-substring-no-properties region))
;;(o2m-inner-sign-string)

(defun o2m-arguments (inner-sign)
  "Return the currrent functions arguments.
The result is a list ((arg1) (arg2) ... (opt1 val1) ... (opt2 val2)),
where argNs, optNs, valNs are all strings."
  (loop for p in (split-string inner-sign "," t " +")
	collect (split-string p "=" t " +")))
;;(o2m-arguments (o2m-inner-sign-string))

(defun o2m-new-default (opt val i)
  (format "  if (nargin < %d)\n    %s = %s;\n  end" (1+ i) opt val))
;;(o2m-new-default "period" "NA" 1)

(defun o2m-convert-arguments (args)
  (list (concat* (mapcar #'car args) :in ", ")
	(aif (loop for (arg val) in args
		   for i from 0
		   if val
		   collect (o2m-new-default arg val i))
	  (concat* it :in "\n" :suf "\n\n")
	  "")))
;;(o2m-convert-arguments '(("prefix") ("period" "NA") ("s" "NA") ("verbose" "false")))

(defun o2m-convert-defaults-1 ()
  "Rewrite function foo (a = initval1, b = initval2) ...
to

function foo (a)
  if (nargsin < 1)
    a = initval1;
  end
  if (nargsin < 2)
    b = initval2;
  end
  ..."
  (destructuring-bind (new-sign new-defaults)
      (o2m-convert-arguments (o2m-arguments (o2m-inner-sign-string)))
    (region-replace-raw new-sign (o2m-sign-inner-region))
    (save-excursion
      (eod)
      (bod)
      (forward-line 1)
      (when new-defaults
	(insert new-defaults)))))

(defun o2m-forward-defun ()
  (when (re-search-forward "^[[:space:]]*function" nil t)
    (bol)))
;;(o2m-forward-defun)

(defun o2m-convert-defaults ()
  "Apply `o2m-convert-defaults' to all defuns in buffer."
  (assert (o2m-validate-buffer))
  (save-excursion
    (bob)
    (while (o2m-forward-defun)
      (o2m-convert-defaults-1)
      (forward-line 1))))
;;(o2m-convert-defaults)

(defun o2m-convert-strings ()
  (assert (o2m-validate-buffer))
  (save-excursion
    (bob)
    (while (re-search-forward "'" nil t)
      (when (and (nth 3 (syntax-ppss))
		 (= (char-after) ?')
		 (= (char-before (1- (point))) ?'))
	(replace-match "''")))
    (bob)
    (while (re-search-forward "\"" nil t)
      (replace-match "'"))))
;;(o2m-convert-strings)

(defun o2m-all ()
  (o2m-convert-defaults)
  (o2m-not-ify-buffer)
  (o2m-comments-buffer)
  (o2m-convert-empty-curls)
  (o2m-convert-NA)
  (o2m-convert-defun-names)
  (o2m-convert-ends)
  (o2m-split-multiassign)
  (o2m-convert-strings)
  (o2m-convert-tests))
;;(o2m-all)

(defun o2m-add-test-defun (code defun file)
  (with-file file
    (bob)
    (unless (re-search-forward "%% Fixures")
      (error "Test file does not have a %%%% Fixures line"))

    (bol)
    (insert (format "function %s (testCase)\n%s\nend\n\n"
	      defun code))))
;;(o2m-add-test-defun "  y = 2 + 2;" "foo" "~/projects/matlab/ls/div/divTest.m")

(defun o2m-test-module-name ()
  (file-name-nondirectory
   (directory-file-name (file-name-directory (buffer-file-name)))))
;;(o2m-test-module-name)

(defun o2m-test-defun-name-obsolete (name)
  "I thought for a moment that 0-9 were not allowed in test function names.
I was wrong."
  (string-replace-map name
    (transpose
     (list (mapcar #'number-to-string (0-n 10))
	   (loop for i below 10
		 collect (capitalize (integer-to-literary-string i)))))))
;;(o2m-test-defun-name-1 "qwe1")

(defun o2m-test-defun-name ()
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
;;(o2m-test-defun-name)

(defun o2m-convert-assert (s)
  ""
  (format "  verifyEqual (testCase, %s"
    (string-match*
	"[[:space:]]*assert[[:space:]]*(\\(.*);?\\)[[:space:]]*$"
      s :num 1)))
;;(o2m-convert-assert "assert (a, b);")

(defun o2m-convert-test-line (s)
  "Assume %! is stripped."
  (case (read s)
    (assert (o2m-convert-assert s))
    (test nil)
    (t (concat "  " (string-trim-left s)))))
;;(o2m-convert-test-line "assert (a, b);")

(defun o2m-convert-test-string (s)
  (concat*
      (loop for l in (string-lines s)
	    for i from 0
	    do (message "%d" i)
	    if (and (not (empty-string-p (string-trim* l "[%! ]*")))
		    (o2m-convert-test-line (string-trim-left* l "[%! ]+")))
	    collect it)
    :in "\n"))
;;(o2m-convert-test-string "%!test\n%! x = randi (100, 10);\n%! assert (a, b);")

(defun o2m-convert-block (s defun-name file)
  (o2m-add-test-defun (o2m-convert-test-string s) defun-name file))
;;(o2m-convert-block "%!test\n%! x = randi (100, 10);\n%! assert (a, b);" "dealnumTest" "~/projects/matlab/ls/div/divTest.m")

(defun o2m-convert-tests ()
  (let ((defun-name (o2m-test-defun-name))
	(file (format "%sTest.m" (o2m-test-module-name))))
    (bob)
    (re-search-forward "^%!")
    (bol)
    (let ((test-blocks (remove-if #'blank-p
			 (split-string*
			  (buffer-substring-no-properties
			   (point) (point-max))
			  "%!\\(test\\||assert\\)"
			  :left))))
      (loop for s in test-blocks 
	    for suffix in (alphanumerate (0-n (length test-blocks)) 1)
	    for fn = (concat defun-name suffix "Test") 
	    do (o2m-convert-block s fn file)))))
;;(o2m-convert-tests)

(defun o2m-convert-doc ()
  (smart-swap)
  (octave-help (octave-main-defun-name))
  (other-window 1)
  (let ((a (point)))
    (re-search-forward "Additional help for built-in functions and")
    (kill-ring-save a (bol))
    (other-window 1)
    (smart-swap)
    (bob)
    (yank)
    (let ((b (point)))
      (re-search-forward "^Function")
      (kill-region b (bol)))
    (let* ((to-end (- (+ 1 (point-max)) (bol))))
      (bob)
      (while (re-search-forward "^" (- (point-max) to-end) t)
	(replace-match "%")))

    (bol)
    (insert "%
%
% Author: Mats Bergstrøm <mbe@lightstructures.no>
% Created: 2017-08-29
")

    (bob)
    (kill-line 2)
    (insert "% Copyright (C) 2019 Light Structures
%
")

    (save-buffer)))
;;(o2m-convert-doc)
