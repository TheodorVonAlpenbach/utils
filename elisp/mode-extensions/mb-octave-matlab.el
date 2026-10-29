;;; mb-octave-matlab.el --- Convert Octave source to MATLAB -*- lexical-binding: t; -*-

;;; Commentary:

;;;; Main differences between Octave and MATLAB can be found here
;;;; https://en.wikibooks.org/wiki/MATLAB_Programming/Differences_between_Octave_and_MATLAB
;;;;
;;;; My additions:
;;;;
;;;; 1. MATLAB is more sensitive with variables that share a name with a function. E.g.
;;;;   [min, imin] = min (varargin{:});
;;;; works in Octave, but not in MATLAB. This does:
;;;;   [mi, imin] = min (varargin{:});
;;;; 2. MATLAB does not automatically convert into logicals, like Octave. E.g.
;;;;   ifelse (diff (t), 2, 1)
;;;; works in Octave, not in MATLAB. But this works in both:
;;;;   ifelse (cl-find (diff (t)), 2, 1)

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function octave-help "mb-octave")
(declare-function octave-main-defun-name "mb-octave")
(declare-function smart-swap "smart-swap")

(defgroup mb-octave-matlab nil
  "Conversion helpers for related Octave and MATLAB trees."
  :group 'octave)

(defcustom o2m-octave-root "~/git/utils/octave"
  "Root directory of the Octave source tree."
  :type 'directory
  :group 'mb-octave-matlab)

(defcustom o2m-matlab-root "~/git/utils/matlab"
  "Root directory of the MATLAB source tree."
  :type 'directory
  :group 'mb-octave-matlab)

(defvar *version-swaps* nil)

(defun o2m-register-version-swaps ()
  "Register corresponding Octave and MATLAB directories.
Return the updated `*version-swaps*' value."
  (dolist (directory
           '("lscommon" "lsbin" "div" "time" "file" "string"
             "timeseries" "math"))
    (cl-pushnew
     (list (expand-file-name directory o2m-octave-root)
           (expand-file-name directory o2m-matlab-root))
     *version-swaps* :test #'equal))
  *version-swaps*)

(defun o2m--line-string ()
  "Return the current line without text properties."
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

(defun o2m--replace-region (replacement beginning end)
  "Replace text from BEGINNING to END with REPLACEMENT."
  (delete-region beginning end)
  (goto-char beginning)
  (insert replacement))

(defun o2m--commented-p ()
  "Return non-nil when point is in an Octave or MATLAB comment."
  (or (nth 4 (syntax-ppss))
      (save-excursion
        (beginning-of-line)
        (looking-at-p "[[:space:]]*[%#]"))))

(defun o2m-validate-buffer ()
  "Return non-nil when the current file is below `o2m-matlab-root'."
  (and buffer-file-name
       (file-in-directory-p buffer-file-name
                            (expand-file-name o2m-matlab-root))))
;;(o2m-validate-buffer)

(cl-defun o2m-forward-multiassign ()
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
  (let* ((line-prefix (make-string indent ?\s))
         (parts (split-string s "=" t trim))
         (assignments
          (cl-loop for (left right) on parts
                   while right
                   collect (format "%s = %s;" left right))))
    (mapconcat (lambda (assignment)
                 (concat line-prefix assignment))
               (nreverse assignments) "\n")))
;;(o2m-split-multiassign-string "  a = b = c = v; " 0)

(cl-defun o2m-split-multiassign ()
  "Next: split on =. Reassemble"
  (save-excursion
    (goto-char (point-min))
    (cl-loop for p = (o2m-forward-multiassign)
	  while p
	  do (o2m--replace-region
	      (o2m-split-multiassign-string (o2m--line-string) p)
              (line-beginning-position) (line-end-position)))))
;;(o2m-split-multiassign)

(defconst +o2m-ends-regexp+
  (regexp-opt
   (mapcar
    (lambda (name) (concat "end" (symbol-name name)))
    '(function for switch while if unwind_protect_ try_catch_))
   'symbols))

(cl-defun o2m-convert-ends ()
  "Substitute Octave ending keywords with `end'."
  (cl-assert (o2m-validate-buffer))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward +o2m-ends-regexp+ nil t)
      (replace-match "end"))))
;;(o2m-convert-ends)

(cl-defun o2m-convert-defun-names ()
  "Convert _defun_name to defun_name_."
  (cl-assert (o2m-validate-buffer))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\_<_\\([_[:word:]]+\\)\\_>" nil t)
      (unless (o2m--commented-p)
	(replace-match "\\1_")))))
;;(o2m-convert-defun-names)

(cl-defun o2m-convert-NA ()
  "Convert defun_name_ to defun_name_."
  (cl-assert (o2m-validate-buffer))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\_<\\(NA\\)\\_>" nil t)
      (replace-match "NaN" t))
    (goto-char (point-min))
    (while (re-search-forward "\\_<\\(isna\\)\\_>" nil t)
      (replace-match "isnan" t))))
;;(o2m-convert-NA)

(cl-defun o2m-convert-empty-curls ()
  "Substitute dubious Octave empty curls, {} with the more explicit {:}.
The function avoids commented lines."
  (cl-assert (o2m-validate-buffer))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "{}" nil t)
      (unless (o2m--commented-p)
	(replace-match "{:}")))))
;;(o2m-convert-empty-curls)

(cl-defun o2m-comments-buffer ()
  "Convert octave style comments (## / #) to MATLAB style (% / %)"
  (cl-assert (o2m-validate-buffer))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "##" nil t)
      (replace-match "%"))
    (goto-char (point-min))
    (while (re-search-forward "#" nil t)
      (unless (nth 3 (syntax-ppss))
	(replace-match "%")))))
;;(o2m-comments-buffer)

(cl-defun o2m-not-ify-buffer ()
  "Convert octave style not (!) to MATLAB style (~)."
  (cl-assert (o2m-validate-buffer))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\!" nil t)
      (unless (or (nth 3 (syntax-ppss))
		  (= (char-before (1- (point))) ?%)) 
	(replace-match "~")))))

(cl-defun o2m-convert-colon-string (s prefix)
  (cl-destructuring-bind (l r)
      (split-string s "=" t " +")
    (let ((position (string-match ")(:)" r))
          (temporary (format "tmp_%s_" l)))
      (unless position
        (error "No condensed colon expression in: %s" s))
      (format "%s%s = %s;\n%s%s = %s(:);"
              prefix temporary (substring r 0 (1+ position))
              prefix l temporary))))
;;(o2m-convert-colon-string "  foo = barcode (x)(:);" "  ")

(cl-defun o2m-convert-colon ()
  "Split condense colon expression.
E.g. replace
v = foo (x)(:)
with
v_tmp_ = foo (x);
v = v_tmp_(:);"
  (cl-assert (o2m-validate-buffer))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ")(:)" nil t)
      (unless (nth 3 (syntax-ppss)) 
	(back-to-indentation)
	(let ((prefix (make-string (current-column) ?\s)))
          (beginning-of-line)
	  (o2m--replace-region
	   (o2m-convert-colon-string (o2m--line-string) prefix)
           (line-beginning-position) (line-end-position)))))))
;;(o2m-convert-colon)

(cl-defun o2m-sign-outer-region ()
  "Return the outer signature parenthesis region.
It assumes that point is on the signature line in the current
buffer."
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (re-search-forward "(")
    (backward-char 1)
    (list (point) (scan-sexps (point) 1))))

(cl-defun o2m-sign-inner-region ()
  "As `o2m-sign-outer-region' but without the parenthesis characters."
  (cl-destructuring-bind (a b) (o2m-sign-outer-region)
    (list (1+ a) (1- b))))

(cl-defun o2m-inner-sign-string (&optional (region (o2m-sign-inner-region)))
  "Return the inner signature parenthesis as a string.
It assumes that point is on the signature line in the current
buffer."
  (apply #'buffer-substring-no-properties region))
;;(o2m-inner-sign-string)

(cl-defun o2m-arguments (inner-sign)
  "Return the currrent functions arguments.
The result is a list ((arg1) (arg2) ... (opt1 val1) ... (opt2 val2)),
where argNs, optNs, valNs are all strings."
  (cl-loop for p in (split-string inner-sign "," t " +")
	collect (split-string p "=" t " +")))
;;(o2m-arguments (o2m-inner-sign-string))

(cl-defun o2m-new-default (opt val i)
  (format "  if (nargin < %d)\n    %s = %s;\n  end" (1+ i) opt val))
;;(o2m-new-default "period" "NA" 1)

(cl-defun o2m-convert-arguments (args)
  (let ((defaults
         (cl-loop for (arg val) in args
                  for i from 0
                  when val
                  collect (o2m-new-default arg val i))))
    (list (string-join (mapcar #'car args) ", ")
          (if defaults
              (concat (string-join defaults "\n") "\n\n")
            ""))))
;;(o2m-convert-arguments '(("prefix") ("period" "NA") ("s" "NA") ("verbose" "false")))

(cl-defun o2m-convert-defaults-1 ()
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
  (cl-destructuring-bind (new-sign new-defaults)
      (o2m-convert-arguments (o2m-arguments (o2m-inner-sign-string)))
    (apply #'o2m--replace-region new-sign (o2m-sign-inner-region))
    (save-excursion
      (end-of-defun)
      (beginning-of-defun)
      (forward-line 1)
      (when new-defaults
	(insert new-defaults)))))

(cl-defun o2m-forward-defun ()
  (when (re-search-forward "^[[:space:]]*function" nil t)
    (beginning-of-line)))
;;(o2m-forward-defun)

(cl-defun o2m-convert-defaults ()
  "Apply `o2m-convert-defaults' to all defuns in buffer."
  (cl-assert (o2m-validate-buffer))
  (save-excursion
    (goto-char (point-min))
    (while (o2m-forward-defun)
      (o2m-convert-defaults-1)
      (forward-line 1))))
;;(o2m-convert-defaults)

(cl-defun o2m-convert-strings ()
  (cl-assert (o2m-validate-buffer))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "'" nil t)
      (when (and (nth 3 (syntax-ppss))
		 (= (char-after) ?')
		 (= (char-before (1- (point))) ?'))
	(replace-match "''")))
    (goto-char (point-min))
    (while (re-search-forward "\"" nil t)
      (replace-match "'"))))
;;(o2m-convert-strings)

(cl-defun o2m-add-test-defun (code defun file)
  (with-temp-file file
    (insert-file-contents file)
    (goto-char (point-min))
    (unless (re-search-forward "%% Fixures")
      (error "Test file does not have a %%%% Fixures line"))

    (beginning-of-line)
    (insert (format "function %s (testCase)\n%s\nend\n\n"
	      defun code))))
;;(o2m-add-test-defun "  y = 2 + 2;" "foo" "~/projects/matlab/ls/div/divTest.m")

(cl-defun o2m-test-module-name ()
  (file-name-nondirectory
   (directory-file-name (file-name-directory (buffer-file-name)))))
;;(o2m-test-module-name)

(cl-defun o2m-test-defun-name-obsolete (name)
  "I thought for a moment that 0-9 were not allowed in test function names.
I was wrong."
  (let ((words
         ["Zero" "One" "Two" "Three" "Four"
          "Five" "Six" "Seven" "Eight" "Nine"]))
    (replace-regexp-in-string
     "[0-9]"
     (lambda (digit)
       (aref words (string-to-number digit)))
     name t t)))
;;(o2m-test-defun-name-1 "qwe1")

(cl-defun o2m-test-defun-name ()
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
;;(o2m-test-defun-name)

(cl-defun o2m-convert-assert (s)
  "Convert the Octave assert statement S to MATLAB verification syntax."
  (unless
      (string-match
       "[[:space:]]*assert[[:space:]]*(\\(.*);?\\)[[:space:]]*$" s)
    (error "Invalid Octave assert statement: %s" s))
  (format "  verifyEqual (testCase, %s" (match-string 1 s)))
;;(o2m-convert-assert "assert (a, b);")

(cl-defun o2m-convert-test-line (s)
  "Assume %! is stripped."
  (let* ((form (car (read-from-string s)))
         (head (if (consp form) (car form) form)))
    (pcase head
      ('assert (o2m-convert-assert s))
      ('test nil)
      (_ (concat "  " (string-trim-left s))))))
;;(o2m-convert-test-line "assert (a, b);")

(cl-defun o2m-convert-test-string (s)
  (string-join
   (cl-loop for line in (string-lines s)
            for stripped = (string-trim-left line "[%! ]+")
            for converted =
            (unless (string-empty-p
                     (string-trim line "[%! ]*" "[%! ]*"))
              (o2m-convert-test-line stripped))
            when converted collect converted)
   "\n"))
;;(o2m-convert-test-string "%!test\n%! x = randi (100, 10);\n%! assert (a, b);")

(cl-defun o2m-convert-block (s defun-name file)
  (o2m-add-test-defun (o2m-convert-test-string s) defun-name file))
;;(o2m-convert-block "%!test\n%! x = randi (100, 10);\n%! assert (a, b);" "dealnumTest" "~/projects/matlab/ls/div/divTest.m")

(defun o2m--test-blocks (string)
  "Return the `%!test' and `%!assert' blocks in STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let (starts blocks)
      (while (re-search-forward "^%!\\(?:test\\|assert\\)" nil t)
        (push (match-beginning 0) starts))
      (setq starts (nreverse starts))
      (cl-loop for start in starts
               for end in (append (cdr starts) (list (point-max)))
               for block =
               (string-trim
                (buffer-substring-no-properties start end))
               unless (string-empty-p block)
               do (push block blocks))
      (nreverse blocks))))

(cl-defun o2m-convert-tests ()
  (let ((defun-name (o2m-test-defun-name))
	(file (format "%sTest.m" (o2m-test-module-name))))
    (goto-char (point-min))
    (re-search-forward "^%!")
    (beginning-of-line)
    (let ((test-blocks
           (o2m--test-blocks
            (buffer-substring-no-properties (point) (point-max)))))
      (cl-loop for s in test-blocks 
	    for index from 0
            for suffix = (format "%c" (+ ?A index))
	    for fn = (concat defun-name suffix "Test") 
	    do (o2m-convert-block s fn file)))))
;;(o2m-convert-tests)

(cl-defun o2m-convert-doc ()
  (smart-swap)
  (octave-help (octave-main-defun-name))
  (other-window 1)
  (let ((a (point)))
    (re-search-forward "Additional help for built-in functions and")
    (beginning-of-line)
    (kill-ring-save a (point))
    (other-window 1)
    (smart-swap)
    (goto-char (point-min))
    (yank)
    (let ((b (point)))
      (re-search-forward "^Function")
      (beginning-of-line)
      (kill-region b (point)))
    (beginning-of-line)
    (let* ((to-end (- (1+ (point-max)) (point))))
      (goto-char (point-min))
      (while (re-search-forward "^" (- (point-max) to-end) t)
	(replace-match "%")))

    (beginning-of-line)
    (insert "%
%
% Author: Mats Bergstrøm <mbe@lightstructures.no>
% Created: 2017-08-29
")

    (goto-char (point-min))
    (kill-line 2)
    (insert "% Copyright (C) 2019 Light Structures
%
")

    (save-buffer)))
;;(o2m-convert-doc)

(cl-defun o2m-all ()
  (o2m-convert-defaults)
  (o2m-not-ify-buffer)
  (o2m-comments-buffer)
  (o2m-convert-empty-curls)
  (o2m-convert-NA)
  (o2m-convert-defun-names)
  (o2m-convert-ends)
  (o2m-split-multiassign)
  (o2m-convert-strings)
  (o2m-convert-tests)
  (o2m-convert-doc))
;;(o2m-all)

(provide 'mb-octave-matlab)

;;; mb-octave-matlab.el ends here
