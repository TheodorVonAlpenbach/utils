(defconst *mb-ert-file-prefix* "test-")
(defconst *mb-ert-defun-prefix* "test-")

(defun mb-ert-map ()
  "Return a sub map for functions related to ERT."
  (let ((ert-map (make-sparse-keymap)))
    (define-key ert-map "i" #'insert-ert-test-skeleton)
    (define-key ert-map "f" #'mb-ert-test-defun)
    (define-key ert-map "b" #'mb-ert-test-buffer)
    (define-key ert-map "d" #'mb-ert-test-buffer-directory)
    (define-key ert-map "e" #'mb-ert-test-mb-elisp)
    (define-key ert-map "a" #'elisp-test-all)
    ert-map))

(cl-defun mb-ert-test-filename-p (filename)
  "Return non-nil if and only if FILENAME is an ERT module."
  (and
   ;; a (temporary? exception)
   (not (string-match "mb-js-mode" filename))
   (string-match (format "^%s.*.el$" *mb-ert-file-prefix*)
		 (file-name-nondirectory filename))))
;;(mb-ert-test-filename-p "qwe/test-mb-utils-div.el")

(cl-defun mb-ert-test-buffer-p (&optional buffer (current-buffer))
  "Return non-nil if and only if BUFFER contains an ERT test module.
BUFFER is optional with the current buffer as default value."
  (mb-ert-test-filename-p (buffer-name buffer)))
;;(mb-ert-test-buffer-p (get-buffer "test-mb-utils-div.el"))

(defun mb-ert-insert-skeleton (defun-name)
  "Add test for the function DEFUN-NAME to the ERT test module.
If such module does not exist, create one."
  (when (empty-buffer-p)
    (insertf "(require 'ert)\n(require '%s)\n\n(provide '%s)"
	     (file-name-sans-extension
	      (file-name-nondirectory (mb-ert-swap-filename (buffer-name))))
	     (file-name-sans-extension (buffer-name))))
  (eob)
  (backward-sexp 1)
  (insertf
   "(ert-deftest %s%s ()\n  \"Test of `%s'\"\n (should (equal (%s ) nil)))\n\n"
   *mb-ert-defun-prefix* defun-name defun-name defun-name)
  (backward-char 10))

;;; Swapping
(defun mb-ert-swap-filename (filename)
  "Swap from module filename to ERT test module filename or vice-versa."
  (destructuring-bind (test-prefix name)
      (string-match* (format "\\(%s\\)?\\(.*\\.el$\\)" *mb-ert-file-prefix*)
	(file-name-nondirectory filename) :num '(1 2))
    (expand-file-name (aif test-prefix name (concat *mb-ert-file-prefix* name))
		      (file-name-directory filename))))

;;(mb-ert-swap-filename "/dir/test-ert.el")

(cl-defun mb-ert-get-test-filename (buffer-or-filename)
  "Return the filename of the ERT test module associated with BUFFER-OR-FILENAME.
The function assumes that if BUFFER-OR-FILENAME's path is
\"path/filename.el\", then the associated test filename's path is
\"path/test-filename.el\"."
  (let ((filename (aif (get-buffer buffer-or-filename)
		      (buffer-file-name it)
		      buffer-or-filename)))
    (if (mb-ert-test-filename-p filename)
     filename (mb-ert-swap-filename filename))))
;;(mb-ert-get-test-filename "~/mbe/ert/test-mymodule.el")

(cl-defun mb-ert-get-module-filename (buffer-or-filename)
  "Return the filename of the non-test module associated with BUFFER-OR-FILENAME.
The function assumes that if BUFFER-OR-FILENAME's path is
\"path/test-filename.el\", then the associated module filename's
path is \"path/filename.el\". Else, it simply returns the
filename of BUFFER-OR-FILENAME."
  (mb-ert-swap-filename (mb-ert-get-test-filename buffer-or-filename)))
;;(mb-ert-get-module-filename "test-mb-ert.el")

(cl-defun mb-ert-swap-file (&optional (filename (buffer-file-name)))
  "Return buffer containing the ERT brother of FILENAME"
  (find-file (mb-ert-swap-filename filename)))
;;(mb-ert-swap-file)

(cl-defun mb-ert-name-p (name-or-symbol &optional (prefix "test-"))
  (string-match* (concat "^" prefix) (sstring name-or-symbol)))

(cl-defun mb-ert-swap-defun-name (&optional
				  (defun-name (sstring (defun-symbol)))
				  to
				  (prefix "test-"))
  "Swap a defun name with the name of its ERT associated test defun, or vice versa.
If TO is :ert then it prepends PREFIX to DEFUN-SYMBOL. If TO
is :non-ert then it removes the length of PREFIX characters from
the left hand side of DEFUN-SYMBOL. For other values of TO, the
function assumes that DEFUN-SYMBOL is the name of a test function
if and only if it has PREFIX has prefix. By default, TO is nil.
\n(fn &optional SYMBOL TO PREFIX)"
  (case to
    (:ert (concat prefix defun-name))
    (:ert-soft (if (mb-ert-name-p defun-name)
		 defun-name (concat prefix defun-name)))
    (:non-ert (substring defun-name (length prefix)))
    (otherwise (let* ((n (length prefix))
		      (to* (if (and (> (length defun-name) n)
				    (string= (substring defun-name 0 n) prefix))
			     :non-ert :ert)))
		 (mb-ert-swap-defun-name defun-name to* prefix)))))
;;(mb-ert-swap-defun-name)

(cl-defun mb-ert-swap-defun-symbol (&optional (symbol (defun-symbol)) &rest args)
  "Swap a defun SYMBOL with the symbol of its ERT associated test defun, or vice versa.
For more swap options, use `mb-ert-swap-defun-name'.
\n(fn &optional SYMBOL TO PREFIX)"
  (intern (apply #'mb-ert-swap-defun-name (sstring symbol) args)))
;;(mb-ert-swap-defun-symbol 'mb-ert-swap-defun-symbol)

(cl-defun mb-swap-ert-defun (&optional (symbol-or-name (defun-symbol)))
  "Swap an emacs lisp file with its associated test file."
  (let ((defun-name (sstring symbol-or-name)))
    (mb-ert-swap-file (buffer-file-name))
    (when defun-name
      (if (mb-ert-test-buffer-p)
	(or (elisp-goto-defun (mb-ert-swap-defun-name defun-name :ert))
	    (mb-ert-insert-skeleton defun-name))
	(elisp-goto-defun (mb-ert-swap-defun-name defun-name :non-ert))))))
;;(mb-swap-ert-defun)

(defun mb-ert-forward-test ()
  "Move point to next ERT test in buffer.
If no such test is found, it returns NIL."
  (re-search-forward "^[[:space:]]*([[:space:]]*ert-deftest[[:space:]]*" nil t))

(defun mb-ert-file-defuns (test-filename)
  "Return a list of all ERT test function symbols in FILENAME."
  (unless (mb-ert-test-filename-p test-filename)
    (error "Argument is not a path to an ERT test file."))
  (with-temp-buffer
    (insert-file test-filename)
    (cl-delete-duplicates
     (loop while (mb-ert-forward-test)
	   collect (defun-symbol (point) '(ert-deftest))))))
;;(mb-ert-file-defuns "~/projects/utils/elisp/mode-extensions/test-mb-texinfo.el")

(defun mb-ert-directory-defuns (directory)
  "Return a list of all ERT test function symbols in DIRECTORY."
  (loop for f in (directory-files directory t "\.el$")
	if (mb-ert-test-filename-p f)
	append (mb-ert-file-defuns f)))
;;(mb-ert-directory-defuns "~/projects/utils/elisp/utils")

(cl-defun mb-non-ert-files (&optional (directory "."))
  "Return a list of elisp files in DIRECTORY that are not test files."
  (cl-remove "^test-" (directory-files directory nil "\.el$")
	     :test #'string-match))
;;(mb-non-ert-files ".")

(cl-defun mb-ert-files (&optional (directory ".") verify-p)
  "Return a list of every ERT test file in DIRECTORY.
If VERIFY-P is not nil, then the function checks that each test
file corresponds to an actual elisp file in DIRECTORY."
  (let ((res (directory-files directory nil "^test-.*\.el$")))
    (awhen (and verify-p
		(cl-set-difference
		 (loop for x in res collect (subseq x 5))
		 (mb-non-ert-files directory)
		 :test #'string=))
      (warn "The following test files do not have matching elisp files:\n%s"
	    (concat* it :in "\n" :key #'(lambda (x) (concat "test-" x)))))
    res))
;;(mb-ert-files "." t)

(defun mb-test-ert-symbols (symbols)
  (ert (cons 'member (listify symbols))))

(cl-defun mb-eval/load (&optional (buffer-or-file (current-buffer)))
  "Evalute or load BUFFER-OR-FILE.
If BUFFER-OR-FILE is a designator for an existing buffer, that
is, it is either a buffer object or the name of an existing
buffer, then the function will evaluate that buffer with
`eval-buffer'. Else, if buffer-or-file is a path to an existing
file, this file will be loaded, but not inserted to a new buffer
in Emacs."
  (aif (get-buffer buffer-or-file)
    (eval-buffer it)
    (load buffer-or-file)))
;;(mb-eval/load "/tmp/eval-load-test")
;;(mb-eval/load (current-buffer))
  
(cl-defun mb-eval/load-ert-pair (&optional (buffer-or-file (current-buffer)))
  "Loads or evals BUFFER-OR-FILE and its ERT testing counterpart, if it exists"
  (let ((module-name (mb-ert-get-module-filename buffer-or-file))
	(test-name (mb-ert-get-test-filename buffer-or-file)))
    (when (file-exists-p module-name)
      (mb-eval/load module-name))
    (when (file-exists-p test-name)
      (mb-eval/load test-name))))
;;(mb-eval/load-ert-pair (current-buffer))

;;;; Test functions
(cl-defun mb-ert-test-symbols (symbols)
  "Test all test functions designated by a symbol in SYMBOLS.
This function does not evaluate neither the function nor its
associated function."
  (message "%S" (cons 'member symbols))
  (ert (cons 'member symbols))
    ;; just skip output from ert
    t)

(defun mb-ert-test-defun ()
  "Perform ERT test of defun at point
Does not support testing of defun when point is at its test function."
  (interactive)
  (awhen (defun-symbol)
    ;;(mb-eval/load-ert-pair)
    (mb-ert-test-symbols (list (mb-ert-swap-defun-symbol it :ert-soft)))))

(cl-defun mb-ert-test-region (beg end)
  (interactive "r")
  (error "Not implemented"))

(cl-defun mb-ert-test-file (filename)
  "Test all functions in FILENAME.
FILENAME must be an absolute path or relative to current
directory. It can be either a test file, or the module associated
with a test file. In the latter case, every function in the
module with a corresponding ert test function will be included in
the test."
  (interactive)
  (mb-eval/load-ert-pair filename)
  (mb-ert-test-symbols
   (mb-ert-file-defuns (mb-ert-get-test-filename filename))))
;;(mb-ert-test-file "../utils/test-mb-utils-div.el")

(cl-defun mb-ert-test-buffer (&optional (buffer (current-buffer)))
  "Run all ERT tests associated with defuns in BUFFER."
  (interactive)
  (mb-ert-test-file (buffer-file-name buffer)))
;;(mb-ert-test-buffer (get-buffer "mb-utils-div.el"))

(cl-defun mb-ert-test-buffer-directory (&optional (buffer (current-buffer)))
  "Invoke ERT on every test function in current directory."
  (interactive)
  (let ((fs (copy-if #'mb-ert-test-filename-p
	      (directory-files (buffer-directory buffer) t "\.el$"))))
    (mapc #'mb-eval/load-ert-pair fs)
    (mb-ert-test-symbols (mapcan #'mb-ert-file-defuns fs))))

(cl-defun mb-ert-test-buffer-directories (dirs)
  "Invoke ERT on every test function in current directory."
  (mb-ert-test-symbols
   (loop for d in dirs
	 for fs = (copy-if #'mb-ert-test-filename-p
		    (directory-files d t "\.el$"))
	 do (mapc #'mb-eval/load-ert-pair fs)
	 append (mapcan #'mb-ert-file-defuns fs))))

(cl-defun mb-ert-test-mb-elisp (&optional (buffer (current-buffer)))
  "Invoke ERT on every test function in all of MB's elisp code."
  (interactive)
  (mb-ert-test-buffer-directories
   (subdirs (expand-file-name +mb-lisp-dir+))))

(cl-defun elisp-test-all (&optional (buffer (current-buffer)))
  "Run `ert' on all mb-elisp tests that exists.
Currently it only tests the test functions that have been
evaluated or compiled.

Also, there seems to be a problem with how
ERT handles environments. My let forms seem to be kept in a
separate environment, which causes side effects to be kept until
the time the test is run again. Because of this, you really need
to evaluate the test function before each test in order to reset
the bindings. I believe this is a bug.

I guess the best implementation of this function is to actually
locate every test fuction that there is and re-evaluate each of
them before calling (ert t)."
  (interactive)
  ;; TODO load all test buffers here
  (ert t))
;;(elisp-test-all (get-buffer "mb-utils-div.el"))

;;;; correction of ert
(defun mb-ert--insert-human-readable-selector (selector)
  "Insert a human-readable presentation of SELECTOR into the current buffer."
  ;; This is needed to avoid printing the (huge) contents of the
  ;; `backtrace' slot of the result objects in the
  ;; `most-recent-result' slots of test case objects in (eql ...) or
  ;; (member ...) selectors.
  (cl-labels ((rec (selector)
		;; This code needs to match the etypecase in `ert-select-tests'.
		(etypecase selector
		  ((or (member nil t
			       :new :failed :passed
			       :expected :unexpected)
		       string
		       symbol)
		   selector)
		  (ert-test
		   (if (ert-test-name selector)
		       (make-symbol (format "<%S>" (ert-test-name selector)))
		     (make-symbol "<unnamed test>")))
		  (cons
		   (destructuring-bind (operator &rest operands) selector
		     (ecase operator
		       ((member eql and not or)
			(cons operator (mapcar #'rec operands)))
		       ((member tag satisfies)
			selector)))))))
    (insert (format "%S" (rec selector)))))

(advice-add #'ert--insert-human-readable-selector
	    :override #'mb-ert--insert-human-readable-selector)

(defun ert-string-to-file (string file)
  "Same as `string-to-file', but indended for use with ERT.
The reason is that `string-to-file' calls `write-file' which
issues an `args-out-of-range' error, which is catched by ERT, and
causes the test to fail. This function, probably slower than
`string-to-file', avoids this by using `with-temp-file'
instead."
  (with-temp-file file
    (overwrite-region string (point-min) (point-max))))

(provide 'mb-ert)
