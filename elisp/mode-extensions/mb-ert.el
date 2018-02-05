(defconst *mb-ert-file-prefix* "test-")
(defconst *mb-ert-defun-prefix* "test-")

(defun mb-ert-map ()
  "Return a sub map for functions related to ERT."
  (let ((ert-map (make-sparse-keymap)))
    (define-key ert-map "i" #'insert-ert-test-skeleton)
    (define-key ert-map "f" #'mb-ert-test-defun)
    (define-key ert-map "b" #'mb-ert-test-buffer)
    (define-key ert-map "d" #'mb-ert-test-buffer-directory)
    (define-key ert-map "a" #'elisp-test-all)
    ert-map))

(cl-defun mb-ert-test-filename-p (filename)
  "Return non-nil if and only if FILENAME is an ERT module."
  (string-match (format "^%s.*.el$" *mb-ert-file-prefix*)
		(file-name-nondirectory filename)))
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
    (insertf "(require 'ert)\n(require 'lsconf-sensors)\n\n(provide '%s)"
	     (buffer-name)))
  (eob)
  (backward-sexp 1)
  (insertf
   "(ert-deftest %s%s ()\n\"Test of `%s'\"\n (should (equal (%s ) nil)))\n\n"
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
  "Return the filename of ERT test module associated with BUFFER-OR-FILENAME.
If The function assumes that if BUFFER-OR-FILENAME's path is
\"path/filename.el\", then the associated test filename's path is
\"path/test-filename.el\"."
  (let ((filename (or (get-buffer buffer-or-filename) buffer-or-filename)))
    (if (mb-ert-test-filename-p filename)
     filename (mb-ert-swap-filename filename))))
;;(mb-ert-get-test-filename "~/mbe/ert/mymodule.el")

(cl-defun mb-ert-buffer-filename (buffer)
  "Return the path to the BUFFER's test module.
The function assumes that if BUFFER's path is \"path/buffer-name.el\", then
the associated test buffer's path is \"path/test-buffer-name.el\"."
  (mb-ert-get-test-filename (buffer-file-name buffer)))
;;(mb-ert-buffer-filename (get-buffer "test-mb-utils-strings.el"))

(cl-defun mb-ert-swap-file (&optional (filename (buffer-file-name)))
  "Return buffer containing the ERT brother of FILENAME"
  (find-file (mb-ert-swap-filename filename)))
;;(mb-ert-swap-file)

(cl-defun mb-ert-swap-defun-name (&optional
				  (defun-name (sstring (defun-symbol)))
				  to
				  (prefix "test-"))
  "Swap a defun name with the name of its ERT associated test defun, or vice versa.
If TO is :ert then it prepends PREFIX to DEFUN-SYMBOL. If TO
is :non-ert then it removes the length of PREFIX characters from
the left hand side of DEFUN-SYMBOL. For other values of TO, the
function assumes that DEFUN-SYMBOL is the name of a test function
if and only if it has PREFIX has prefix. By default, TO is nil."
  (case to
    (:ert (concat prefix defun-name))
    (:non-ert (substring defun-name (length prefix)))
    (otherwise (let* ((n (length prefix))
		      (to* (if (and (> (length defun-name) n)
				    (string= (substring defun-name 0 n) prefix))
			     :non-ert :ert)))
		 (mb-ert-swap-defun-name defun-name to* prefix)))))

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

(defun mb-ert-defuns (filename)
  "Return a list of all ERT test function symbols in FILENAME."
  (with-temp-buffer
    (insert-file filename)
    (cl-delete-duplicates
     (loop while (mb-ert-forward-test)
	   collect (defun-symbol (point) '(ert-deftest))))))
;;(mb-ert-defuns "~/projects/utils/elisp/utils/test-mb-utils-strings.el")

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

(defun mb-ert-test-defun ()
  "Perform ERT test of defun at point
Does not support testing of defun when point is at its test function."
  (interactive)
  (awhen (defun-symbol)
    (eval/load-buffer/file)
    (ert (mb-eval-defun))))

(cl-defun eval/load-buffer/file (&optional (buffer-or-file (current-buffer)))
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
;;(eval/load-buffer/file "/tmp/eval-load-test")
;;(eval/load-buffer/file (current-buffer))
  
(cl-defun mb-load/eval-ert-pair (&optional (buffer-or-file (current-buffer)))
  ())

(cl-defun mb-ert-test-region (beg end)
  (interactive "r"))

(cl-defun mb-ert-test-buffer (&optional (buffer (current-buffer)))
  "Run all ERT tests associated with defuns in BUFFER."
  (interactive)
  (save-excursion
    (eval-buffer buffer)
    (mb-ert-swap-file (buffer-file-name))
    (eval-buffer)))

(cl-defun mb-ert-test-file (filename)
  (interactive)
  (let ((test-path (mb-ert-get-test-filename buffer)))
    (aif (get-buffer (file-name-nondirectory test-path))
      (eval-buffer it)
      (load test-path))
    (mb-ert (cons 'member (mb-ert-defuns test-path)))))
;;(mb-ert-test-buffer (get-buffer "mb-utils-div.el"))

(cl-defun elisp-test-directory (&optional (buffer (current-buffer)))
  "Invoke ERT on every test function in current directory."
  (interactive)
  (cl-mapc #'load (directory-files (buffer-directory buffer) nil "\.el$"))
  (let ((test-path (mb-ert-buffer-filename buffer)))
    (aif (get-buffer (file-name-nondirectory test-path))
      (eval-buffer it)
      (load test-path))
    (ert (cons 'member (cl-delete-duplicates (mb-ert-defuns test-path))))))
;;(mb-ert-test-buffer (get-buffer "mb-utils-div.el"))

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

(provide 'mb-ert)
