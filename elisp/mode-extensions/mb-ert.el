;;; mb-ert.el --- ERT navigation and test commands -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'subr-x)

(declare-function mb-tests-load-baseline "test-manifest")

(defconst *mb-ert-file-prefix* "test-")
(defconst *mb-ert-defun-prefix* "test-")

(cl-defun mb-ert-map ()
  "Return a sub map for functions related to ERT."
  (let ((ert-map (make-sparse-keymap)))
    (define-key ert-map "i" #'mb-ert-insert-skeleton)
    (define-key ert-map "f" #'mb-ert-test-defun)
    (define-key ert-map "b" #'mb-ert-test-buffer)
    (define-key ert-map "d" #'mb-ert-test-buffer-directory)
    (define-key ert-map "e" #'mb-ert-test-mb-elisp)
    (define-key ert-map "a" #'elisp-test-all)
    ert-map))

(cl-defun mb-ert-test-filename-p (filename)
  "Return non-nil if and only if FILENAME is an ERT module."
  (and (stringp filename)
       (string-match-p
        (format "\\`%s.*\\.el\\'" (regexp-quote *mb-ert-file-prefix*))
        (file-name-nondirectory filename))))
;;(mb-ert-test-filename-p "qwe/test-mb-utils-div.el")

(cl-defun mb-ert-test-buffer-p (&optional (buffer (current-buffer)))
  "Return non-nil if and only if BUFFER contains an ERT test module.
BUFFER is optional with the current buffer as default value."
  (mb-ert-test-filename-p (buffer-name buffer)))
;;(mb-ert-test-buffer-p (get-buffer "test-mb-utils-div.el"))

(defun mb-ert--defun-symbol-at-point ()
  "Return the symbol defined by the top-level form at point."
  (save-excursion
    (beginning-of-defun)
    (let ((form (read (current-buffer))))
      (and (listp form)
           (symbolp (cl-second form))
           (cl-second form)))))

(defun mb-ert--goto-definition (name)
  "Move to the definition of NAME in the current buffer."
  (goto-char (point-min))
  (when (re-search-forward
         (format
          "^(\\(?:\\(?:cl-\\)?def\\(?:un\\|macro\\)\\|ert-deftest\\)[[:space:]]+%s\\_>"
          (regexp-quote name))
         nil t)
    (beginning-of-line)
    (point)))

(cl-defun mb-ert-insert-skeleton
    (&optional (defun-name (symbol-name (mb-ert--defun-symbol-at-point))))
  "Add test for the function DEFUN-NAME to the ERT test module.
If such module does not exist, create one."
  (interactive)
  (when (= (point-min) (point-max))
    (insert
     (format "(require 'ert)\n(require '%s)\n\n(provide '%s)"
             (file-name-sans-extension
              (file-name-nondirectory (mb-ert-swap-filename (buffer-name))))
             (file-name-sans-extension (buffer-name)))))
  (goto-char (point-max))
  (backward-sexp 1)
  (insert
   (format
    "(ert-deftest %s%s ()\n  \"Test of `%s'\"\n (should (equal (%s ) nil)))\n\n"
    *mb-ert-defun-prefix* defun-name defun-name defun-name))
  (backward-char 10))

;;; Swapping
(cl-defun mb-ert-swap-filename (filename)
  "Swap from module filename to ERT test module filename or vice-versa."
  (let ((name (file-name-nondirectory filename)))
    (when (string-match
           (format "\\`\\(%s\\)?\\(.*\\.el\\)\\'"
                   (regexp-quote *mb-ert-file-prefix*))
           name)
      (expand-file-name
       (if (match-string 1 name)
           (match-string 2 name)
         (concat *mb-ert-file-prefix* (match-string 2 name)))
       (file-name-directory filename)))))
;;(mb-ert-swap-filename "/dir/test-ert.el")

(cl-defun mb-ert-get-test-filename (buffer-or-filename)
  "Return the filename of the ERT test module associated with BUFFER-OR-FILENAME.
The function assumes that if BUFFER-OR-FILENAME's path is
\"path/filename.el\", then the associated test filename's path is
\"path/test-filename.el\"."
  (let* ((buffer (get-buffer buffer-or-filename))
         (filename (if buffer
                       (buffer-file-name buffer)
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
  (let ((other-file (mb-ert-swap-filename filename)))
    (when other-file
      (find-file other-file))))
;;(mb-ert-swap-file)

(cl-defun mb-ert-name-p (name-or-symbol &optional (prefix "test-"))
  (string-prefix-p prefix
                   (if (symbolp name-or-symbol)
                       (symbol-name name-or-symbol)
                     name-or-symbol)))

(cl-defun mb-ert-swap-defun-name (&optional
				  (defun-name
                                   (symbol-name
                                    (mb-ert--defun-symbol-at-point)))
				  to
				  (prefix "test-"))
  "Swap a defun name with the name of its ERT associated test defun, or vice versa.
If TO is :ert then it prepends PREFIX to DEFUN-SYMBOL. If TO
is :non-ert then it removes the length of PREFIX characters from
the left hand side of DEFUN-SYMBOL. For other values of TO, the
function assumes that DEFUN-SYMBOL is the name of a test function
if and only if it has PREFIX has prefix. By default, TO is nil.
\n(fn &optional SYMBOL TO PREFIX)"
  (cl-case to
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

(cl-defun mb-ert-swap-defun-symbol
    (&optional (symbol (mb-ert--defun-symbol-at-point)) &rest args)
  "Swap SYMBOL with the symbol of its associated ERT test, or vice versa.
For more swap options, use `mb-ert-swap-defun-name'.
\n(fn &optional SYMBOL TO PREFIX)"
  (intern (apply #'mb-ert-swap-defun-name (symbol-name symbol) args)))
;;(mb-ert-swap-defun-symbol 'mb-ert-swap-defun-symbol)

(cl-defun mb-swap-ert-defun
    (&optional (symbol-or-name (mb-ert--defun-symbol-at-point)))
  "Swap an emacs lisp file with its associated test file."
  (let ((defun-name (if (symbolp symbol-or-name)
                        (symbol-name symbol-or-name)
                      symbol-or-name)))
    (when (and (mb-ert-swap-file (buffer-file-name))
	       defun-name)
      (if (mb-ert-test-buffer-p)
	(or (mb-ert--goto-definition
             (mb-ert-swap-defun-name defun-name :ert))
	    (mb-ert-insert-skeleton defun-name))
	(mb-ert--goto-definition
         (mb-ert-swap-defun-name defun-name :non-ert))))))
;;(mb-swap-ert-defun)

(cl-defun mb-ert-forward-test ()
  "Move point to next ERT test in buffer.
If no such test is found, it returns NIL."
  (re-search-forward "^[[:space:]]*([[:space:]]*ert-deftest[[:space:]]*" nil t))

(cl-defun mb-ert-file-defuns (test-filename)
  "Return a list of all ERT test function symbols in FILENAME."
  (unless (mb-ert-test-filename-p test-filename)
    (error "Argument is not a path to an ERT test file."))
  (with-temp-buffer
    (insert-file-contents test-filename)
    (cl-delete-duplicates
     (cl-loop while (mb-ert-forward-test)
	   collect (read (current-buffer)))
     :from-end t)))
;;(mb-ert-file-defuns "~/projects/utils/elisp/mode-extensions/test-mb-texinfo.el")

(cl-defun mb-ert-directory-defuns (directory)
  "Return a list of all ERT test function symbols in DIRECTORY."
  (cl-loop for f in (directory-files directory t "\\.el\\'")
	if (mb-ert-test-filename-p f)
	append (mb-ert-file-defuns f)))
;;(mb-ert-directory-defuns "~/projects/utils/elisp/utils")

(cl-defun mb-non-ert-files (&optional (directory "."))
  "Return a list of elisp files in DIRECTORY that are not test files."
  (cl-remove-if #'mb-ert-test-filename-p
                (directory-files directory nil "\\.el\\'")))
;;(mb-non-ert-files ".")

(cl-defun mb-ert-files (&optional (directory ".") verify-p)
  "Return a list of every ERT test file in DIRECTORY.
If VERIFY-P is not nil, then the function checks that each test
file corresponds to an actual elisp file in DIRECTORY."
  (let ((res (directory-files directory nil "^test-.*\\.el\\'")))
    (let ((unmatched
           (and verify-p
                (cl-set-difference
                 (cl-loop for x in res
                          collect (cl-subseq x
                                            (length *mb-ert-file-prefix*)))
                 (mb-non-ert-files directory)
                 :test #'string=))))
      (when unmatched
        (warn "The following test files do not have matching elisp files:\n%s"
              (mapconcat
               (lambda (name) (concat *mb-ert-file-prefix* name))
               unmatched "\n"))))
    res))
;;(mb-ert-files "." t)

(cl-defun mb-test-ert-symbols (symbols)
  (ert (cons 'member (if (listp symbols) symbols (list symbols)))))

(cl-defun mb-eval/load (&optional (buffer-or-file (current-buffer)))
  "Evalute or load BUFFER-OR-FILE.
If BUFFER-OR-FILE is a designator for an existing buffer, that
is, it is either a buffer object or the name of an existing
buffer, then the function will evaluate that buffer with
`eval-buffer'. Else, if buffer-or-file is a path to an existing
file, this file will be loaded, but not inserted to a new buffer
in Emacs."
  (let ((buffer (get-buffer buffer-or-file)))
    (if buffer
        (eval-buffer buffer)
      (load buffer-or-file))))
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

(cl-defun mb-ert-test-defun ()
  "Perform ERT test of defun at point
Does not support testing of defun when point is at its test function."
  (interactive)
  (let ((symbol (mb-ert--defun-symbol-at-point)))
    (when symbol
      (mb-ert-test-symbols
       (list (mb-ert-swap-defun-symbol symbol :ert-soft))))))

(cl-defun mb-ert-test-region (_beg _end)
  (interactive "r")
  (error "Not implemented"))

(cl-defun mb-ert-test-file (filename)
  "Test all functions in FILENAME.
FILENAME must be an absolute path or relative to current
directory. It can be either a test file, or the module associated
with a test file. In the latter case, every function in the
module with a corresponding ert test function will be included in
the test."
(interactive "fModule or test file: ")
  (mb-eval/load-ert-pair filename)
  (mb-ert-test-symbols
   (mb-ert-file-defuns (mb-ert-get-test-filename filename))))
;;(mb-ert-test-file "../utils/test-mb-utils-div.el")

(cl-defun mb-ert-test-buffer (&optional (buffer (current-buffer)))
  "Run all ERT tests associated with defuns in BUFFER."
  (interactive)
  (mb-ert-test-file (buffer-file-name buffer)))
;;(mb-ert-test-buffer (get-buffer "mb-utils-div.el"))

(cl-defun mb-ert-test-files (files)
  "Invoke ERT on every test function in FILES.
Note: Either this or mb-ert-test-file is obsolete."
  (let ((fs (cl-remove-if-not #'mb-ert-test-filename-p files)))
    (mapc #'mb-eval/load-ert-pair fs)
    (mb-ert-test-symbols (mapcan #'mb-ert-file-defuns fs))))

(cl-defun mb-ert-test-buffer-directory (&optional (buffer (current-buffer)))
  "Invoke ERT on every test function in current directory."
  (interactive)
  (mb-ert-test-files
   (directory-files
    (with-current-buffer buffer default-directory) t "\\.el\\'")))

(cl-defun mb-ert-test-buffer-directories (dirs)
  "Invoke ERT on every test function in directories DIRS."
  (mb-ert-test-symbols
   (cl-loop for d in dirs
	 for fs = (cl-remove-if-not
                   #'mb-ert-test-filename-p
		   (directory-files d t "\\.el\\'"))
	 do (mapc #'mb-eval/load-ert-pair fs)
	 append (mapcan #'mb-ert-file-defuns fs))))

(cl-defun mb-ert-test-mb-elisp (&optional _buffer)
  "Load and run the maintained Emacs Lisp test baseline."
  (interactive)
  (require 'test-manifest)
  (mb-tests-load-baseline)
  (ert t))

(cl-defun elisp-test-all (&optional buffer)
  "Compatibility command for `mb-ert-test-mb-elisp'."
  (interactive)
  (mb-ert-test-mb-elisp buffer))
;;(elisp-test-all (get-buffer "mb-utils-div.el"))

(cl-defun ert-string-to-file (string file)
  "Write STRING to FILE for use in ERT fixtures."
  (with-temp-file file
    (insert string)))

(provide 'mb-ert)

;;; mb-ert.el ends here
