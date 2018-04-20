(require 'octave)
(require 'mb-octave-eval)
(require 'octave-debug-mode)
(require 'mb-texinfo)

(defun mb-octave-kbd-maps ()
  (let ((mb-local-map (make-sparse-keymap))
	(test-map (make-sparse-keymap)))
    (key-chord-define evil-normal-state-local-map "gh" mb-local-map)
    (define-key mb-local-map "i" (mb-texinfo-insert-map))
    (define-key mb-local-map "r" (mb-texinfo-ref-map))
    (define-key mb-local-map "t" (mb-octave-test-map))
    (define-key mb-local-map "b" (octave-breakpoint-map))
    (define-key mb-local-map "d" (octave-debug-map))))

(defun mb-octave-test-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "f" #'mb-octave-test-buffer-file)
    (define-key map "d" (mb-octave-test-directory-fn (file-name-directory (buffer-file-name)) nil))
    (define-key map "D" (mb-octave-test-directory-fn (file-name-directory (buffer-file-name)) t))
    (define-key map "r" (mb-octave-test-directory-fn "." nil))
    (define-key map "R" (mb-octave-test-directory-fn "." t))
    (define-key map "a" (mb-octave-test-directory-fn "/home/mbe/git/utils/octave/" t))
    (define-key map "A" (mb-octave-test-directory-fn "/home/mbe/git/imms/src/octave/" t))
    map))

(add-hook 'octave-mode-hook #'mb-octave-kbd-maps)
(add-hook 'octave-mode-hook 'turn-on-eldoc-mode)
(add-hook 'octave-mode-hook 'linum-mode)

(defun octave-mode-p (buffer-or-name)
  "Return non nil if BUFFER-OR-NAME is in Octave mode."
  (major-mode-p 'octave-mode buffer-or-name))
;;(octave-mode-p "dealmat.m")

(defun octave-help (fn)
  "Display the documentation of FN."
  (interactive (list (octave-completing-read)))
  (inferior-octave-send-list-and-digest
   (list (format "help ('%s');\n" fn)))
  (let ((lines inferior-octave-output-list)
        (inhibit-read-only t))
    (when (string-match "error: \\(.*\\)$" (car lines))
      (error "%s" (match-string 1 (car lines))))
    (with-help-window octave-help-buffer
      (princ (mapconcat 'identity lines "\n"))
      (with-current-buffer octave-help-buffer
        ;; Bound to t so that `help-buffer' returns current buffer for
        ;; `help-setup-xref'.
        (let ((help-xref-following t))
          (help-setup-xref (list 'octave-help fn)
                           (called-interactively-p 'interactive)))
        ;; Note: can be turned off by suppress_verbose_help_message.
        ;;
        ;; Remove boring trailing text: Additional help for built-in functions
        ;; and operators ...
        (goto-char (point-max))
        (when (search-backward "\n\n\n" nil t)
          (goto-char (match-beginning 0))
          (delete-region (point) (point-max)))
        ;; File name highlight
        (goto-char (point-min))
        (when (re-search-forward "from the file \\(.*\\)$"
		(line-end-position)
		t)
          (let* ((file (match-string 1))
                 (dir (file-name-directory
                       (directory-file-name (file-name-directory file)))))
            (replace-match "" nil nil nil 1)
            (insert "`")
            ;; Include the parent directory which may be regarded as
            ;; the category for the FN.
            (help-insert-xref-button (file-relative-name file dir)
                                     'octave-help-file fn)
            (insert "'")))

        ;; Make 'See also' clickable.
        (with-syntax-table octave-mode-syntax-table
          (when (re-search-forward "^\\s-*See also:" nil t)
            (let ((end (save-excursion (re-search-forward "^\\s-*$" nil t))))
              (while (re-search-forward
			 "\\s-*\\([^,\n]+?\\)\\s-*\\(?:[,]\\|[.]?$\\)" end t)
                (make-text-button (match-beginning 1) (match-end 1)
                                  :type 'octave-help-function)))))

	;; MB: Make 'xref's, 'pxref's, 'ref's clickable.
        (with-syntax-table octave-mode-syntax-table
	  (bob)
          (while (re-search-forward "\\*note\\s-+\\([^:]*\\)::" nil t)
	    (let ((new-beginning (match-beginning 0))
		  (new-end (+ (match-beginning 0) (- (match-end 1)
						     (match-beginning 1)))))
	      (replace-match "\\1")
	      (make-text-button new-beginning new-end
				:type 'octave-help-function))))
        (octave-help-mode)))))
;;(inferior-octave-send-list-and-digest (list (format "help ('%s');\n" "egina_export")))

(defun octave-main-defun-name ()
  "Return the name of the function at point"
  (apply #'buffer-substring-no-properties (last (octave-function-file-p) 2)))

(defun octave-main-defun-string ()
  "Return the complete function defintion as a string."
  (apply #'buffer-substring-no-properties (subseq (octave-function-file-p) 0 2)))

(defun octave-parse-defun-header (string)
  "Return the name of the Octave function defined by STRING."
  (string-match* octave-function-header-regexp string :num '(1 2 3)))
;;(octave-parse-defun-header "function induce_index_ (s, y, m, d, h);\nqwe\nqwe")

(defun octave-parse-defun-output-parameters (string)
  "Return the output parameters in the Octave function defined by STRING."
  (string-trim (second (octave-parse-defun-header string))))
;;(octave-parse-defun-output-parameters "function induce_index_ (s, y, m, d, h);\nqwe\nqwe")

(defun octave-parse-defun-name (string)
  (third (octave-parse-defun-header string)))
;;(octave-parse-defun-name "function induce_index_ (s, y, m, d, h);\nqwe\nqwe")

(defun octave-defun-name ()
  "Return the name of the Octave function surrounding point"
  (octave-parse-defun-name (defun-string)))


(defun octave-info (prefix)
  "Open the Info page for Octave.
If the current buffer mode is Octave mode, the Info buffer is
shown in the other window. The other window is created if
necessary.

If PREFIX argument is provided it opens the Info page for Octave
mode in Emacs."
  (interactive "P")
  (cl-flet ((main (nodename bufname)
	      (aif (get-buffer bufname)
		(switch-to-buffer-other-window it)
		(save-excursion
		  (other-window 1)
		  (info nodename bufname)))))
    (if prefix
      (main "(octave-mode.info)Top" "*Octave-mode-info*")
      (main "(octave.info)Top" "*Octave-info*"))))

(defun inferior-octave-info ()
  (interactive)
  (aif (get-buffer "*Octave-info*")
    (switch-to-buffer it)
    (info "(octave.info)Top" "*Octave-info*")))

(defun octave-fill-documentation-paragraph ()
  "Fills an Octave documentation paragraph.
This functionality is not well covered by octave-fill-paragraph"
  (let ((fill-prefix "## ")
	(paragraph-separate (regexp-or paragraph-separate
				       "## -*- texinfo -*-"
				       "## @seealso"
				       "## @deftypefn"))
	(fill-paragraph-function nil))
    (if (use-region-p)
      (fill-region (region-beginning) (region-end))
      (fill-paragraph))))

(defun octave-fill-example-comment-paragraph ()
  "Fills an Octave documentation paragraph.
This functionality is not well covered by octave-fill-paragraph"
  (let ((fill-prefix "## ## ")
	(paragraph-separate (regexp-or paragraph-separate
				       "## [^#]"
				       "## -*- texinfo -*-"
				       "## @seealso"
				       "## @deftypefn"))
	(fill-paragraph-function nil))
    (if (use-region-p)
      (fill-region (region-beginning) (region-end))
      (fill-paragraph))))

(defun octave-in-main-documentation-p ()
  "Returns non-nil iff POINT is in the function file comment region."
  (i-contains (octave-function-file-comment) (point)))

(defun octave-comment-line-p ()
  "Return non-nil if point is in a commented line."
  (string-match "^[ \t]*#+" (line-string)))

(defun octave-in-documentation-p ()
  "Return non-nil if point is a larger Octave comment section.
Currently only the documentation of the main function is
supported. To implement the general version we need to know if we
are between functions.

Consider also `octave-function-file-comment'."
  (string-match "\\(^##.*\\)"
		(line-string)))

(defun octave-in-example-comment-p ()
  "Returns non-nil if point is in a comment above a doc example."
  (string-match "\\(^##[[:space:]]+##.*\\)"
		(line-string)))

(defun mb-octave-fill-paragraph ()
  (interactive)
  (if (octave-in-example-comment-p)
    (octave-fill-example-comment-paragraph)
    (if (octave-in-documentation-p)
      (octave-fill-documentation-paragraph)
      (octave-fill-paragraph))))

;;;; testing
(defun mb-octave-test-buffer-file ()
  "Run tests for the current buffer's file."
  (interactive)
  (let ((octave-send-echo-input nil))
    (octave-send-string
     (format "source (\"%s\"); test (\"%s\");"
       (expand-file-name ".octaverc" (buffer-directory))
       (buffer-file-name))
     t)))
;;(mb-octave-test-buffer-file)

(defun mb-octave-test-directory-fn (directory &optional recursively-p verbose-p)
  "Run tests for all files in the current buffer's directory.
If RECURSIVELY-P is not NIL, then the test covers the files in
all the subdirectories as well."
  (lexical-let ((dir directory)
		(recursively-p* recursively-p)
		(verbose-p* verbose-p))
    (lambda (prefix)
      (interactive "P")
      (let ((octave-send-echo-input nil))
	(print (format "prefix: %S, verbose-p*: %S" prefix verbose-p*))
	(octave-send-string
	 (format "mbruntests %s %s %s"
	   dir
	   (if recursively-p* "1" "0")
	   (if (or verbose-p* prefix) "1" "0"))
	 t)))))

;;; Shortcuts (TODO: make evil shortcuts out of them)
(define-key octave-mode-map (kbd "M-q") 'mb-octave-fill-paragraph)

(define-key octave-mode-map (kbd "C-h i") #'octave-info)

(define-key inferior-octave-mode-map (kbd "C-h i") #'inferior-octave-info)

(define-key octave-mode-map (kbd "C-c i @") #'texinfo-insert-@var*)

(defun filter-octave-texinfo-line (string functions)
  "Expand the @seealso macro to @xref-s and @ref-s.
The macro @seealso is not a part of standard texinfo, so it is
converted like this:

EDIT: update, becase this have been changed:

## @seealso{fn1, fn2, ..., fnN}
-->
@xref{fn1}, @ref{fn2}, ..., and @ref{fnN} for related topics.

Note the use of @xref for the first reference. In a printed
manual this macro adds 'See ' before the reference string, while
@ref ommits it. Unfortunaly, in Info both macros add a prefix, so
it does not look that good...

Important note: The function discards any of the arguments fn1,
fn2, ..., fnN, if it is not an element in FUNCTIONS."
  (if (string-match "@seealso" string)
    (awhen (cl-intersection (rest (split-string string "[{, }]+" t)) functions
			    :test #'string=)
      (if it
	(concat "See also "
		(andcat (mapcar (bind #'texinfo-@fiy "ref" 1) it))
		" for related topics.")
	(error "qwe")))
    string))
;;(filter-octave-texinfo-line "@seealso{strptime, localtime}" '("strptime" "localtime"))

(defun mb-octave2texinfo-1 (filename fn functions)
  "Extract the texinfo part of FILENAME and convert it to a manual node."
  (let ((doc (string-match* "## -\\*- texinfo -\\*-\n\\(\\(?:##.*\n\\)*\\)"
	       (file-string filename) :num 1)))
    (when doc
      (concat* (loop for l in (string-lines doc)
		     for nl = (replace-regexp-in-string "^## ?" "" l)
		     collect (filter-octave-texinfo-line nl functions))
	:in "\n"
	:pre (format "@node %s\n@subsection %s\n@cindex subsection, %s\n\n"
	       fn fn fn)
	:discard-nil t))))
;;(mb-octave2texinfo-1 "~/git/utils/octave/lsbin/ls2unixtime.m" "ls2unixtime" '("ls2unixtime"))

(defun path2octave-function (filename)
  (file-name-sans-extension (file-name-nondirectory filename)))
;;(path2octave-function "~/git/utils/octave/lsbin/ls2unixtime.m")

(defun mb-octave2texinfo-menu (functions)
  (concat* functions
    :key #'(lambda (x) (format "* %s::" x))
    :pre "@menu\n"
    :in "\n"
    :suf "\n@end menu"))
;;(mb-octave2texinfo-menu '("lsdate2tm" "lsdate2tm"))

(defun mb-octave2texinfo (filenames functions)
  "Extract the texinfo part of FILENAMES and convert it to a manual node.
See also `mb-octave2texinfo-1'."
  (loop for f in filenames
	for fn in (mapcar #'path2octave-function filenames)
	for fnode = (mb-octave2texinfo-1 f fn functions)
	if fnode
	collect fn into fns
	and collect fnode into fnodes
	finally return (concat
			 (mb-octave2texinfo-menu fns)
			 "\n\n"
			 (concat* fnodes :in "\n\n"))))
;;(mb-octave2texinfo '("~/git/utils/octave/lsbin/lsread.m") '("lsread"))
;;(mb-octave2texinfo '("~/git/utils/octave/lsbin/lsdate2tm.m") '("lsdate2tm"))

(provide 'mb-octave)
