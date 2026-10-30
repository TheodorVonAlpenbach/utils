;;; mb-octave.el --- Personal Octave mode extensions -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'info)
(require 'octave)
(require 'subr-x)
(require 'vc)
(require 'mb-octave-eval)
(require 'mb-octave-insert)
(require 'mb-texinfo)

(declare-function key-chord-define "key-chord")
(declare-function octave-breakpoint-map "octave-breakpoint")
(declare-function octave-buffer-breakpoints "octave-breakpoint")
(declare-function octave-debug-map "octave-debug-mode")

(defvar help-xref-following)

(defun mb-octave-kbd-maps ()
  "Install optional Evil/key-chord Octave maps when available."
  (when (and (fboundp 'key-chord-define)
             (boundp 'evil-normal-state-local-map)
             (keymapp evil-normal-state-local-map))
    (let ((mb-local-map (make-sparse-keymap)))
      (key-chord-define evil-normal-state-local-map "gh" mb-local-map)
    (define-key mb-local-map "i" (mb-texinfo-insert-map))
    (define-key mb-local-map "r" (mb-texinfo-ref-map))
    (define-key mb-local-map "s" (mb-texinfo-snippet-map))
    (define-key mb-local-map "c" (mb-octave-insert-map))
    (define-key mb-local-map "t" (mb-octave-test-map))
      (when (fboundp 'octave-breakpoint-map)
        (define-key mb-local-map "b" (octave-breakpoint-map)))
      (when (fboundp 'octave-debug-map)
        (define-key mb-local-map "d" (octave-debug-map))))))

(defun mb-octave-test-map ()
  "Return a keymap for running Octave tests."
  (let ((map (make-sparse-keymap)))
    (define-key map "f" #'mb-octave-test-buffer-file)
    (when buffer-file-name
      (let ((directory (file-name-directory buffer-file-name)))
        (define-key map "d" (mb-octave-test-directory-fn directory nil))
        (define-key map "D" (mb-octave-test-directory-fn directory t))))
    (define-key map "r" (mb-octave-test-directory-fn "." nil))
    (define-key map "R" (mb-octave-test-directory-fn "." t))
    map))
 
(defun mb-octave-init ()
  "Apply personal settings to the current Octave buffer."
  (mb-octave-kbd-maps)
  (eldoc-mode 1)
  (display-line-numbers-mode 1)
  (when (boundp 'evil-symbol-word-search)
    (setq-local evil-symbol-word-search t))
  (set-syntax-table (copy-syntax-table (syntax-table)))
  (modify-syntax-entry ?_ "_")
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key (kbd "M-q") #'mb-octave-fill-paragraph)
  (local-set-key (kbd "C-h i") #'octave-info)
  (local-set-key (kbd "C-c i @") #'mb-texinfo-insert-@var))

(add-hook 'octave-mode-hook 'mb-octave-init)

(with-eval-after-load 'evil
  (require 'octave-debug-mode nil t))

(defun octave-mode-p (buffer-or-name)
  "Return non nil if BUFFER-OR-NAME is in Octave mode."
  (when-let ((buffer (get-buffer buffer-or-name)))
    (with-current-buffer buffer
      (derived-mode-p 'octave-mode))))
;;(octave-mode-p "dealmat.m")

(defun octave-help (fn)
  "Display the documentation of FN."
  (interactive (list (octave-completing-read)))
  (inferior-octave-send-list-and-digest
   (list (format "help ('%s');\n" fn)))
  (let ((lines inferior-octave-output-list)
        (inhibit-read-only t))
    (when (and lines (string-match "error: \\(.*\\)$" (car lines)))
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
	  (goto-char (point-min))
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
  "Return the name of the current Octave function file's main function."
  (let ((region (octave-function-file-p)))
    (unless region
      (user-error "Buffer is not an Octave function file"))
    (apply #'buffer-substring-no-properties (last region 2))))

(defun octave-main-defun-string ()
  "Return the complete main Octave function definition as a string."
  (let ((region (octave-function-file-p)))
    (unless region
      (user-error "Buffer is not an Octave function file"))
    (apply #'buffer-substring-no-properties (cl-subseq region 0 2))))

(defun octave-parse-defun-header (string)
  "Return the name of the Octave function defined by STRING."
  (when (string-match octave-function-header-regexp string)
    (mapcar (lambda (number) (match-string number string)) '(1 2 3))))
;;(octave-parse-defun-header "function induce_index_ (s, y, m, d, h);\nqwe\nqwe")

(defun octave-parse-defun-output-parameters (string)
  "Return the output parameters in the Octave function defined by STRING."
  (when-let ((output (cl-second (octave-parse-defun-header string))))
    (string-trim output)))
;;(octave-parse-defun-output-parameters "function induce_index_ (s, y, m, d, h);\nqwe\nqwe")

(defun octave-parse-defun-name (string)
  "Return the function name from the Octave definition STRING."
  (cl-third (octave-parse-defun-header string)))
;;(octave-parse-defun-name "function induce_index_ (s, y, m, d, h);\nqwe\nqwe")

(defun octave-defun-name ()
  "Return the name of the Octave function surrounding point"
  (save-excursion
    (octave-beginning-of-defun)
    (let ((start (point)))
      (end-of-defun)
      (octave-parse-defun-name
       (buffer-substring-no-properties start (point))))))

(defun octave-buffers ()
  "Return a list of all buffers in octave mode."
  (cl-loop for x being the buffers if
	   (octave-mode-p x) collect x))


;;;; Info stuff
(defun octave-info (prefix)
  "Open the Info page for Octave.
If the current buffer mode is Octave mode, the Info buffer is
shown in the other window. The other window is created if
necessary.

If PREFIX argument is provided it opens the Info page for Octave
mode in Emacs."
  (interactive "P")
  (cl-labels ((main (nodename bufname)
                (if-let ((buffer (get-buffer bufname)))
                    (switch-to-buffer-other-window buffer)
                  (save-excursion
                    (other-window 1)
                    (info nodename bufname)))))
    (if prefix
      (main "(octave-mode.info)Top" "*Octave-mode-info*")
      (main "(octave.info)Top" "*Octave-info*"))))

(defun inferior-octave-info ()
  (interactive)
  (if-let ((buffer (get-buffer "*Octave-info*")))
      (switch-to-buffer buffer)
    (info "(octave.info)Top" "*Octave-info*")))

(defun octave-fill-documentation-paragraph ()
  "Fills an Octave documentation paragraph.
This functionality is not well covered by octave-fill-paragraph"
  (let ((fill-prefix "## ")
	(paragraph-separate
         (mapconcat #'identity
                    (list paragraph-separate
                          "## -*- texinfo -*-"
                          "## @seealso"
                          "## @deftypefn")
                    "\\|"))
	(fill-paragraph-function nil))
    (if (use-region-p)
      (fill-region (region-beginning) (region-end))
      (fill-paragraph))))

(defun octave-fill-example-comment-paragraph ()
  "Fills an Octave documentation paragraph.
This functionality is not well covered by octave-fill-paragraph"
  (let ((fill-prefix "## ## ")
	(fill-column 65)
	(paragraph-separate
         (mapconcat #'identity
                    (list paragraph-separate
                          "## [^#]"
                          "## -*- texinfo -*-"
                          "## @seealso"
                          "## @deftypefn")
                    "\\|"))
	(fill-paragraph-function nil))
    (if (use-region-p)
      (fill-region (region-beginning) (region-end))
      (fill-paragraph))))

(defun octave-in-main-documentation-p ()
  "Returns non-nil iff POINT is in the function file comment region."
  (cl-destructuring-bind (start end) (octave-function-file-comment)
    (<= start (point) end)))

(defun octave-comment-line-p ()
  "Return non-nil if point is in a commented line."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[ \t]*#+")))

(defun octave-in-documentation-p ()
  "Return non-nil if point is a larger Octave comment section.
Currently only the documentation of the main function is
supported. To implement the general version we need to know if we
are between functions.

Consider also `octave-function-file-comment'."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "##.*")))

(defun octave-in-example-comment-p ()
  "Returns non-nil if point is in a comment above a doc example."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "##[[:space:]]+##.*")))

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
  (unless buffer-file-name
    (user-error "Octave buffer is not visiting a file"))
  (let ((octave-send-echo-input nil)
	(rc (expand-file-name ".octaverc"
                              (file-name-directory buffer-file-name))))
    (when (file-exists-p rc)
      (octave-send-string
       (format "source (%s);" (mb-octave--quote-string rc)) t))
    (octave-send-string
     (format "test (%s);" (mb-octave--quote-string buffer-file-name)) t)))
;;(mb-octave-test-buffer-file)

(defun mb-octave-test-directory-fn
    (directory &optional recursively-p verbose-p)
  "Run tests for all files in the current buffer's directory.
If RECURSIVELY-P is not NIL, then the test covers the files in
all the subdirectories as well."
  (let ((dir directory)
		(recursively-p* recursively-p)
		(verbose-p* verbose-p))
    (lambda (prefix)
      (interactive "P")
      (let ((octave-send-echo-input nil))
	(octave-send-string
	 (format "mbruntests %s %s %s"
	   (mb-octave--quote-string (expand-file-name dir))
	   (if recursively-p* "1" "0")
	   (if (or verbose-p* prefix) "1" "0"))
	 t)))))

(define-key inferior-octave-mode-map (kbd "C-h i") #'inferior-octave-info)

(defun mb-octave--join-references (functions)
  "Join FUNCTIONS as an English list of Texinfo references."
  (let ((references
         (mapcar (lambda (function)
                   (texinfo-atfiy "ref" function))
                 functions)))
    (pcase references
      ('() "")
      (`(,only) only)
      (`(,first ,second) (concat first " and " second))
      (_ (concat (string-join (butlast references) ", ")
                 ", and " (car (last references)))))))

(defun filter-octave-texinfo-line (string functions)
  "Expand an Octave @seealso line using known FUNCTIONS.
The @seealso macro is not part of standard Texinfo, so it is
converted like this:

## @seealso{fn1, fn2, ..., fnN}
-->
See also @ref{fn1}, @ref{fn2}, ..., and @ref{fnN} for related topics.

Important note: The function discards any of the arguments fn1,
fn2, ..., fnN, if it is not an element in FUNCTIONS."
  (if (string-match-p "@seealso" string)
      (let ((references
             (cl-remove-if-not
              (lambda (function) (member function functions))
              (cdr (split-string string "[{, }]+" t)))))
        (unless references
          (error "No known functions in @seealso line: %s" string))
        (concat "See also "
                (mb-octave--join-references references)
                " for related topics."))
    string))
;;(filter-octave-texinfo-line "@seealso{strptime, localtime}" '("strptime" "localtime"))

(defun mb-octave2texinfo-1 (filename fn functions)
  "Extract the texinfo part of FILENAME and convert it to a manual node."
  (let* ((source
          (with-temp-buffer
            (insert-file-contents filename)
            (buffer-string)))
         (doc
          (when (string-match
                 "## -\\*- texinfo -\\*-\n\\(\\(?:##.*\n\\)*\\)"
                 source)
            (match-string 1 source))))
    (when doc
      (concat
       (format "@node %s\n@subsection %s\n@cindex subsection, %s\n\n"
               fn fn fn)
       (string-join
        (delq nil
              (cl-loop for line in (string-lines doc)
                       for cleaned =
                       (replace-regexp-in-string "^## ?" "" line)
                       collect
                       (filter-octave-texinfo-line cleaned functions)))
        "\n")))))
;;(mb-octave2texinfo-1 "~/git/utils/octave/lsbin/ls2unixtime.m" "ls2unixtime" '("ls2unixtime"))

(defun path2octave-function (filename)
  (file-name-sans-extension (file-name-nondirectory filename)))
;;(path2octave-function "~/git/utils/octave/lsbin/ls2unixtime.m")

(defun mb-octave2texinfo-menu (functions)
  "Return a Texinfo menu for FUNCTIONS."
  (concat "@menu\n"
          (string-join
           (mapcar (lambda (function) (format "* %s::" function))
                   functions)
           "\n")
          "\n@end menu"))
;;(mb-octave2texinfo-menu '("lsdate2tm" "lsdate2tm"))

(defun mb-octave2texinfo (filenames functions)
  "Extract the texinfo part of FILENAMES and convert it to a manual node.
See also `mb-octave2texinfo-1'."
  (cl-loop for f in filenames
	   for fn in (mapcar #'path2octave-function filenames)
	   for fnode = (mb-octave2texinfo-1 f fn functions)
	   if fnode
	   collect fn into fns
	   and collect fnode into fnodes
	   finally return
           (concat (mb-octave2texinfo-menu fns)
                   "\n\n"
                   (string-join fnodes "\n\n"))))
;;(mb-octave2texinfo '("~/git/utils/octave/lsbin/lsread.m") '("lsread"))
;;(mb-octave2texinfo '("~/git/utils/octave/lsbin/lsdate2tm.m") '("lsdate2tm"))

(defun mb-octave-breakpoints-hook ()
  "Signal an error when a file being checked in has Octave breakpoints."
  (unless (fboundp 'octave-buffer-breakpoints)
    (user-error "Octave breakpoint support is not loaded"))
  (cl-loop
   for file in
   (cl-second (vc-deduce-fileset nil t 'state-model-only-files))
   for buffer = (find-file-noselect file)
   do (when (octave-buffer-breakpoints buffer)
        (error "Breakpoints exist in file %s" file))))
;;(mb-octave-breakpoints-hook)

(with-eval-after-load 'octave-breakpoint
  (add-hook 'vc-before-checkin-hook #'mb-octave-breakpoints-hook))

(provide 'mb-octave)

;;; mb-octave.el ends here
