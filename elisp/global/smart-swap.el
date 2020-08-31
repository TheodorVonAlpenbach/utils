;;; Swap files. For older versions, see archive.

(defvar *smart-swaps*
  '((tex pdf)
    (ly pdf)
    (gp pdf)
    (dot png)
    (conf sh)
    (cpp h :search 1)
    (scss js)
    ("Lilypond-compile" ("LilyPond started at .*\n\nlilypond \\(.*\\)$" 1)))
  "TODO: enable :search")

(defvar *version-swaps* nil
  "A list on the same format as *smart-swaps*, except the elements are directories.
When invoked, smart-swap will swap the current buffer with the
file in the other swap directory.

For example:

\(push '(\"dir1/foo.txt\" \"dir
\(find-file \"dir1/foo.txt\")
\(smart-swap)
;; smart swap will find and load 'dir2/foo.txt' into the current buffer
\(smart-swap)
;; smart swap will find and load 'dir1/foo.txt' into the current buffer
")

(require 'mb-ert)
(defun elisp-swap ()
  "Swap an emacs lisp file with its associated test file."
  (when (eql major-mode 'emacs-lisp-mode)
    (mb-swap-ert-defun)))

(defun regexp-swap (file-regexp)
  (destructuring-bind (regexp num) file-regexp
    (awhen (string-match* regexp (buffer-string-no-properties) :num 1)
      (find-file it))))

(defun version-swap ()
  (awhen (swap (buffer-directory) *version-swaps* :test #'file-equal-p)
    (find-file (expand-file-name (buffer-file-name-nondirectory) it))))

(defun substitute-extension (filename extension)
  (format "%s.%s" (file-name-sans-extension filename) (sstring extension)))
;;(substitute-extension (buffer-file-name) 'ly)

(defun expand-swap-target (expr)
  (cond	((symbolp expr)
	 (substitute-extension (buffer-file-name) expr))
	((stringp expr)
	 expr)
	((consp expr)
	 (string-match* (first expr) (buffer-string-no-properties) :num (second expr)))))
;;(swap-target 'el)
;;(swap-target '("filename='\\(.*\\)'" 1))

(defun swap-target (expr)
  "Here I should allow for search"
  (awhen (expand-swap-target expr)
    (when (file-exists-p it) it)))
;;(length (directory-files "." t))

(defun match-current-buffer-p (expr)
  (cond ((symbolp expr)
	 (when (string= (file-name-extension (buffer-file-name))
			(symbol-name expr))
	   (buffer-file-name)))
	((stringp expr)
	 (when (string-match expr (buffer-file-name))
	   (buffer-file-name)))))

(cl-defun smart-swap-find-target (&optional (swaps *smart-swaps*))
  (loop for (from to) in swaps
	when (or (when (match-current-buffer-p from) (swap-target to))
		 (when (match-current-buffer-p to) (swap-target from)))
     return it))

(cl-defun simple-swap-1 (f1 f2 &optional (dir (buffer-directory)))
  "TODO: handle absolute file names"
  (let ((bfn (buffer-file-name (current-buffer)))
	(fn1 (expand-file-name f1 dir))
	(fn2 (expand-file-name f2 dir)))
    (if (string= fn1 bfn)
     (find-file fn2)
     (if (string= bfn fn2)
       (find-file fn1)))))

(cl-defun simple-swap (&optional (swaps *simple-swaps*))
  (interactive)
  (loop for (f1 f2 d) in swaps
	thereis (simple-swap-1 f1 f2 d)))
;;(simple-swap)

(cl-defun smart-swap (&optional (swaps *smart-swaps*))
  "Swaps to file as defined in SWAPS"
  (interactive)
  (or (simple-swap)
      (elisp-swap)
      (version-swap)
      (aif (smart-swap-find-target swaps)
	(find-file it)
	(message "Couldn't find swap target for current file"))))

(provide 'smart-swap)
