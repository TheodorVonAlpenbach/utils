;;; smart-swap.el --- Switch to related files -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(autoload 'mb-swap-ert-defun "mb-ert" nil t)

(defvar *smart-swaps*
  '((tex pdf)
    (ly pdf)
    (gp pdf)
    (dot png)
    (conf sh)
    (cpp h :search 1)
    (scss js)
    ("Lilypond-compile"
     ("LilyPond started at .*\n\nlilypond \\(.*\\)$" 1)))
  "Pairs describing related file or buffer targets.
A symbol denotes a file extension.  A string on the matching side
is a regexp tested against the visited filename or buffer name.
A target list has the form (REGEXP GROUP) and extracts a filename
from the current buffer.  Additional pair properties are retained
for compatibility but currently ignored.")

(defvar *simple-swaps* nil
  "Explicit file pairs used by `simple-swap'.
Each entry is (FILE-1 FILE-2) or (FILE-1 FILE-2 DIRECTORY).")

(defvar *version-swaps* nil
  "Directory pairs containing corresponding versions of files.")

(defun mb-smart-swap-substitute-extension (filename extension)
  "Return FILENAME with its extension replaced by EXTENSION."
  (unless filename
    (user-error "Current buffer is not visiting a file"))
  (concat (file-name-sans-extension filename)
          "."
          (if (symbolp extension)
              (symbol-name extension)
            extension)))

(defalias 'substitute-extension #'mb-smart-swap-substitute-extension)

(defun mb-smart-swap--buffer-text ()
  "Return the current buffer contents without text properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun mb-smart-swap--existing-file (filename)
  "Expand FILENAME and return it when it exists."
  (when filename
    (let ((expanded (expand-file-name filename default-directory)))
      (and (file-exists-p expanded) expanded))))

(defun mb-smart-swap-expand-target (expression)
  "Expand swap target EXPRESSION in the current buffer.
Return nil when the target cannot be derived."
  (cond
   ((symbolp expression)
    (mb-smart-swap-substitute-extension buffer-file-name expression))
   ((stringp expression)
    (expand-file-name expression default-directory))
   ((and (listp expression)
         (= (length expression) 2)
         (stringp (car expression))
         (natnump (cadr expression)))
    (let ((text (mb-smart-swap--buffer-text)))
      (save-match-data
        (when (string-match (car expression) text)
          (let ((match (match-string (cadr expression) text)))
            (and match
                 (expand-file-name match default-directory)))))))
   (t
    (user-error "Invalid smart-swap target: %S" expression))))

(defalias 'expand-swap-target #'mb-smart-swap-expand-target)

(defun mb-smart-swap-target (expression)
  "Return the existing file named by target EXPRESSION."
  (mb-smart-swap--existing-file
   (mb-smart-swap-expand-target expression)))

(defalias 'swap-target #'mb-smart-swap-target)

(defun mb-smart-swap-match-current-buffer-p (expression)
  "Return non-nil when EXPRESSION identifies the current buffer."
  (cond
   ((symbolp expression)
    (and buffer-file-name
         (equal (file-name-extension buffer-file-name)
                (symbol-name expression))))
   ((stringp expression)
    (string-match-p expression
                    (or buffer-file-name (buffer-name))))
   ((consp expression)
    (mb-smart-swap-match-current-buffer-p (car expression)))
   (t
    (user-error "Invalid smart-swap matcher: %S" expression))))

(defalias 'match-current-buffer-p
  #'mb-smart-swap-match-current-buffer-p)

(defun mb-smart-swap-find-target (&optional swaps)
  "Return the first existing target from SWAPS for the current buffer."
  (cl-loop for entry in (or swaps *smart-swaps*)
           thereis
           (progn
             (unless (and (listp entry) (>= (length entry) 2))
               (user-error "Invalid smart-swap rule: %S" entry))
             (let ((from (nth 0 entry))
                   (to (nth 1 entry)))
               (or (and (mb-smart-swap-match-current-buffer-p from)
                        (mb-smart-swap-target to))
                   (and (mb-smart-swap-match-current-buffer-p to)
                        (mb-smart-swap-target from)))))))

(defalias 'smart-swap-find-target #'mb-smart-swap-find-target)

(defun regexp-swap (file-regexp)
  "Visit the file extracted using FILE-REGEXP.
FILE-REGEXP has the form (REGEXP GROUP)."
  (when-let ((target (mb-smart-swap-target file-regexp)))
    (find-file target)))

(defun elisp-swap ()
  "Swap an Emacs Lisp file with its associated ERT test file."
  (when (derived-mode-p 'emacs-lisp-mode)
    (mb-swap-ert-defun)))

(defun mb-smart-swap--buffer-directory ()
  "Return the current buffer's directory."
  (if buffer-file-name
      (file-name-directory buffer-file-name)
    default-directory))

(defun mb-smart-swap--other-directory (directory swaps)
  "Return the directory paired with DIRECTORY in SWAPS."
  (cl-loop for entry in swaps
           thereis
           (progn
             (unless (and (listp entry) (= (length entry) 2))
               (user-error "Invalid version-swap rule: %S" entry))
             (let ((first
                    (file-name-as-directory
                     (expand-file-name (nth 0 entry))))
                   (second
                    (file-name-as-directory
                     (expand-file-name (nth 1 entry)))))
               (cond ((file-equal-p directory first) second)
                     ((file-equal-p directory second) first))))))

(defun version-swap (&optional swaps)
  "Visit the corresponding file in a directory pair from SWAPS."
  (when buffer-file-name
    (when-let ((directory
                (mb-smart-swap--other-directory
                 (mb-smart-swap--buffer-directory)
                 (or swaps *version-swaps*))))
      (find-file
       (expand-file-name
        (file-name-nondirectory buffer-file-name)
        directory)))))

(defun simple-swap-1 (file-1 file-2 &optional directory)
  "Visit the counterpart of FILE-1 or FILE-2 in DIRECTORY."
  (when buffer-file-name
    (let* ((base (or directory (mb-smart-swap--buffer-directory)))
           (first (expand-file-name file-1 base))
           (second (expand-file-name file-2 base))
           (current (expand-file-name buffer-file-name)))
      (cond ((file-equal-p current first) (find-file second))
            ((file-equal-p current second) (find-file first))))))

(defun simple-swap (&optional swaps)
  "Visit the first explicit counterpart found in SWAPS."
  (interactive)
  (cl-loop for entry in (or swaps *simple-swaps*)
           unless (and (listp entry)
                       (memq (length entry) '(2 3)))
           do (user-error "Invalid simple-swap rule: %S" entry)
           thereis (apply #'simple-swap-1 entry)))

(defun smart-swap (&optional swaps)
  "Visit a file related to the current buffer according to SWAPS."
  (interactive)
  (or (simple-swap)
      (elisp-swap)
      (version-swap)
      (when-let ((target (mb-smart-swap-find-target
                          (or swaps *smart-swaps*))))
        (find-file target))
      (progn
        (message "Couldn't find swap target for current buffer")
        nil)))

(provide 'smart-swap)

;;; smart-swap.el ends here
