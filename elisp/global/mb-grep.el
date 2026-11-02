;;; mb-grep.el --- Personal grep commands -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'subr-x)
(require 'thingatpt)

(defgroup mb-grep nil
  "Personal grep commands."
  :group 'grep)

(defcustom mb-grep-program "grep"
  "Program used by `mb-grep-basic'."
  :type 'string)

(defconst mb-grep-cpp-header-types '("h" "hh" "hpp" "hxx"))
(defconst mb-grep-cpp-source-types '("c" "cc" "cpp" "cxx"))

(defun mb-grep-filter-dirs (directories match match-not)
  "Filter DIRECTORIES using MATCH and MATCH-NOT regexps.
MATCH retains matching directories.  MATCH-NOT excludes matching
directories.  Matching is case-sensitive."
  (let ((case-fold-search nil))
    (cl-remove-if
     (lambda (directory)
       (or (and match (not (string-match-p match directory)))
           (and match-not (string-match-p match-not directory))))
     directories)))

(defun mb-grep--brace-types (type)
  "Expand a simple brace expression in TYPE."
  (if (string-match "\\`\\(.*\\){\\([^{}]+\\)}\\(.*\\)\\'" type)
      (let ((prefix (match-string 1 type))
            (suffix (match-string 3 type)))
        (mapcar (lambda (part) (concat prefix part suffix))
                (split-string (match-string 2 type) "," t "[[:space:]]+")))
    (list type)))

(defun mb-grep--types (types)
  "Return TYPES as a flat list of file patterns."
  (mapcan #'mb-grep--brace-types
          (cond ((null types) nil)
                ((listp types) (copy-sequence types))
                (t (list types)))))

(defun mb-grep--type-regexp (type)
  "Return a file-name regexp for TYPE."
  (wildcard-to-regexp
   (cond ((string-match-p "[][?*]" type) type)
         ((string-prefix-p "." type) type)
         (t (concat "*." type)))))

(defun mb-grep--files (directories types)
  "Return regular files directly in DIRECTORIES matching TYPES."
  (let ((regexps (mapcar #'mb-grep--type-regexp (mb-grep--types types))))
    (cl-loop for directory in directories
             append
             (cl-loop for file in
                      (directory-files (expand-file-name directory) t
                                       directory-files-no-dot-files-regexp t)
                      for name = (file-name-nondirectory file)
                      when (and (file-regular-p file)
                                (or (null regexps)
                                    (cl-some
                                     (lambda (regexp)
                                       (string-match-p regexp name))
                                     regexps)))
                      collect file))))

(defun mb-grep-build-command (target directories &optional types match match-not)
  "Build a grep command for TARGET in DIRECTORIES.
TYPES is a file type, wildcard, brace expression, or list thereof.
MATCH and MATCH-NOT filter directory names."
  (unless (and (stringp target) (not (string-empty-p target)))
    (user-error "Grep target must be a nonempty string"))
  (let* ((filtered-directories
          (mb-grep-filter-dirs directories match match-not))
         (files (mb-grep--files filtered-directories types)))
    (unless files
      (user-error "No matching files to search"))
    (mapconcat #'shell-quote-argument
               (append (list mb-grep-program "-nHs" "-E" "--" target)
                       files)
               " ")))

(cl-defun mb-grep-basic
    (&key
     (target (if (use-region-p)
                 (buffer-substring-no-properties
                  (region-beginning) (region-end))
               (thing-at-point 'symbol t)))
     (directories
      (list (if buffer-file-name
                (file-name-directory buffer-file-name)
              default-directory)))
     (types
      (and buffer-file-name
           (or (file-name-extension buffer-file-name)
               (file-name-nondirectory buffer-file-name))))
     match match-not)
  "Run grep for TARGET in DIRECTORIES, restricted by TYPES.
MATCH and MATCH-NOT filter directory names."
  (unless (executable-find mb-grep-program)
    (user-error "Grep program is not executable: %s" mb-grep-program))
  (compilation-start
   (mb-grep-build-command target directories types match match-not)
   'grep-mode))

(defun mb-grep-find-directories (root-directory &optional max-depth)
  "Return directories below ROOT-DIRECTORY through MAX-DEPTH.
ROOT-DIRECTORY itself is depth zero.  A nil MAX-DEPTH means no
depth limit.  Symbolic links to directories are not followed."
  (when (and max-depth
             (not (and (integerp max-depth) (>= max-depth 0))))
    (user-error "Maximum depth must be a nonnegative integer or nil"))
  (let ((root (file-name-as-directory (expand-file-name root-directory)))
        result)
    (unless (file-directory-p root)
      (user-error "Not a directory: %s" root-directory))
    (cl-labels
        ((walk
          (directory depth)
          (push directory result)
          (when (or (null max-depth) (< depth max-depth))
            (dolist (entry
                     (directory-files directory t
                                      directory-files-no-dot-files-regexp t))
              (when (and (file-directory-p entry)
                         (not (file-symlink-p entry)))
                (walk (file-name-as-directory entry) (1+ depth)))))))
      (walk root 0))
    (nreverse result)))

(defalias 'finddirs #'mb-grep-find-directories)
(defalias 'subdirs #'mb-grep-find-directories)

(defun mb-grep--parent-directory (directory levels)
  "Return the LEVELSth parent of DIRECTORY."
  (let ((result (file-name-as-directory (expand-file-name directory))))
    (dotimes (_ levels result)
      (setq result
            (file-name-directory (directory-file-name result))))))

(defun mb-grep-dirs-1 (up max-depth directory)
  "Return directories UP levels above DIRECTORY through MAX-DEPTH."
  (mb-grep-find-directories
   (mb-grep--parent-directory directory up)
   max-depth))

(defun mb-grep-dirs (prefix &optional directory)
  "Return search directories selected by PREFIX from DIRECTORY.
Nil and zero select DIRECTORY recursively.  Positive single-digit
values select a parent recursively.  For larger positive values,
the tens digit is the number of parent levels and the units digit
is maximum depth.

A bare minus prefix selects only DIRECTORY.  Other negative values
reverse the two positive fields: -5 selects DIRECTORY through
depth five, and -14 selects the fourth parent through depth one."
  (setq directory (or directory "."))
  (cond
   ((or (eq prefix '-) (equal prefix "-"))
    (mb-grep-dirs-1 0 0 directory))
   (t
    (let ((value (prefix-numeric-value (or prefix 0))))
      (cond
       ((zerop value)
        (mb-grep-dirs-1 0 nil directory))
       ((> value 0)
        (if (< value 10)
            (mb-grep-dirs-1 value nil directory)
          (pcase-let ((`(,up ,max-depth) (cl-multiple-value-list
                                          (cl-floor value 10))))
            (mb-grep-dirs-1 up max-depth directory))))
       ((> value -10)
        (mb-grep-dirs-1 0 (- value) directory))
       (t
        (pcase-let ((`(,max-depth ,up) (cl-multiple-value-list
                                        (cl-floor (- value) 10))))
          (mb-grep-dirs-1 up max-depth directory))))))))

(defun mb-grep-interactive ()
  "Prompt for a regexp and grep directories selected by the prefix."
  (interactive)
  (mb-grep-basic
   :target (read-from-minibuffer "Grep regexp: ")
   :directories (mb-grep-dirs current-prefix-arg)))

(defun mb-grep ()
  "Grep for the region or symbol using mode-appropriate file types."
  (interactive)
  (pcase major-mode
    ('emacs-lisp-mode (mb-elisp-grep))
    ('mbscilab-mode (mb-scilab-grep))
    (_ (mb-gen-grep))))

(defun mb-gen-grep ()
  "Grep files matching the current buffer's file type."
  (interactive)
  (mb-grep-basic :directories
                 (mb-grep-dirs (or current-prefix-arg 1))))

(defun mb-elisp-grep ()
  "Grep Emacs Lisp files, excluding directories named old."
  (interactive)
  (mb-grep-basic
   :directories (mb-grep-dirs (or current-prefix-arg 1))
   :types "el"
   :match-not "/old/"))

(defun mb-scilab-grep ()
  "Grep Scilab source files."
  (interactive)
  (mb-grep-basic
   :directories (mb-grep-dirs (or current-prefix-arg 2))
   :types '("sci" "sce")
   :match-not (regexp-opt '("CVS" "etc" "help"))))

(defun mb-js-grep ()
  "Grep JavaScript files."
  (interactive)
  (mb-grep-basic :types "js"))

(defun mb-h-grep ()
  "Grep C and C++ header files."
  (interactive)
  (mb-grep-basic :types mb-grep-cpp-header-types))

(defun mb-cpp-grep ()
  "Grep C and C++ source files."
  (interactive)
  (mb-grep-basic :types mb-grep-cpp-source-types))

(defun mb-all-grep ()
  "Grep all files."
  (interactive)
  (mb-grep-basic :types nil))

(defun mb-c++-grep ()
  "Grep C and C++ source and header files."
  (interactive)
  (mb-grep-basic
   :types (append mb-grep-cpp-header-types mb-grep-cpp-source-types)))

(defun mb-lisp-grep ()
  "Grep Common Lisp source files."
  (interactive)
  (mb-grep-basic :types '("lisp" "lsp" "cl")))

(provide 'mb-grep)

;;; mb-grep.el ends here
