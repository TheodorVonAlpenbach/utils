;;; mb-utils-io.el --- File, CSV, and binary I/O helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Text and binary file access, temporary buffer contexts, CSV conversion, and
;; byte-stream helpers used by the MIDI modules.

;;; Code:

(require 'cl-lib)
(require 'hexl)
(require 'pp)
(require 'subr-x)

(defun read* (file)
  "Read and return the first Emacs Lisp object in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(cl-defun print* (object file &optional pretty-print-p)
  "Write OBJECT as an Emacs Lisp object to FILE.
Pretty-print OBJECT when PRETTY-PRINT-P is non-nil.  Replace any existing
contents and return `finished'."
  (with-temp-file file
    (if pretty-print-p
        (pp object (current-buffer))
      (print object (current-buffer)))
    'finished))

(cl-defun buffer-string-no-properties (&optional (buffer (current-buffer)))
  "Return all text in BUFFER without text properties."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(cl-defun file-string (filename &optional coding-system)
  "Return the contents of FILENAME as a string.
Bind `coding-system-for-read' to CODING-SYSTEM when it is non-nil.
Resolve relative file names against `default-directory'."
  (with-temp-buffer
    (let ((coding-system-for-read coding-system))
      (insert-file-contents filename)
      (buffer-string))))

(cl-defun file-lines (filename &optional coding-system)
  "Return the contents of FILENAME as a list of lines.
Pass CODING-SYSTEM to `file-string'."
  (string-lines (file-string filename coding-system)))

(cl-defun string-to-file (string path &optional (overwrite t))
  "Write STRING to PATH.
Overwrite existing contents when OVERWRITE is non-nil; otherwise append
STRING."
  (funcall (if overwrite #'write-region #'append-to-file) string nil path))

(defmacro with-buffer* (buffer &rest body)
  "Evaluate BODY in BUFFER and return the value of its last form.
Restore the caller's current buffer afterwards, including when BODY
signals an error.  Point changes in BUFFER persist."
  (declare (indent 1) (debug t))
  `(with-current-buffer ,buffer
     ,@body))
;;(with-buffer* *lynx-buf* (goto-char 10))

(defmacro with-buffer (buffer-or-name &rest body)
  "Evaluate BODY in BUFFER-OR-NAME and return its last value.
Nil means the current buffer.  Restore the caller's current buffer
afterwards, including when BODY signals an error.  Point changes in
the target buffer persist."
  (declare (indent 1) (debug t))
  `(with-current-buffer (or ,buffer-or-name (current-buffer))
     ,@body))
;;(with-buffer *lynx-buf* (goto-char 2))

(defmacro with-buffers (buffers-or-names &rest body)
  "Evaluate BODY in every buffer in BUFFERS-OR-NAMES.
Return nil, as for a `dolist' used only for side effects."
  (declare (indent 1) (debug t))
  (let ((buffer (cl-gensym "buffer")))
    `(dolist (,buffer ,buffers-or-names)
       (with-buffer ,buffer
         ,@body))))
;;(with-buffer *lynx-buf* (goto-char 2))


(cl-defmacro with-point ((point &optional buffer-or-name) &rest body)
  "Evaluate BODY at POINT in BUFFER-OR-NAME.
Nil BUFFER-OR-NAME means the current buffer.  Point changes in the
target buffer persist."
  (declare (indent 1) (debug ((form &optional form) body)))
  `(with-buffer (or ,buffer-or-name (current-buffer))
     (goto-char ,point)
     ,@body))
;;(with-point (290 "mb-utils-io.el") (line-string))

(defmacro with-file (file &rest body)
  "Insert FILE in a temporary buffer and evaluate BODY.
Save the possibly modified buffer back to FILE after BODY returns."
  (declare (indent 1) (debug t))
  `(with-temp-file ,file
     (insert-file-contents ,file)
     ,@body))

(defmacro with-file-readonly (file &rest body)
  "Insert FILE in a temporary buffer and evaluate BODY without saving."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert-file-contents ,file)
     ,@body))
  
(cl-defun blank-buffer (&key (buffer (current-buffer)) start end)
  "Delete the region between START and END in BUFFER.
START and END default to the accessible beginning and end of BUFFER."
  (with-current-buffer buffer
    (delete-region (or start (point-min)) (or end (point-max)))))

(cl-defun mb-csv--read-field (line start char-separator)
  "Read one field from LINE at START.
Return (FIELD . NEXT-POSITION), where NEXT-POSITION is nil after
the final field.  CHAR-SEPARATOR delimits unquoted fields."
  (cl-loop with field
           with quoted
           with length = (length line)
           for index from start below length
           for character = (aref line index)
           do
           (cond
            (quoted
             (cond
              ((and (= character ?\")
                    (< (1+ index) length)
                    (= (aref line (1+ index)) ?\"))
               (push ?\" field)
               (cl-incf index))
              ((and (= character ?\\)
                    (< (1+ index) length))
               (cl-incf index)
               (push (aref line index) field))
              ((= character ?\")
               (setq quoted nil))
              (t
               (push character field))))
            ((= character ?\")
             (setq quoted t))
            ((= character char-separator)
             (cl-return (cons (concat (nreverse field)) (1+ index))))
            (t
             (push character field)))
           finally
           (when quoted
             (error "Unterminated quoted CSV field"))
           (cl-return (cons (concat (nreverse field)) nil))))

(cl-defun parse-csv-line (line &optional (char-separator ?,) (omit-nulls t))
  "Parse one CSV LINE separated by CHAR-SEPARATOR.
Quoted fields may contain separators.  Both doubled quotes and
backslash-escaped characters are accepted inside quoted fields.
When OMIT-NULLS is non-nil, omit empty fields."
  (cl-loop with position = 0
           with done
           until done
           for (field . next-position) =
           (mb-csv--read-field line position char-separator)
           unless (and omit-nulls (string-empty-p field))
           collect field
           do (if next-position
                (setq position next-position)
                (setq done t))))
;;(parse-csv-line "** 30031,\"FR165 L34P LO S.S., MIN\",** 30031")
;;(parse-csv-line "")

(defvar *csv-show-progress* nil
  "When non-nil, report progress while parsing CSV rows.")

(cl-defun parse-csv-string (string &optional
				   (column-separator ";")
				   line-separator (omit-nulls t))
  "Parse CSV data in STRING into a list of rows.
COLUMN-SEPARATOR may be a regexp string for simple splitting or a
character for quote-aware field parsing.  LINE-SEPARATOR defaults to a
newline regexp.  When OMIT-NULLS is non-nil, omit blank lines and empty
fields parsed with a character separator."
  (cl-loop with lines = (split-string string (or line-separator "\n") omit-nulls)
	for x in lines
	for i from 0
	while (and x (not (string-empty-p (string-trim x))))
	if *csv-show-progress* do (message "%d/%d" i (length lines))
	collect (if (stringp column-separator)
		  (split-string x column-separator)
		  (parse-csv-line x column-separator omit-nulls))))

(cl-defun parse-csv-file (filename &rest args)
  "Parse CSV file FILENAME into a list of rows.
Pass ARGS to `parse-csv-string'."
  (apply #'parse-csv-string (file-string filename) args))

(cl-defun csv-string (lists &optional (column-separator ";")
			      line-separator (nil-conversion ""))
  "Serialize LISTS as separated rows.
COLUMN-SEPARATOR defaults to semicolon and LINE-SEPARATOR to newline.
Render non-nil values with `%S' and nil values as NIL-CONVERSION."
  (mapconcat
   (lambda (row)
     (mapconcat (lambda (value)
                  (if value (format "%S" value) nil-conversion))
                row column-separator))
   lists (or line-separator "\n")))
;;(csv-string '((a b c) (1 nil 3)))

(cl-defun write-csv (lists filename &key column-separator line-separator
				      overwrite)
  "Serialize LISTS and write them to FILENAME.
COLUMN-SEPARATOR and LINE-SEPARATOR have the meanings documented by
`csv-string'.  Overwrite FILENAME when OVERWRITE is non-nil; otherwise
append the serialized rows."
  (string-to-file
   (csv-string lists
     (or column-separator ";") (or line-separator "\n")) filename overwrite))

;;; Binary stuff
(cl-defun read-byte-vector (file &key (from 0) to)
  "Return bytes FROM through TO in FILE as a vector.
FROM defaults to zero and TO defaults to the end of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil from to)
    (cl-coerce (buffer-string) 'vector)))
;;(pp (read-byte-vector "/cygdrive/c/Users/eier/Documents/MATLAB/miningsuite/ragtime.wav" :to 10000))

(cl-defun write-byte-vector (vector file &key (from 0) to)
  "Write the slice from FROM through TO of byte VECTOR to FILE.
FROM defaults to zero and TO defaults to the end of VECTOR."
  (with-temp-file file
    (set-buffer-multibyte nil)
    (mapc (lambda (byte) (insert-char byte))
          (cl-subseq vector from to))))

(cl-defun read-position ()
  "Return the current byte address in a hexl buffer."
  (hexl-current-address))

(cl-defun read-bytes (n)
  "Return the next N bytes from the current hexl buffer.
Move the stream position forward by N bytes.  N must be a non-negative
integer."
  (unless (and (integerp n) (>= n 0))
    (error "Byte count must be a non-negative integer: %S" n))
  (cl-loop repeat n
	collect (hexl-char-after-point)
	do (when (< (hexl-current-address) hexl-max-address)
	     (hexl-forward-char 1))))

(cl-defun read-byte (&optional (n 1))
  "Return the Nth byte from the current hexl stream.
Discard the preceding N minus one bytes and move the stream position
forward by N bytes.  N must be a positive integer."
  (unless (and (integerp n) (cl-plusp n))
    (error "Byte offset must be a positive integer: %S" n))
  (cl-first (last (read-bytes n))))

(cl-defun bytes-to-buffer (bytes buffer)
  "Replace BUFFER contents with BYTES."
  (with-current-buffer buffer
    (delete-region (point-min) (point-max))
    (mapc #'insert bytes)))

(cl-defun bytes-to-file (bytes file)
  "Write BYTES to FILE, replacing any existing contents."
  (with-temp-buffer
    (bytes-to-buffer bytes (current-buffer))
    (setq buffer-file-name file)
    (setq save-buffer-coding-system 'binary)
    (save-buffer)))
;(bytes-to-file '(1 2 3 4) "c:/emacs-22.1/site-lisp/mb-lisp/utils/test1.bin")

(cl-defun read-variable-length-integer ()
  "Read and return a MIDI variable-length integer.
Return a list containing its decoded value and encoded byte count."
  (let ((bytes (cl-loop for byte = (read-byte) then (read-byte)
		        collect (mod byte 128)
		        until (< byte 128))))
    (list (cl-reduce (lambda (value byte) (+ (* value 128) byte))
                     bytes :initial-value 0)
          (length bytes))))

(provide 'mb-utils-io)

;;; mb-utils-io.el ends here
