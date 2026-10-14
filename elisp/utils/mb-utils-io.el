;;; mb-utils-io.el --- File, CSV, and binary I/O helpers -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'hexl)
(require 'pp)
(require 'subr-x)

(cl-defun overwrite-buffer-with-file (f &optional buf &key (pos nil))
  "Deletes buffer content and inserts file F instead. If :POS is
specified, then goto that posistion in file. Else goto beginning of
file."
  (when buf (set-buffer buf))
  (blank-buffer (current-buffer))
  (insert-file-contents f)
  (goto-char (or pos (point-min))))

(cl-defun read* (file)
  "`read's first elisp object in FILE and returns it."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(cl-defun print* (object file &optional pritty-print-p)
  "`print's OBJECT to FILE. Overwrites if FILE already exists."
  (with-temp-file file
    (if pritty-print-p
      (pp object (current-buffer))
      (print object (current-buffer)))
    'finished))

(cl-defun prinl (list)
  "Pretty prints LIST. Each item is printed on a separated line"
  (cl-loop for x in list
	do (insert (format "%S\n" x))))
;;(prinl '(a b c))

(cl-defun buffer-string-no-properties (&optional (buffer (current-buffer)))
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))
;;(substring (buffer-string-no-properties) 1 30)

(cl-defun file-string (filename &optional coding-system)
  "Returns content of FILENAME as a string.
Note that the function discards the EOF character.

NB! If FILENAME should be absolute, otherwise this function will
expand it to the directory of this module."
  ;; (unless (absolute-path-p filename)
  ;;   (warn "Filename is not absolute. Results are undefined."))
  (with-temp-buffer
    (let ((coding-system-for-read coding-system))
      (insert-file-contents filename)
      (buffer-string))))
;;(file-string "~/tmp/qwe.txt")

(cl-defun file-lines (filename &optional coding-system)
  "Returns content of file as a string.
Note that the function discards the EOF character."
  (string-lines (file-string filename coding-system)))
;;(file-lines "mb-utils-io.el")

(cl-defun string-to-file (string path &optional (overwrite t))
  "Writes STRING to FILE.
If optional argument OVERWRITE is NIL then STRING is appended to
FILE. Otherwise it overwrites the old content."
  (funcall (if overwrite #'write-region #'append-to-file) string nil path))
;;(string-to-file "aaa" "~/tmp/qwe.txt" nil)

(cl-defun overwrite-safe (string &optional (buffer (current-buffer)))
  "Overwrites transaction safe the content of BUFFER with STRING. The
default value of optional parameter BUFFER is the current buffer."
  (save-excursion
    (switch-to-buffer buffer)
    (let ((old-end (point-max)))	; ensure transaction
      (goto-char old-end)
      (insert string)
      (kill-region (point-min) old-end))))

(defmacro with-buffer* (buffer &rest body)
  "Same as with-buffer but allows changes of point."
  (let ((current (gensym)))
    `(let ((,current (current-buffer)))
      (unwind-protect
	(progn
	  (set-buffer ,buffer)
	  ,@body)
	(set-buffer ,current)))))
(def-edebug-spec with-buffer* t)
;;(with-buffer* *lynx-buf* (goto-char 10))

(defmacro with-buffer (buffer-or-name &rest body)
  "Temporarily work with BUFFER-OR-NAME and evalue last form in BODY."
  `(save-excursion
     (set-buffer (or ,buffer-or-name (current-buffer)))
     ,@body))
;;(with-buffer *lynx-buf* (goto-char 2))
(def-edebug-spec with-buffer t)

(defmacro with-buffers (buffers-or-names &rest body)
  "Eval BODY in every buffer in the list BUFFERS-OR-NAMES."
  `(cl-loop for x in ,buffers-or-names
	 do (with-buffer x ,@body)))
;;(with-buffer *lynx-buf* (goto-char 2))
(def-edebug-spec with-buffers t)


(cl-defmacro with-point ((point &optional buffer-or-name) &rest body)
  "Temporarily work with BUFFER at POINT and evalue last form in BODY."
  `(with-buffer (or ,buffer-or-name (current-buffer))
     (goto-char ,point)
     ,@body))
;;(with-point (290 "mb-utils-io.el") (line-string))
(def-edebug-spec with-point t)

(defmacro with-file (file &rest body)
  "Insert FILE in a temporary buffer, evaluate body and save the
\(possibly\) modified buffer back to FILE."
  `(with-temp-file ,file
     (insert-file-contents ,file)
     ,@body))
(def-edebug-spec with-file t)

(defmacro with-file-readonly (file &rest body)
  "Insert FILE in a temporary buffer, evaluate body and kill temporary buffer"
  `(with-temp-buffer
     (unwind-protect 
       (progn
	 (insert-file-contents ,file)
	 ,@body))))
(def-edebug-spec with-file-readonly t)

(cl-defun get-temp-file-name ()
  "Return a unique file name and its temporary directory.
The result is a list of the form (NAME DIRECTORY).  The file is
not created."
  (let ((path (make-temp-name
               (expand-file-name "mb-utils-" temporary-file-directory))))
    (list (file-name-nondirectory path) (file-name-directory path))))

(defmacro with-temp-file* (&rest body)
  "Evaluate BODY in a temporary file buffer.
The temporary path is bound to `file'.  Delete the file and kill
the buffer after BODY finishes."
  (declare (indent 0) (debug t))
  (let ((temporary-path (cl-gensym "temporary-path")))
    `(let* ((,temporary-path (make-temp-file "mb-utils-"))
            (file ,temporary-path))
       (unwind-protect
           (with-temp-buffer
             (setq buffer-file-name file)
             ,@body)
         (when (file-exists-p ,temporary-path)
           (delete-file ,temporary-path))))))
  
(defmacro with-current-directory (directory &rest body)
  "Evalute BODY with current directory temporarily set to DIRECTORY."
  `(let ((default-directory ,directory))
     ,@body))
  
(cl-defun blank-buffer (&key (buffer (current-buffer)) start end)
  (with-current-buffer buffer
    (kill-region (or start (point-min)) (or end (point-max)))))

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

(defvar *csv-show-progress* nil)

(cl-defun parse-csv-string (string &optional
				   (column-separator ";")
				   line-separator (omit-nulls t))
  "Parse CSV data in STRING into a list of rows."
  (cl-loop with lines = (split-string string (or line-separator "\n") omit-nulls)
	for x in lines
	for i from 0
	while (and x (not (string-empty-p (string-trim x))))
	if *csv-show-progress* do (message "%d/%d" i (length lines))
	collect (if (stringp column-separator)
		  (split-string x column-separator)
		  (parse-csv-line x column-separator omit-nulls))))

(cl-defun parse-csv-file (filename &rest args)
  "Parses csv file FILENAME to a list of lists.
See `parse-csv-string' for more details"
  (apply #'parse-csv-string (file-string filename) args))
;;(parse-csv-file "")

(cl-defun csv-string (lists &optional (column-separator ";")
			      line-separator (nil-conversion ""))
  "Default LINE-SEPARATOR is \\n"
  (mapconcat
   (lambda (row)
     (mapconcat (lambda (value)
                  (if value (format "%S" value) nil-conversion))
                row column-separator))
   lists (or line-separator "\n")))
;;(csv-string '((a b c) (1 nil 3)))

(cl-defun write-csv (lists filename &key column-separator line-separator
				      overwrite )
  "Saves  csv file FILENAME to a list of lists."
  (string-to-file
   (csv-string lists
     (or column-separator ";") (or line-separator "\n")) filename overwrite))

;;; Binary stuff
(cl-defun read-byte-vector (file &key (from 0) to)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil from to)
    (cl-coerce (buffer-string) 'vector)))
;;(pp (read-byte-vector "/cygdrive/c/Users/eier/Documents/MATLAB/miningsuite/ragtime.wav" :to 10000))

(cl-defun write-byte-vector (vector file &key (from 0) to)
  (with-temp-file file
    (set-buffer-multibyte nil)
    (mapc (lambda (byte) (insert-char byte))
          (cl-subseq vector from to))))

(cl-defun test-io-byte-vector ()
  (let ((f1 "/cygdrive/c/Users/eier/Documents/MATLAB/miningsuite/ragtime.wav")
	(f2 "/cygdrive/c/Users/eier/Documents/MATLAB/miningsuite/ragtime2.wav"))
    (write-byte-vector (cl-coerce (read-byte-vector f1) 'string) f2)))
;;(test-io-byte-vector)

(cl-defun file-to-bytes (file)
  (with-temp-buffer
    (insert-file-contents file)
    (hexl-mode)
    (buffer-to-bytes (current-buffer))))
;;(file-to-bytes "c:/emacs-22.1/site-lisp/mb-lisp/midi/test-new2.midi")
  
(cl-defun buffer-to-bytes (buffer)
  "this has been checked and ok! next step is to make a read file thing."
  (with-current-buffer buffer
    (hexl-beginning-of-buffer 1)
    (cl-loop with bytes = (list (hexl-char-after-point))
	  for n below hexl-max-address
	  do (progn (hexl-forward-char 1)
		    (push (hexl-char-after-point) bytes))
	  finally return (nreverse bytes))))
;;(read-bytes-from-buffer "test.midi")

(cl-defun read-position ()
  (hexl-current-address))

(cl-defun read-bytes (n)
  "Returns the folloing N bytes in the buffer stream as a list.
Also, stream marker is moved to the point after the NTh byte.
Must be called in a hexl mode buffer context."
  (cl-loop for i below n 
	collect (hexl-char-after-point)
	do (when (< (hexl-current-address) hexl-max-address)
	     (hexl-forward-char 1))))

(cl-defun bs-read-string (n)
  "Reads N bytes from the buffer stream and converts them to a string of length N"
  (apply #'string (read-bytes n)))
;;(bs-read-string (read-bytes-from-file "c:/emacs-22.1/site-lisp/mb-lisp/music-analyzer/test.midi") 4)

(cl-defun read-integer (&optional (n 4))
  "Reads N bytes from the buffer stream and converts them to a N bytes integer"
  (hexl-hex-string-to-integer
   (mapconcat (lambda (byte) (format "%02x" byte)) (read-bytes n) "")))

(cl-defun read-byte (&optional (n 1))
  "Returns the Nth next byte in the buffer stream, and moves
stream marker to the point after this byte. The bytes 1...N-1 are
discareded. Must be called in a hexl mode buffer context."
  (car (last (read-bytes n))))

(cl-defun insert-bytes-in-buffer (bytes buffer)
  "Inserts the BYTES list into BUFFER at curent point"
  (with-current-buffer buffer
    (mapc #'insert bytes)))

(cl-defun bytes-to-buffer (bytes buffer)
  "Removes exising content in BUFFER and inserts the BYTES list into it."
  (with-current-buffer buffer
    (delete-region (point-min) (point-max))
    (mapc #'insert bytes)))

(cl-defun bytes-to-file (bytes file)
  "Creates a new file FILE consisting of BYTES.
If FILE already exists, it will be overwritten."
  (with-temp-buffer
    (bytes-to-buffer bytes (current-buffer))
    (setq buffer-file-name file)
    (setq save-buffer-coding-system 'binary)
    (save-buffer)))
;(bytes-to-file '(1 2 3 4) "c:/emacs-22.1/site-lisp/mb-lisp/utils/test1.bin")

(cl-defun read-variable-length-integer ()
  (let ((bytes (nreverse 
		(cl-loop for byte = (read-byte) then (read-byte)
		      collect (mod byte 128)
		      until (< byte 128)))))
    (list (cl-reduce (lambda (value byte) (+ (* value 128) byte))
                     bytes :initial-value 0)
          (length bytes))))

(provide 'mb-utils-io)

;;; mb-utils-io.el ends here
