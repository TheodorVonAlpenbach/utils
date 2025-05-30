(cl-defun overwrite-buffer-with-file (f &optional buf &key (pos nil))
  "Deletes buffer content and inserts file F instead. If :POS is
specified, then goto that posistion in file. Else goto beginning of
file."
  (when buf (set-buffer buf))
  (blank-buffer (current-buffer))
  (insert-file f)
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
  (with-buffer buffer
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
     (insert-file ,file)
     ,@body))
(def-edebug-spec with-file t)

(defmacro with-file-readonly (file &rest body)
  "Insert FILE in a temporary buffer, evaluate body and kill temporary buffer"
  `(with-temp-buffer
     (unwind-protect 
       (progn
	 (insert-file ,file)
	 ,@body))))
(def-edebug-spec with-file-readonly t)

(cl-defun get-temp-file-name ()
  "Returns a list of two items: a unique file name and a path to a tmp
dir."
  (list (iso-date-and-time :with-seconds t) "C:/WINNT/Temp/"))

(defmacro with-temp-file* (&rest body)
  "Creates a new file and corresponding buffer, evaluates BODY in that
buffer, and kills file and buffer afterwords. Symbol FILE takes the
temporary file name as value."
  `(let ((file (format "C:/WINNT/Temp/%s" (iso-date-and-time :with-seconds t))))
      (unwind-protect
	(progn
	  (find-file-noselect ,file)
	  ,@body)
	(set-buffer ,current))))
  
(defmacro with-current-directory (directory &rest body)
  "Evalute BODY with current directory temporarily set to DIRECTORY."
  `(let ((default-directory directory))
     ,@body))
  
(cl-defun blank-buffer (&key (buffer (current-buffer)) start end)
  (with-buffer buffer
    (kill-region (or start (point-min)) (or end (point-max)))))

(read-from-string ",\"FR165 L34P LO S.S., MIN\",** 30031")
(read-from-string ",** 30031,\"FR165 L34P LO S.S., MIN\",** 30031")
(read-from-string "** 30031,\"FR165 L34P LO S.S., MIN\",** 30031")

(cl-defun parse-csv-column (line &optional (pos 0) (char-separator ?,) (omit-nulls t))
  "Assumes LINE starts with a comma. Use destructuring-bind when this is stable"
  (let* ((x (read-from-string line pos))
	 (res (car x))
	 (newpos (cdr x)))
    (if (stringp res)
      (list res newpos)
      (let ((newpos (cl-position char-separator line :start (1+ pos))))
	(list (substring line pos newpos) newpos)))))
;;(parse-csv-column "** 30031,\"FR165 L34P LO S.S., MIN\",** 30031")
;;(parse-csv-column ",\"FR165 L34P LO S.S., MIN\",** 30031")
;;(parse-csv-column "\n")

(cl-defun parse-csv-line (line &optional (char-separator ?,) (omit-nulls t))
  (cl-loop with i = 0
	with n = (length line)
	with s = (string-trim (concat "," line))
	for (x pos) = (parse-csv-column s 0) then (parse-csv-column s (1+ pos))
	while (and pos (< pos n)) collect x))
;;(parse-csv-line "** 30031,\"FR165 L34P LO S.S., MIN\",** 30031")
;;(parse-csv-line "")

(cl-defun parse-csv-string (string &optional (column-separator ";") line-separator (omit-nulls t))
  "Parses csv file FILENAME to a list of lists."
  (mapcar #'(lambda (x) (if (stringp column-separator)
			  (split-string x column-separator)
			  (parse-csv-line x column-separator omit-nulls)))
	  (split-string string (or line-separator "\n") omit-nulls)))
;;(parse-csv-string "")

(defvar *csv-show-progress* nil)

(cl-defun parse-csv-string (string &optional
				   (column-separator ";")
				   line-separator (omit-nulls t))
  "Parses csv file FILENAME to a list of lists."
  (cl-loop with lines = (split-string string (or line-separator "\n") omit-nulls)
	for x in lines
	for i from 0
	while (and x (not (empty-string-p (string-trim x))))
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
  (concat* lists 
	   :in (or line-separator "\n")
	   :key #'(lambda (list)
		    (concat* list 
			     :in column-separator 
			     :key #'(lambda (x)
				      (if x (format "%S" x) nil-conversion))))))
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
    (insert (subseq vector from to))))

(cl-defun test-io-byte-vector ()
  (let ((f1 "/cygdrive/c/Users/eier/Documents/MATLAB/miningsuite/ragtime.wav")
	(f2 "/cygdrive/c/Users/eier/Documents/MATLAB/miningsuite/ragtime2.wav"))
    (write-byte-vector (cl-coerce (read-byte-vector f1) 'string) f2)))
;;(test-io-byte-vector)

(require 'hexl)
(cl-defun file-to-bytes (file)
  (with-temp-buffer
    (insert-file file)
    (hexl-mode)
    (buffer-to-bytes (current-buffer))))
;;(file-to-bytes "c:/emacs-22.1/site-lisp/mb-lisp/midi/test-new2.midi")
  
(cl-defun buffer-to-bytes (buffer)
  "this has been checked and ok! next step is to make a read file thing."
  (with-buffer buffer
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
  (apply #'concat (mapcar #'byte-to-char (read-bytes n))))
;;(bs-read-string (read-bytes-from-file "c:/emacs-22.1/site-lisp/mb-lisp/music-analyzer/test.midi") 4)

(cl-defun read-integer (&optional (n 4))
  "Reads N bytes from the buffer stream and converts them to a N bytes integer"
  (hexl-hex-string-to-integer
   (apply #'concat (mapcar #'byte-to-2hex (read-bytes n)))))

(cl-defun read-byte (&optional (n 1))
  "Returns the Nth next byte in the buffer stream, and moves
stream marker to the point after this byte. The bytes 1...N-1 are
discareded. Must be called in a hexl mode buffer context."
  (first (last (read-bytes n))))

(cl-defun insert-bytes-in-buffer (bytes buffer)
  "Inserts the BYTES list into BUFFER at curent point"
  (with-buffer buffer
    (mapc #'insert bytes)))

(cl-defun bytes-to-buffer (bytes buffer)
  "Removes exising content in BUFFER and inserts the BYTES list into it."
  (with-buffer buffer
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
    (list (calculate-n-ary 128 bytes) (length bytes))))

(provide 'mb-utils-io)
