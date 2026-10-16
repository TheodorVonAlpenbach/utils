(require 'mb-utils-strings)

;;;; Token stream methods
(cl-defun ts-read (token-stream &optional n reverse)
  "Reads N tokens from TOKEN-STREAM and returns the N-th.
If reverse if non nil, then it returns the first token"
  (pop* (second token-stream) (or n 1) reverse))

(cl-defun ts-read-list (token-stream n)
  "Reads N tokens from TOKEN-STREAM and returns them as a list,
preserving order."
  (pop (second token-stream) (or n 1)))

(cl-defun ts-peak (token-stream &optional n)
  "Returns the Nth next token in token-stream. Does not alter the stream."
  (nth (or n 0) (second token-stream)))

(cl-defun ts-size (token-stream)
  "Returns the size of the TOKEN-STREAM,
i.e. how many tokens left to read."
  (length (second token-stream)))

(cl-defun ts-eof (token-stream)
  "Returns nil iff token-stream is not empty (size is zero)"
  (zerop (ts-size token-stream)))

(cl-defun ts-source (token-stream)
  "Returns the name of the buffer where the Lilypond code was parsed"
  (first token-stream))

(cl-defun ts-push-list (token-stream list)
  "Inserts LIST at the beginning of TOKEN-STREAM"
  (setf (second token-stream)  (append list (second token-stream))))

(cl-defun ts-parse (buffer)
  "Parses Lilypond BUFFER and creates a tokens stream object (TS).
A token stream is actually a list (buffer-name token-list), but
it should only be used with the specialized methods
`ts-read',`ts-read-list', `ts-peak', `ts-size', `ts-eof',
`ts-source'"
  (with-buffer buffer
    (list (buffer-name buffer)
	  (ly-read-tokens))))
;;;; End Token stream methods

(cl-defun ly-read-token ()
  "Reads and returns next token(s) . 
Can't handle {ees, <<aes and similar constructs."
  (condition-case err
      (if (eq (char-after) ?\")
	(format "\"%s\"" (read (current-buffer)))
	(let* ((beg (point))
	       (end (re-search-forward "\\S-\\s-"))
	       (token (buffer-substring-no-properties beg end)))
	  (string-trim token)))
    (error nil)))
  
(cl-defun ly-read-tokens ()
  (save-excursion
    (goto-char (point-min))
    ;(perform-replace "\\" "\\\\" nil nil nil)
    (goto-char (point-min))
    (let ((token (ly-read-token))
	  (tokens ()))
      (while token
	(if (listp token)
	  (append tokens token)
	  (push-back token tokens))
	(setq token (ly-read-token)))
      tokens)))

(cl-defun ly-parse-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (ly-parse-buffer (current-buffer))))

(cl-defun ly-parse-buffer (&optional buffer)
  (ly-parse (ts-parse (or buffer (current-buffer)))))

(cl-defun ly-top-level-token-type (token)
  (cond ((equal token "{") :list-begin)
	((equal token "<<") :simultaneous-music-begin)
	((equal (substring token 0 1) "\\") :function)
	((string-match-exact "[[:alnum:]]+" token) :identifier)))

(cl-defun ly-token-type (token)
  (cond ((equal token "|") :bar)
	((equal token "{") :list-begin)
	((equal token "}") :list-end)
	((equal token "<<") :simultaneous-music-begin)
	((equal token ">>") :simultaneous-music-end)
	((equal token "//") :simultaneous-music-split)
	((equal (substring token 0 1) "\\") :function)
	((equal (substring token 0 1) "\"") :string)
	(t :note)))

;;;; Lilypond Lisp functions
(cl-defun ly-version (string) 
  "Returns Lilypond version number tag"
  string)

(cl-defun ly-bar (string) 
  "Ignored"
  nil)

(cl-defun ly-clef (string) 
  "Ignored"
  nil)

(cl-defun chb-octave-difference (pbc1 pbc2)
  (let ((chb-difference (chb- pbc2 pbc1)))
    (if (< chb-difference -3)
      1
      (if (< chb-difference 4)
	0 -1))))
;;(chb-octave-difference (chb-from-string "G") (chb-from-string "D"))

(cl-defun ly-absolute-pitch (pitch reference-pitch)
  "Returns the corresponding absolute pitch of a relative PITCH from RELATIVE-PITCH.

For converting to absolute pitch the method modifies PITCH's
octave value according to the following criteria:

1. the direction of the ordered interval (OI) is chosen so the
interval is as short as possible

2. an additional N number of '-s or ,-s increases the octave
value by N or -N respectively."
  (let ((absolute-pitch (copy-pitch pitch)))
    (cl-incf (p-octave absolute-pitch)
	  (+ (p-octave reference-pitch)
	     (chb-octave-difference (chrome-base (p-chrome reference-pitch))
				    (chrome-base (p-chrome pitch)))
	     -3)) ;; -3 is because the octave of a parsed c (ie. without , or ') has the value 3
    absolute-pitch))

(cl-defun ly-convert-relative-pitches-to-absolute (tree reference-note)
  "Traversing TREE only looking at note structs and lists (as they could be containing note structs),
it modifies the the next note in in the traversal order gets its
octave modified as described in `ly-toggle-relative-octaves'.
Each modified note serves as the relative note for the next
visited note struct. For optional argument RELATIVE-NOTE, see `ly-toggle-octave-mode'."
  (let ((notes (copy-if #'note-p (flatten tree))))
    (cl-loop for n in-ref notes
	  for reference-pitch = (n-pitch reference-note) then abs
	  for abs = (ly-absolute-pitch (n-pitch n) reference-pitch)
	  do (setf (n-pitch n) abs))
    tree))

(cl-defun ly-relative (reference-note music) 
  "Interprets MUSIC relative to REFERENCE-NOTE.
See `ly-convert-relative-pitches-to-absolute' for more information"
  (ly-convert-relative-pitches-to-absolute music reference-note))

(cl-defun ly-time (ly-ts-string)
  (ts-from-string ly-ts-string 'lilypond))

(cl-defun ly-major () "major")
(cl-defun ly-minor () "minor")

(cl-defun ly-key (ly-chrome-string ly-mode-music)
  (k-from-strings ly-chrome-string ly-mode-music 'lilypond))
;;()

(cl-defun ly-new (context &optional name)
  nil)

(cl-defun ly-set (assignment)
  (list 'set (first assignment) (ly-eval-markup (second assignment))))

(cl-defun ly-eval-markup (markup)
  markup)

(cl-defun ly-markup (list)
  "TODO: should handle string as well as list"
  (cons 'markup list))

(cl-defun ly-score (music)
  "Music should be a list of voices"
  (list 'score music))

(cl-defun ly-lisp-function (ly-function)
  (or (intern-soft (concat "ly-" (substring ly-function 1)))
      (error "Lisp function for lilypond function %S does not exist!" ly-function)))
;;(funcall (ly-lisp-function "\\version") 1)

(cl-defun ly-read-list (token-stream list-delimiters)
  (let ((token-list (list (ts-read token-stream)))
	(delimiter-count 1)
	(token nil))
    (while (> delimiter-count 0)
      (setq token (ts-read token-stream))
      (when (equal token (first list-delimiters))
	(cl-incf delimiter-count))
      (when (equal token (second list-delimiters))
	(decf delimiter-count))
      (push token token-list))
    (reverse token-list)))

(cl-defun ly-read-element (token-stream variables)
  (cl-case (ly-token-type (ts-peak token-stream))
    (:list-begin (ly-read-list token-stream '("{" "}")))
    (:simultaneous-music-begin (ly-read-list token-stream '("<<" ">>")))
    (t (list (ts-read token-stream)))))

(cl-defun ly-read-function (token-stream variables)
  "Can't handle optional arguments"
  (let* ((function-token (ts-read token-stream))
	 (function (ly-lisp-function function-token))
	 (args (function-args function nil)))
    (cons function-token
	  (cl-loop for i below (length args)
		append (ly-read-element token-stream variables)))))

(cl-defun ly-read-variable-definition (token-stream variables)
  "Reads and returns the tokens that represents the defined variable's value
E.g. for the definition
foo = | { list-element-1 list-element-2 } ('|' represents the stream head)
the method returns the (list '{ 'list-element-1 'list-element-2 '}"
  (let ((variable-name (ts-read token-stream 2 t))
	(value-tokens (if (eq (ly-token-type (ts-peak token-stream)) :function)
			(ly-read-function token-stream variables)
			(ly-read-element token-stream variables))))
    (list variable-name value-tokens)))

(cl-defun ly-read-context (token-stream variables contexts)
  "Read and skip context"
  (if (equal (ts-peak token-stream 1) "=")
    (ly-read-variable-definition token-stream variables)
    (ts-read token-stream)))

(cl-defun ly-eval-list (token-stream variables contexts)
  (if (string/= (ts-peak token-stream) "{")
    (error "Expected '{', not '%s'" (ts-peak token-stream))
    (ts-read token-stream))
  (cl-loop for next-token = (ts-peak token-stream)
	while (string/= (ts-peak token-stream) "}")
	collect (ly-eval-music token-stream variables contexts)
	finally (ts-read token-stream))) ;;pop final '}'

(cl-defun ly-eval-sequential-music (token-stream variables contexts)
  (when (neq (ly-token-type (ts-read token-stream)) :list-begin)
    (error "Expected start of sequential-music" ))
  (let ((elements ()))
    (cl-loop for type = (ly-token-type (ts-peak token-stream))
	  for res = (ly-eval-music token-stream variables contexts)
	  while (neq res :list-end)
	  if res
	  if (or (and (listp res) (neq (first res) 'set))
		 (eq type :list-begin)) 
	  append res into ret
	  else 
	  collect res into ret
	  finally return ret)))

(cl-defun ly-eval-simultaneous-music (token-stream variables contexts)
  "Does not support the '//' construct"
  (when (neq (ly-token-type (ts-read token-stream)) :simultaneous-music-begin)
    (error "Expected start of simultaneous-music" ))
  (let ((elements ()))
    (cl-loop for type = (ly-token-type (ts-peak token-stream))
	  for res = (ly-eval-music token-stream variables contexts)	  
	  while (neq res :simultaneous-music-end)
	  if res
	  if (eq type :simultaneous-music-begin) append res
	  else collect res)))

(cl-defun ly-eval-note (note-token variables contexts)
  (n-from-string note-token 'lilypond))

(cl-defun ly-eval-music (token-stream variables contexts)
  (cl-case (ly-token-type (ts-peak token-stream))
    (:list-begin (ly-eval-sequential-music token-stream variables contexts))
;    (:list-begin (ly-eval-list token-stream variables contexts))
    (:list-end (prog1 :list-end (ts-read token-stream)))
    (:simultaneous-music-begin (ly-eval-simultaneous-music token-stream variables contexts))
    (:simultaneous-music-end (prog1 :simultaneous-music-end (ts-read token-stream)))
    (:function (aif (ly-find-variable (ts-peak token-stream) variables)
		 (ly-insert-variable-and-eval it token-stream variables contexts)
		 (ly-eval-function token-stream variables contexts)))
    (:note (ly-eval-note (ts-read token-stream) variables contexts))
    ;;skip help bars
    (:string (ts-read token-stream))
    (:bar (ts-read token-stream))
    ;;else skip next element
    (t (progn (ly-eval-element token-stream variables contexts)
	      (ly-eval-music token-stream variables contexts)))))

(cl-defun ly-eval-typed-element (type token-stream variables contexts)
  (string-case (first (last (split-string (symbol-name type) "-")))
    ("music" (ly-eval-music token-stream variables contexts))
    ("list" (ly-eval-list token-stream variables contexts))
    ("function" (ly-eval-function token-stream variables contexts))
    ("string" (ts-read token-stream))
    ("number" (ts-read token-stream))
    ("note" (ly-eval-music token-stream variables contexts))
    ("context" (ly-read-context token-stream variables contexts))
    ("assignment" (ly-read-variable-definition token-stream variables))))

(cl-defun ly-eval-element (token-stream variables contexts)
  (cl-case (ly-token-type (ts-peak token-stream))
    (:list (ly-eval-list token-stream variables contexts))
    (:function (ly-eval-function token-stream variables contexts))
    (t (ts-read token-stream))))

(cl-defun ly-find-variable (name variables)
  (cl-find (substring (ts-peak token-stream) 1) variables :key #'first :test #'equal))

(cl-defun ly-insert-variable-and-eval (variable token-stream variables contexts)
  "Prepends the definition (ie. a list of tokens) of VARIABLE on
TOKEN-STREAM and continues evaluations of the modified stream."
  (ts-read token-stream) ;skip variable identifier
  (ts-push-list token-stream (second variable)) ;push variable values on stream
  (ly-eval-music token-stream variables contexts)) ;continue reading now modified stream

(cl-defun ly-eval-function (token-stream variables contexts)
  (let* ((function (ly-lisp-function (ts-read token-stream)))
	 (args (function-args function nil))
	 (opts (function-args function '&optional)))
    (when (and (= (length opts) 1)
	       (eq (ts-peak token-stream) :=))
      (push-back (pop* token 2) args))
    (apply function (mapcar #'(lambda (x) (ly-eval-typed-element x token-stream variables contexts)) args))))

(cl-defun ly-parse-top-level (token-stream)
  (let ((variables)
	(contexts)
	(result))
    (while (not (ts-eof token-stream))
      (cl-case (ly-top-level-token-type (ts-peak token-stream))
	(:list (push (ly-eval-sequential-music token-stream variables contexts) result))
	(:<< (push (ly-eval-simultaneous-music token-stream variables contexts) result))
	(:function (push (ly-eval-function token-stream variables contexts) result))
	(:identifier (push (ly-read-variable-definition token-stream variables) variables))
	(otherwise (ts-read token-stream))))
    (nreverse result)))

(cl-defun ly-clean-note-tree (n-tree)
  (if (listp n-tree)
    (if (= (length n-tree) 1)
      (ly-clean-note-tree (first n-tree))
      (cl-loop for x in n-tree
	    collect (ly-clean-note-tree x)))
    n-tree))

(cl-defun ly-v-new (ly-voice v-reference)
  (let* ((notes-start-pos (position-if #'note-p ly-voice))
	 (meta-data (subseq ly-voice 0 notes-start-pos))
	 (ly-notes (and notes-start-pos
			(nthcdr notes-start-pos ly-voice)))
	 (ts)
	 (key)
	 (notes)
	 (instrument))
    (cl-loop for x in meta-data
	  do (cond 
	      ((and (listp x) 
		    (eq (first x) 'set)
		    (string-match "instrumentName" (second x))) 
	       (setf instrument (string-trim* (third x) "[#\"]")))

	      ((time-signature-p x) (setf ts x))

	      ((key-p x) (setf key x))))
    (setf notes
	  (cl-loop for x in ly-notes
		when (note-p x)
		collect x))
    (v-calculate-start-times
     (v-new notes 
	    (or ts (v-time-signature v-reference))
	    (or key (v-key v-reference))
	    instrument))))

(cl-defun ly-make-voice-group (ly-voices name)
  (let* ((v-reference (v-new))
	 (voices (if (listp (first ly-voices))
		   (cl-loop for ly-voice in ly-voices
			 for v = (ly-v-new ly-voice v-reference) then (ly-v-new ly-voice v)
			 if (v-notes v) collect v)
		   (ly-v-new ly-voices v-reference))))
    (vg-new (if (voice-p voices) 
	      (list voices) voices) 
	    name)))

(cl-defun ly-make-voice-groups (ly-top-level-objects)
  (cl-loop with last-markup = ""
	for o in ly-top-level-objects
	if (eq (first o) 'markup)
	do (setq last-markup (second o))
	if (eq (first o) 'score)
	collect (ly-make-voice-group (second o) (substring* last-markup 1 -1))))

(cl-defun ly-x-to-string (x)
  (cl-case (struct-type x)
    (note (n-to-string x))
    (key (k-to-string x))
    (time-signature (ts-to-string x))))

;;clean up stuff
(cl-defun ly-fill-durations (tree last-duration)
  "only fills duration so far"
  (if (listp tree)
    (cl-loop for x in tree
	  for duration = (ly-fill-durations x last-duration) then (ly-fill-durations x duration)
	  finally return duration)
    (if (note-p tree)
      (or (n-duration tree)
	  (setf (n-duration tree) (copy-duration last-duration)))
      last-duration)))

(cl-defun ly-parse (token-stream)
  (let* ((top-level-objects (copy-if #'listp (ly-parse-top-level token-stream)))
	 (tree (ly-clean-note-tree top-level-objects)))
    (ly-fill-durations tree (make-duration))
    (ly-make-voice-groups tree)))

(provide 'lilypond-parser)
;;(v-to-string (first (ly-parse-file "c:/documents and settings/matsb/my documents/projects/uio/project/bach-chorals/lilypond/cp.ly")))
