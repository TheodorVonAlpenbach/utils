;;; mb-insert.el --- Personal insertion commands -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;; Commands for inserting dates, paired delimiters, repeated symbols, line
;; prefixes, underlines, and provide forms.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mb-things)
(require 'mb-utils-buffer)
(require 'mb-utils-iso-time)

(defvar mb-default-underline-pattern "="
  "Pattern used by `insert-underline' when the requested pattern is empty.")

(cl-defun insert-parentheses*
    (arg &key (left ?\() (right ?\)) (ensure-space :around))
  "Enclose ARG sexps between LEFT and RIGHT.
Leave point after LEFT.  A negative ARG encloses preceding sexps; nil
or zero inserts an empty pair.  ENSURE-SPACE may be `:around',
`:before', `:after', or `:none'."
  (setq arg (prefix-numeric-value (or arg 0)))
  (cond
   ((> arg 0)
    (skip-chars-forward " \t"))
   ((< arg 0)
    (forward-sexp arg)
    (setq arg (- arg))))
  (when (and (memq ensure-space '(:before :around))
             (not (bobp))
             (memq (char-syntax (preceding-char)) '(?w ?_ ?\))))
    (insert " "))
  (insert left)
  (save-excursion
    (unless (zerop arg)
      (forward-sexp arg))
    (insert right)
    (when (and (memq ensure-space '(:after :around))
               (not (eobp))
               (memq (char-syntax (following-char)) '(?w ?_ ?\()))
      (insert " "))))

(defun insert-date (prefix)
  "Insert today's ISO date.
With PREFIX, insert date and time; a numeric prefix of 3 includes
seconds."
  (interactive "P")
  (insert
   (if prefix
       (iso-date-and-time
        :with-seconds (and (numberp prefix) (= prefix 3)))
     (iso-date))))

(defun insert-time (prefix)
  "Insert the current ISO time.
A numeric PREFIX of 3 includes seconds."
  (interactive "P")
  (insert (iso-time :with-seconds
                    (and (numberp prefix) (= prefix 3)))))

(defun insert-date-and-time (&optional prefix)
  "Insert the current ISO date and time.
A numeric PREFIX of 3 includes seconds."
  (interactive "P")
  (insert (iso-date-and-time
           :with-seconds (and (numberp prefix) (= prefix 3)))))

(defun insert-full-date ()
  "Insert today's full date."
  (interactive)
  (insert (full-date)))

(defun insert-todo-item ()
  "Insert a dated item at the beginning of the current line."
  (interactive)
  (beginning-of-line)
  (insert (format "* %s \n" (short-date)))
  (end-of-line 0))

(defun iso-to-short-date-at-point ()
  "Replace the ISO date at point with its short representation."
  (interactive)
  (let ((date (date-at-point t))
        (bounds (thing-at-point-bounds-of-date t)))
    (unless (and date bounds)
      (user-error "No ISO date at point"))
    (overwrite-region (short-date date)
                      (cl-first bounds) (cl-second bounds))))

(defun kill-sexp* (count)
  "Kill COUNT sexps before point and return the killed text."
  (backward-kill-sexp count)
  (current-kill 0 t))

(defun insert-quotes (count)
  "Insert a backtick/apostrophe pair around COUNT sexps."
  (interactive "P")
  (insert-parentheses* count :left ?` :right ?'))

(defun insert-single-quotes (count)
  "Insert single quotes around COUNT sexps."
  (interactive "P")
  (insert-parentheses* count :left ?' :right ?'))

(defun insert-double-quotes (count)
  "Insert double quotes around COUNT sexps."
  (interactive "P")
  (insert-parentheses* count :left ?\" :right ?\"))

(defun insert-asterisks (count)
  "Insert asterisks around COUNT sexps."
  (interactive "P")
  (insert-parentheses* count :left ?* :right ?*))

(defun insert-guillemets (count)
  "Insert guillemets around COUNT sexps."
  (interactive "P")
  (insert-parentheses* count :left ?« :right ?»))

(defun insert-square-brackets (count)
  "Insert square brackets around COUNT sexps."
  (interactive "P")
  (insert-parentheses* count :left ?\[ :right ?\] :ensure-space :after))

(defun insert-curly-brackets (count)
  "Insert curly brackets around COUNT sexps."
  (interactive "P")
  (insert-parentheses* count :left ?{ :right ?}))

(defun insert-angle-brackets (count)
  "Insert angle brackets around COUNT sexps."
  (interactive "P")
  (insert-parentheses* count :left ?< :right ?>))

(defun insert-dollar-brackets (count)
  "Insert dollar signs around COUNT sexps."
  (interactive "P")
  (insert-parentheses* count :left ?$ :right ?$))

(defun insert-xml-tag (count tag)
  "Insert <TAG></TAG> around COUNT sexps."
  (interactive "P\nsTag: ")
  (insert-parentheses* count
                       :left (format "<%s>" tag)
                       :right (format "</%s>" tag)))

(defun mb-insert-repeat (string &optional count)
  "Insert STRING COUNT times, defaulting to once."
  (dotimes (_ (max 0 (or count 1)))
    (insert string)))

(defun insert-skip-quote (&optional count)
  "Insert COUNT omission markers, defaulting to one."
  (interactive "p")
  (mb-insert-repeat "[...]" count))

(defun insert-post-scriptum ()
  "Insert post scriptum and deinde scriptum delimiters."
  (interactive)
  (insert "\nP.S. \nD.S.")
  (backward-char 5))

(defun insert-n (object)
  "Return an interactive command that inserts OBJECT repeatedly.
OBJECT is converted with `format' using the %s directive."
  (let ((string (format "%s" object)))
    (lambda (&optional count)
      (interactive "p")
      (mb-insert-repeat string count))))

(defun insert-symbol-sup1 (&optional count)
  "Insert COUNT superscript ones."
  (interactive "p")
  (mb-insert-repeat "¹" count))

(defun insert-symbol-sup2 (&optional count)
  "Insert COUNT superscript twos."
  (interactive "p")
  (mb-insert-repeat "²" count))

(defun insert-symbol-sup3 (&optional count)
  "Insert COUNT superscript threes."
  (interactive "p")
  (mb-insert-repeat "³" count))

(defun insert-symbol-deg (&optional count)
  "Insert COUNT degree signs."
  (interactive "p")
  (mb-insert-repeat "°" count))

(defun insert-symbol-ordm (&optional count)
  "Insert COUNT masculine ordinal indicators."
  (interactive "p")
  (mb-insert-repeat "º" count))

(defun insert-symbol-ordf (&optional count)
  "Insert COUNT feminine ordinal indicators."
  (interactive "p")
  (mb-insert-repeat "ª" count))

(defun insert-symbol-macr (&optional count)
  "Insert COUNT spacing macrons."
  (interactive "p")
  (mb-insert-repeat "¯" count))

(defun insert-symbol-bullet (&optional count without-space-p)
  "Insert COUNT bullets.
Unless WITHOUT-SPACE-P is non-nil, ensure one following space."
  (interactive "p")
  (mb-insert-repeat "·" count)
  (unless without-space-p
    (just-one-space)))

(defun insert-german-double-s (&optional count)
  "Insert COUNT German sharp-s characters."
  (interactive "p")
  (mb-insert-repeat "ß" count))

(defun insert-c-cedilla (&optional count)
  "Insert COUNT lowercase c-cedilla characters."
  (interactive "p")
  (mb-insert-repeat "ç" count))

(defun insert-1/2 (&optional count)
  "Insert COUNT one-half characters."
  (interactive "p")
  (mb-insert-repeat "½" count))

(defun insert-underline (pattern)
  "Underline the current line using repetitions of PATTERN.
An empty PATTERN uses `mb-default-underline-pattern'."
  (interactive "*sUnderline pattern: ")
  (let* ((length (last-column))
         (unit (if (string-empty-p pattern)
                   mb-default-underline-pattern
                 pattern))
         (line unit))
    (while (< (length line) length)
      (setq line (concat line line)))
    (newline)
    (insert (substring line 0 length))))

(defun insert-prefix-region (beg end &optional prefix)
  "Insert PREFIX at the beginning of every line from BEG to END.
PREFIX defaults to \"> \"."
  (interactive "*r\nsPrefix: ")
  (let ((text (if (or (null prefix) (string-empty-p prefix))
                  "> "
                prefix))
        (end-marker (copy-marker end)))
    (unwind-protect
        (save-excursion
          (goto-char beg)
          (beginning-of-line)
          (while (< (point) end-marker)
            (insert text)
            (forward-line 1)))
      (set-marker end-marker nil))))

(defun insert-object (object)
  "Insert OBJECT using its %s representation."
  (insert (format "%s" object)))

(defun ins (&rest objects)
  "Insert each object in OBJECTS using `insert-object'."
  (dolist (object objects)
    (insert-object object)))

(defun insert-expression (expression)
  "Insert the value of EXPRESSION at point."
  (interactive (list (eval-minibuffer "Insert expression: ")))
  (ins expression))

(defun insert-provide ()
  "Append a provide form for the current file or buffer."
  (interactive)
  (let ((feature
         (file-name-sans-extension
          (file-name-nondirectory
           (or buffer-file-name (buffer-name))))))
    (save-excursion
      (goto-char (point-max))
      (delete-blank-lines)
      (unless (bolp)
        (newline))
      (insert (format "(provide '%s)\n" feature)))))

(provide 'mb-insert)

;;; mb-insert.el ends here
