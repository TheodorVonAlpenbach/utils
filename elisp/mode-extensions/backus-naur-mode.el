;;; backus-naur-mode.el --- Edit Backus-Naur grammars -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'imenu)
(require 'ring)
(require 'subr-x)

(defvar bn-tab-width 2
  "Tab width in backus-naur-mode")

(defvar bn-left-nonterminal-definition-old
  "^\\([A-Za-z-]*\\)\\(:\\)[ ]\\{0,3\\}$"
  "The regular expression that marks the left side from the \
definition of a rule" )

(defconst bn-assignment "::=")
(defconst bn-lhs-token "[A-Za-z-]*")
(defconst bn-left-nonterminal-definition
  (format "<\\(%s\\)>\\s-*\\(%s\\)" bn-lhs-token bn-assignment)
  "The regular expression that marks the left side from the \
definition of a rule" )
;  ^<\([A-Za-z-]*\)>\s-*\(::=\)

(defvar imenu-create-index-function)
(defvar imenu-use-markers)

(defvar bn-left-nonterminal-list nil
  "keeps the list of all the left nonterminals. this could be
defined locally only, inside `bn-lazy-font-lock'. Used for
imenu and completion-list .")

(defvar bn-timer-var nil
  "non nil means that a fontification will follow")

(defvar bn-search-history nil
  "used by search-completion to keep the history of searched
  strings" )

(defvar bn-search-overlays nil
  "Overlays used by the active nonterminal search.")

(defvar bn-mode-hook nil
  "Hook run after entering `bn-mode'.")

(defvar bn-comment-char "%"
  "a line which starts with this character is commented")

(cl-defun bn-mode nil
  "A major mode for well writing context-free grammars in
Backus-Naur notation."
  (interactive)
  'bn-mode
  'fundamental-mode
  '"Backus-Naur"
  (kill-all-local-variables)
  (setq major-mode 'bn-mode
	mode-name "Backus-Naur" )
  (setq-local tab-width bn-tab-width)
  (setq-local indent-region-function #'bn-indent-region)
  ;;(setq indent-region-function nil)

  (make-local-variable 'bn-comment-char)
  (make-local-variable 'after-change-functions)
  (make-local-variable 'bn-left-nonterminal-list)
  (make-local-variable 'bn-search-overlays)

  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'bn-imenu)

  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression t)

  (and t
       (fboundp 'imenu-add-to-menubar)
       (imenu-add-to-menubar "Index"))

  (setq font-lock-defaults
	(list
	 (list
	  (list
	   bn-left-nonterminal-definition
	   '(1 'bn-left-nonterminal-definition-font )
	   '(2 'bn-left-nonterminal-definition-closure ) )
	  '( "\\('\\)\\([[:graph:]]*\\)\\('\\)"
	     (1 'bn-terminal-closure )
	     (2 'bn-terminal-definition )
	     (3 'bn-terminal-closure )
	     )
	  '( "\\(@\\)\\([[:graph:]]+\\)"
	     (1 'bn-terminal-closure )
	     (2 'bn-terminal-definition )
	     )
          (list
	   (concat "[^[:graph:]]"
		   "\\([[]\\)"
		   "\\(.*\\)"
		   "[^[:graph:]]"
		   "\\([]]\\)")
	   '(1 'bn-optional-closure )
	   '(2 'bn-optional-definition )
	   '(3 'bn-optional-closure ))
          (list
	   (concat "^" "\\(" bn-comment-char "\\)"
                   "\\(.*\\)" )
	   '(1 'bn-comment-sign )
	   '(2 'bn-commented-text ) )
	  (list
	   "|"
	   '(0 'bn-or ) ) )
	 t ) )

  (bn-stop-timer)
  (bn-clear-search-overlays)
  (add-hook 'after-change-functions #'bn-change nil t)
  (add-hook 'kill-buffer-hook #'bn-stop-timer nil t)
  (local-set-key [tab] 'bn-indent)
  (local-set-key [home]
		 (lambda nil (interactive)
		   (if (equal (char-before) ?\n)
		       (bn-beginning-of-defun)
		     (beginning-of-line) ) ) )
  (local-set-key [end]
		 (lambda nil (interactive)
		   (if (equal (char-after) ?\n)
		       (bn-end-of-defun)
		     (end-of-line) ) ) )
  (bn-lazy-font-lock)
  (local-set-key "\M-s" 'bn-search )
  (local-set-key (kbd "C-x SPC")
		 (lambda ()
		   "jump to the definition of the nonterminal under the cursor. "
                   (interactive "")
		   (let* ((type (get-text-property (point) 'type))
			  (nonterminal
			   (and type
				(get-text-property (point) 'nonterminal) ) )
			  (p
			   (and nonterminal
			    (get-text-property (point) 'position) ) ) )
		     (cond
		      ( (eq type 'right)
			(and p
			     (bn-jump p )
			     '(recenter)
			     (message "jump to `%s' %s" nonterminal p ) ) )
		      ((eq type 'left)
			   (error "`%s' is a left nonterminal\n" nonterminal ) ) ) ) ) )

  (make-local-variable 'bn-search-history)

  (local-set-key (kbd "C-;")
                 (lambda nil
                   "replaces isearch"
                   (interactive)
                   (let ((aa
                          (completing-read
                           "search left-nonterminal: "
                           (mapcar 'caar bn-left-nonterminal-list )
                           nil
                           t
                           nil
                           bn-search-history
                           nil
                           nil ) ) )
                     (and aa
                          (bn-jump (cdr (assoc aa (mapcar 'car bn-left-nonterminal-list ) ) ) ) ) ) ) )
  (run-mode-hooks 'bn-mode-hook)
  )

(defalias 'backus-naur-mode 'bn-mode)

(cl-defun bn-jump (p)
  (push-mark)
  (goto-char p) )

(defun bn-clear-search-overlays ()
  "Delete overlays created by `bn-search' in the current buffer."
  (mapc #'delete-overlay bn-search-overlays)
  (setq bn-search-overlays nil))

(defun bn-mouse-jump-to-definition (event)
  "Jump to the nonterminal definition associated with mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (let ((position (get-text-property (point) 'position)))
    (unless (integer-or-marker-p position)
      (user-error "No nonterminal definition at mouse position"))
    (bn-jump position)))

(cl-defun bn-stop-timer ()
  "stop the timer for fontification"
  (and bn-timer-var
       (progn
	 (cancel-timer bn-timer-var)
	 (setq bn-timer-var nil) ) ) )

(defface bn-left-nonterminal-definition-font
  '((((class grayscale) (background light)) (:foreground "Gray90" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "red"))
    (((class color) (min-colors 88) (background dark)) (:foreground "orange red"))
    (((class color) (min-colors 16) (background light)) (:foreground "orange"))
    (((class color) (min-colors 16) (background dark)) (:foreground "gold"))
    (((class color) (min-colors 8)) (:foreground "dark orange"))
    (t (:weight bold :underline t)))
  "font used for left nonterminals." )

(defface bn-right-nonterminal-definition-font
  '((((class grayscale) (background light)) (:foreground "AntiqueWhite4" :weight bold))
    (((class grayscale) (background dark)) (:foreground "AntiqueWhite4" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "AntiqueWhite4"))
    (((class color) (min-colors 88) (background dark)) (:foreground "AntiqueWhite4"))
    (((class color) (min-colors 16) (background light)) (:foreground "AntiqueWhite4"))
    (((class color) (min-colors 16) (background dark)) (:foreground "AntiqueWhite4"))
    (((class color) (min-colors 8)) (:foreground "AntiqueWhite4"))
    (t (:weight bold :underline t)))
  "font used for right nonterminals." )

(defface bn-left-nonterminal-definition-closure
  '((((class grayscale) (background light)) (:foreground "Gray90" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "deep sky blue"))
    (((class color) (min-colors 88) (background dark)) (:foreground "spring green"))
    (((class color) (min-colors 16) (background light)) (:foreground "dark sea green"))
    (((class color) (min-colors 16) (background dark)) (:foreground "yellow green"))
    (((class color) (min-colors 8)) (:foreground "olive drab"))
    (t (:weight bold :underline t)))
  "font used for the character `:' at the end of a left nonterminal." )

(defface bn-terminal-definition
  '((((class grayscale) (background light)) (:foreground "DeepSkyBlue1" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DeepSkyBlue1" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "DeepSkyBlue1"))
    (((class color) (min-colors 88) (background dark)) (:foreground "DeepSkyBlue1"))
    (((class color) (min-colors 16) (background light)) (:foreground "DeepSkyBlue1"))
    (((class color) (min-colors 16) (background dark)) (:foreground "DeepSkyBlue1"))
    (((class color) (min-colors 8)) (:foreground "DeepSkyBlue1"))
    (t (:weight bold :underline t)))
  "font used for terminals." )

(defface bn-terminal-closure
  '((((class grayscale) (background light)) (:foreground "dark blue" :weight bold))
    (((class grayscale) (background dark)) (:foreground "dark blue" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "dark blue"))
    (((class color) (min-colors 88) (background dark)) (:foreground "dark blue"))
    (((class color) (min-colors 16) (background light)) (:foreground "dark blue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "dark blue"))
    (((class color) (min-colors 8)) (:foreground "dark blue"))
    (t (:weight bold :underline t)))
  "font used for the characters \' around a terminal." )

(defface bn-optional-closure
  '((((class grayscale) (background light)) (:foreground "RoyalBlue1" :weight bold))
    (((class grayscale) (background dark)) (:foreground "RoyalBlue1" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "RoyalBlue1"))
    (((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))
    (((class color) (min-colors 16) (background light)) (:foreground "RoyalBlue1"))
    (((class color) (min-colors 16) (background dark)) (:foreground "RoyalBlue1"))
    (((class color) (min-colors 8)) (:foreground "RoyalBlue1"))
    (t (:weight bold :underline t)))
  "font used for the characters [ ] about an optional ." )

(defface bn-optional-definition
  '((((class grayscale) (background light)) (:foreground "dark green" :weight bold))
    (((class grayscale) (background dark)) (:foreground "dark green" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "dark green"))
    (((class color) (min-colors 88) (background dark)) (:foreground "dark green"))
    (((class color) (min-colors 16) (background light)) (:foreground "dark green"))
    (((class color) (min-colors 16) (background dark)) (:foreground "dark green"))
    (((class color) (min-colors 8)) (:foreground "dark green"))
    (t (:weight bold :underline t)))
  "font used for the ." )

(defface bn-search-rotate-face
  '((((class grayscale) (background light)) (:foreground "Gray90" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "chartreuse1"))
    (((class color) (min-colors 88) (background dark)) (:foreground "chartreuse2"))
    (((class color) (min-colors 16) (background light)) (:foreground "chartreuse3"))
    (((class color) (min-colors 16) (background dark)) (:foreground "chartreuse4"))
    (((class color) (min-colors 8)) (:foreground "SpringGreen4"))
    (t (:weight bold :underline t)))
  "font used for matched right nonterminals." )

(defface bn-commented-text
  '((((class grayscale) (background light)) (:foreground "medium sea green" :weight bold))
    (((class grayscale) (background dark)) (:foreground "medium sea green" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "medium sea green"))
    (((class color) (min-colors 88) (background dark)) (:foreground "medium sea green"))
    (((class color) (min-colors 16) (background light)) (:foreground "medium sea green"))
    (((class color) (min-colors 16) (background dark)) (:foreground "medium sea green"))
    (((class color) (min-colors 8)) (:foreground "medium sea green"))
    (t (:weight bold :underline t)))
  "font used for the text on a commented line" )

(defface bn-or
  '((((class grayscale) (background light)) (:foreground "brown" :weight bold))
    (((class grayscale) (background dark)) (:foreground "blue" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "blue"))
    (((class color) (min-colors 88) (background dark)) (:foreground "blue"))
    (((class color) (min-colors 16) (background light)) (:foreground "blue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "blue"))
    (((class color) (min-colors 8)) (:foreground "blue"))
    (t (:weight bold :underline t :foreground "blue" :weight bold)))
  "font used for" )

(defface bn-comment-sign
  '((((class grayscale) (background light)) (:foreground "Gray90" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "indian red"))
    (((class color) (min-colors 88) (background dark)) (:foreground "lightgreen"))
    (((class color) (min-colors 16) (background light)) (:foreground "tomato"))
    (((class color) (min-colors 16) (background dark)) (:foreground "dark violet"))
    (((class color) (min-colors 8)) (:foreground "sienna"))
    (t (:weight bold :underline t)))
  "font used for the character that comments a line" )

(cl-defun bn-indent-old ()
  " The lines that contain a left nonterminal are indented from
  0. These are the lines that match the regular expression
 `bn-left-nonterminal-definition'.
The blank lines are cleared.
The lines which contain a rule are indented with a tab."
  (interactive)
  (let (
	;; initial point
	(cc (current-column))
	;; end of line
	(eol (prog2
		 (move-end-of-line nil)
		 (current-column) ) )
	;; beginning of text
	(bot (prog2
		 (move-beginning-of-line nil)
		 (skip-chars-forward "[:blank:]")
		 (current-column) ) ) )
    (cond
     ;; empty line
     ((zerop eol)
      t)
     ;; lines with blanks
     ((eq bot eol)
      (move-beginning-of-line nil)
      (just-one-space 0) )
     ;; line containing a left nonterminal
     ((save-excursion
	(move-to-column eol)
	(and (> (current-column) 3)
	     (search-backward ":" (- (point) 1) t ) ) )
      (move-beginning-of-line nil)
      (just-one-space 0) )
     ;; for the rest... add a tab at the beginning of the line
     ( (save-excursion
	 (move-beginning-of-line nil)
	 (not (search-forward-regexp "\t[^[:blank:]]" (+ 2 (point) ) t ) ) )
       (just-one-space 0)
       (insert-char ?\t 1)
       (skip-chars-forward "[:blank:]") ) )
    ;; if the cursor was before the indetation inside the word, keep
    ;; it on the same position; if the initial position of the cursor
    ;; was before the word, move it at the beginning of the word
    (move-to-column (+ (current-column)
		       (max 0 (- cc bot) ) ) ) ) )

(cl-defun bn-commented-line-p ()
  "return t when the point is inside a commented line, and nil
otherwise."
  (equal
   (save-excursion
     (beginning-of-line)
     (following-char) )
   (elt bn-comment-char 0 ) ) )

(cl-defun bn-lazy-font-lock ()
  "called after every buffer change to re-fontify the text."
  (bn-stop-timer)
  (save-excursion
    (let (list-of-left-nonterminals
          make-fontification
	  (map-right-nonterminal (make-sparse-keymap) )
	  (map-left-nonterminal (make-sparse-keymap) )
	  ;; font lock properties modify the buffer, so this hook is
	  ;; called recursively. temporarly after-change-functions
	  ;; must be nil.
	  after-change-functions
	  ;; this is equalent in this case to setting
	  ;; after-change-functions to nil. However, we keep both
	  (inhibit-modification-hooks t))
      (with-silent-modifications
      (define-key map-right-nonterminal [mouse-3]
        #'bn-mouse-jump-to-definition)
      (define-key map-left-nonterminal "\M-s" 'bn-search )
      (goto-char (point-min) )
      ;; make the list of left nonterminals out
      (while (search-forward-regexp
              ;; TODO: to add fontification here for optional
              ;; nonterminals inside `[ ... ]'
              (concat bn-left-nonterminal-definition )
              nil t)
	(push (cons (substring-no-properties (match-string 1) )
		    (match-beginning 1) )
	      list-of-left-nonterminals ) )
      ;; clean the previous fontification
      (remove-list-of-text-properties
       (point-min) (point-max)
       '(mouse-face font-lock-face keymap position help-echo
         type nonterminal))
      (setq bn-left-nonterminal-list nil)
      ;; add first the fontification of font-lock
      (font-lock-flush)
      (font-lock-ensure)
      ;; scan the buffer to find the positions where every
      ;; left-nonterminal appears in the rules
      (dolist (m list-of-left-nonterminals)
	(let ( (match-positions) )
	  (goto-char (point-min) )
	  (while
	      (and (search-forward-regexp
		    (concat "[\t ]" "\\(" (car m) "\\)" "[\n .\t]") nil t)
		   (goto-char (1- (point) ) ) )
	    (setq make-fontification
		  (not (eq (get-text-property (1- (point)) 'font-lock-face)
			   'bn-optional-definition)))
	    (and
	     ;; if a right nonterminal is found, add it to the
	     ;; matching list, subject to not being on a commented line
	     ;; or inside a [ comment ]
	     make-fontification
	     (not (bn-commented-line-p) )
	     (add-text-properties
	      (match-beginning 1)
	      (match-end 1)
	      (list
	       'font-lock-face 'bn-right-nonterminal-definition-font)))

	    (and
	     (not (bn-commented-line-p) )
	     (add-text-properties
	      (match-beginning 1)
	      (match-end 1)
	      (list 'mouse-face 'highlight
		    'help-echo
		    (concat (car m)
                            (format " at %d." (cdr m) )
                            (let ((nl (count-lines (point) (cdr m) ) ) )
                              (cond ( (> (point) (cdr m) )
                                      (format " %d line%s backward."
                                              (1- nl)
                                              (if (equal 2 nl)
                                                    ""
                                                "s" ) ) )
                                    ( t
                                      (format " %d line%s forward."
                                              nl
                                              (if (equal 1 nl)
                                                  ""
                                                "s" ) ) )
                                  ) )
                            "\nright click to jump there." )
		    'position (cdr m)
		    'type 'right
		    'nonterminal (car m)
		    'keymap map-right-nonterminal) )
	     (push (match-beginning 1) match-positions) ) )
	  ;; add text properties to the left nonterminal
	  (add-text-properties
	   (cdr m) (+ (length (car m) ) (cdr m) )
	   (list
	    'help-echo (concat "`" (car m) "'" " "
			       (if match-positions
				   (format "at %s" (reverse match-positions) )
				 "is a start symbol" )
			       (format
				".\n %s to rotate around matches."
				(substitute-command-keys "\\[bn-search]") ) )
	    'position (progn
			(let ( ( ring-positions (make-ring (length match-positions) ) ) )
			  (dolist (p (reverse match-positions) )
			    (ring-insert ring-positions p) )
			  ring-positions) )
	    'nonterminal (car m)
	    'type 'left
	    'keymap map-left-nonterminal) )
	  (push (cons m match-positions) bn-left-nonterminal-list ) ) )) ) ))

(cl-defun bn-change (&rest _change)
  "hook which is called after every buffer change. it starts a
timer that fontify the current buffer, according to the rules
defined in the grammar. if a previous timer is active, stop it
before starting the new timer."
  ;; clear the overlays from the previous search if any
  (bn-stop-timer)
  '(setq overriding-terminal-local-map nil)
  (let ((buffer (current-buffer)))
    (setq bn-timer-var
          (run-with-idle-timer
           1 nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (derived-mode-p 'bn-mode)
                   (bn-lazy-font-lock)))))))))

(cl-defun bn-beginning-of-defun ()
  "jump at the beginning of a left nonterminal definition."
  (search-backward-regexp bn-left-nonterminal-definition nil t) )

(cl-defun bn-end-of-defun ()
  "jump at the end of a left nonterminal definition."
 (if (search-forward-regexp bn-left-nonterminal-definition nil t)
      (beginning-of-line)
   (bn-jump (point-max) ) )
  (let ((repeat t))
    (while repeat
      (skip-chars-backward " \t\n")
      (if (not (bn-commented-line-p) )
	  (setq repeat nil)
	(beginning-of-line) ) ) )
  (skip-chars-forward " \t")
  (and (not (eobp) )
       (forward-char) ) )

(cl-defun bn-search ()
  "Rotate through uses of the left nonterminal at point.
Press `n' and `p' to move between uses, and `q' to finish."
  (interactive)
  (let* ((nonterminal (get-text-property (point) 'nonterminal))
         (positions (and nonterminal
                         (get-text-property (point) 'position))))
    (unless (ring-p positions)
      (user-error "Point is not on a left nonterminal definition"))
    (if (ring-empty-p positions)
        (message "`%s' is a start symbol" nonterminal)
      (bn-clear-search-overlays)
      (dolist (position (ring-elements positions))
        (let ((overlay
               (make-overlay position
                             (+ position (length nonterminal)))))
          (overlay-put overlay 'face 'bn-search-rotate-face)
          (push overlay bn-search-overlays)))
      (let* ((index 0)
             (many-matches (> (ring-size positions) 1))
             (instructions
              (concat
               (when many-matches
                 "`n' next, `p' previous, ")
               "`q' finish"))
             key
             done)
        (unwind-protect
            (progn
              (bn-jump (ring-ref positions index))
              (while (not done)
                (message "Search for `%s': %s" nonterminal instructions)
                (setq key (read-key-sequence nil))
                (cond
                 ((equal key "q")
                  (setq done t))
                 ((and many-matches (equal key "n"))
                  (cl-decf index)
                  (goto-char (ring-ref positions index)))
                 ((and many-matches (equal key "p"))
                  (cl-incf index)
                  (goto-char (ring-ref positions index)))
                 (t
                  (message "`%s' does not match; %s"
                           (key-description key) instructions)))))
          (bn-clear-search-overlays))))))

(cl-defun bn-imenu ()
  (mapcar
   (lambda (l)
     (cons (car l)
	   (copy-marker (cdr l) ) ) )
   (mapcar 'car
	   bn-left-nonterminal-list) ) )


;;; MB changes
;;; buffer utils

(cl-defun bn--buffer-lines (start end)
  "Returns the lines between point START and END as a list of
strings"
  (interactive "r")
  (string-lines (buffer-substring-no-properties start end)))

(cl-defun bn--point-beginning-of-line (point &optional (forward-line 0))
  "Returns the point at the beginning of the line that includes POINT"
  (save-excursion
    (goto-char point)
    (move-beginning-of-line nil)
    (forward-line forward-line)
    (point)))

(cl-defun bn--buffer-region-whole-lines (start end)
  (interactive "r") ;;just for debugging
  (bn--buffer-lines
   (bn--point-beginning-of-line start)
   (bn--point-beginning-of-line end 1)))

(cl-defun bn--buffer-current-line (&optional (point (point)))
  (or (car (bn--buffer-region-whole-lines point point))
      ""))

;;; indent
(cl-defun bn-first-line-of-definition-p ()
  ""
  (string-match bn-left-nonterminal-definition
                (substring-no-properties (bn--buffer-current-line))))

(cl-defun bn-definition-start-line-p (line)
  (string-match bn-left-nonterminal-definition line))

(cl-defun bn-definition-start-lines (&optional _start)
  (cl-remove-if-not
   #'bn-definition-start-line-p
   (string-lines
    (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defun bn-alignment-column (lines)
  "Returns the first column that aligns all BNF definitions.
Aligment is on assigment sign.
Assumes one space from end of "
  (if lines
      (apply #'max
             (mapcar (lambda (line)
                       (or (string-match bn-assignment line) 0))
                     lines))
    0))
;;(string-match "::=" "af ::= adf")

(cl-defun bn-indent-assigment-first-line (column)
  "Indents current line?"
  (save-excursion
    (move-end-of-line nil)
    (re-search-backward bn-assignment nil t)
    (let ((n (- column (current-column))))
      (if (> n 0)
	(insert (make-string n ? ))
	(delete-char n)))))

(cl-defun bn-indent-assigment-rest-lines (column)
  (save-excursion
    (move-beginning-of-line nil)
    (just-one-space (+ column (length bn-assignment)))))

(cl-defun bn-indent-current-line (column)
  (save-excursion
    (move-beginning-of-line nil)
    (if (bn-first-line-of-definition-p)
      (bn-indent-assigment-first-line column)
      (bn-indent-assigment-rest-lines column))))

(cl-defun bn-indent-region (start end &optional column)
  (interactive "r\nP")
  (setq column
        (or (and column (prefix-numeric-value column))
            (bn-alignment-column
             (bn--buffer-region-whole-lines start end))))
  (let ((end-marker (copy-marker end t)))
    (unwind-protect
        (save-excursion
          (goto-char start)
          (while (< (point) end-marker)
            (bn-indent-current-line column)
            (forward-line 1)))
      (set-marker end-marker nil))))

(cl-defun bn-goto-previous-assigment ()
  (re-search-backward bn-assignment nil t)
  (current-column))

(cl-defun bn-previous-assigment-column ()
  (save-excursion
    (bn-goto-previous-assigment)))

(cl-defun bn-indent ()
  "Indent to previous"
  (interactive)
  (let ((column (bn-previous-assigment-column)))
    (unless column
      (user-error "No previous BNF assignment"))
    (bn-indent-region (line-beginning-position)
                      (line-end-position)
                      column)))

(provide 'backus-naur-mode)

;;; backus-naur-mode.el ends here
