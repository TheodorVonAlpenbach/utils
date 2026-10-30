;;; backus-naur-mode.el --- Edit Backus-Naur grammars -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'imenu)
(require 'ring)
(require 'subr-x)

(defvar bn-tab-width 2
  "Tab width in backus-naur-mode")

(defconst bn-assignment "::=")
(defconst bn-lhs-token "[A-Za-z-]+")
(defconst bn-left-nonterminal-definition
  (format "^\\s-*<\\(%s\\)>\\s-*\\(%s\\)"
          bn-lhs-token bn-assignment)
  "The regular expression that marks the left side from the \
definition of a rule" )
;  ^<\([A-Za-z-]*\)>\s-*\(::=\)

(defvar-local bn-left-nonterminal-list nil
  "Left nonterminal definitions and their reference positions.")

(defvar-local bn-timer-var nil
  "Pending refontification timer, or nil.")

(defvar-local bn-search-history nil
  "Completion history for `bn-completing-search'.")

(defvar-local bn-search-overlays nil
  "Overlays used by the active nonterminal search.")

(defvar bn-comment-char "%"
  "a line which starts with this character is commented")

(defvar bn-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] #'bn-indent)
    (define-key map [home] #'bn-beginning-or-line)
    (define-key map [end] #'bn-end-or-line)
    (define-key map (kbd "M-s") #'bn-search)
    (define-key map (kbd "C-x SPC") #'bn-jump-to-definition-at-point)
    (define-key map (kbd "C-;") #'bn-completing-search)
    map)
  "Keymap for `bn-mode'.")

(defun bn--font-lock-keywords ()
  "Return font-lock keywords for the current BNF buffer."
  (list
   (list bn-left-nonterminal-definition
         '(1 'bn-left-nonterminal-definition-font)
         '(2 'bn-left-nonterminal-definition-closure))
   '("\\('\\)\\([[:graph:]]*\\)\\('\\)"
     (1 'bn-terminal-closure)
     (2 'bn-terminal-definition)
     (3 'bn-terminal-closure))
   '("\\(@\\)\\([[:graph:]]+\\)"
     (1 'bn-terminal-closure)
     (2 'bn-terminal-definition))
   (list
    (concat "[^[:graph:]]"
            "\\([[]\\)"
            "\\(.*\\)"
            "[^[:graph:]]"
            "\\([]]\\)")
    '(1 'bn-optional-closure)
    '(2 'bn-optional-definition)
    '(3 'bn-optional-closure))
   (list (concat "^\\(" (regexp-quote bn-comment-char) "\\)\\(.*\\)")
         '(1 'bn-comment-sign)
         '(2 'bn-commented-text))
   '("|" (0 'bn-or))))

(defun bn-beginning-or-line ()
  "Move to the previous definition, or to the beginning of the line."
  (interactive)
(if (eq (char-before) ?\n)
      (bn-beginning-of-defun)
    (beginning-of-line)))

(defun bn-end-or-line ()
  "Move to the end of the definition, or to the end of the line."
  (interactive)
  (if (eq (char-after) ?\n)
      (bn-end-of-defun)
    (end-of-line)))

(defun bn-jump-to-definition-at-point ()
  "Jump from the right nonterminal at point to its definition."
  (interactive)
  (let ((type (get-text-property (point) 'type))
        (nonterminal (get-text-property (point) 'nonterminal))
        (position (get-text-property (point) 'position)))
    (pcase type
      ('right
       (unless (integer-or-marker-p position)
         (user-error "No definition for `%s'" nonterminal))
       (bn-jump position)
       (message "Jumped to `%s'" nonterminal))
      ('left
       (user-error "`%s' is already a definition" nonterminal))
      (_
       (user-error "Point is not on a nonterminal")))))

(defun bn-completing-search ()
  "Select and jump to a left nonterminal definition."
  (interactive)
  (let* ((definitions (mapcar #'car bn-left-nonterminal-list))
         (name
          (completing-read "Search left nonterminal: "
                           definitions nil t nil
                           'bn-search-history))
         (position (cdr (assoc-string name definitions))))
    (unless position
      (user-error "No definition for `%s'" name))
    (bn-jump position)))

(define-derived-mode bn-mode fundamental-mode "Backus-Naur"
  "Major mode for editing context-free grammars in Backus-Naur notation."
  (setq-local tab-width bn-tab-width)
  (setq-local indent-region-function #'bn-indent-region)
  (setq-local beginning-of-defun-function #'bn-beginning-of-defun)
  (setq-local end-of-defun-function #'bn-end-of-defun)
  (make-local-variable 'bn-comment-char)
  (setq-local imenu-create-index-function #'bn-imenu)
  (setq-local font-lock-defaults
              (list (bn--font-lock-keywords) t))
  (bn-stop-timer)
  (bn-clear-search-overlays)
  (add-hook 'after-change-functions #'bn-change nil t)
  (add-hook 'kill-buffer-hook #'bn-stop-timer nil t)
  (bn-lazy-font-lock)
  (when (fboundp 'imenu-add-to-menubar)
    (imenu-add-to-menubar "Index")))

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

(defun bn-commented-line-p ()
  "Return non-nil when point is on a commented line."
  (eq (char-after (line-beginning-position))
      (string-to-char bn-comment-char)))

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

(defun bn-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a BNF definition.
With numeric ARG, repeat that many times."
  (interactive "p")
  (let ((count (or arg 1))
        found)
    (if (< count 0)
        (bn-end-of-defun (- count))
      (dotimes (_ count)
        (setq found
              (re-search-backward bn-left-nonterminal-definition nil t)))
      (when found
        (beginning-of-line))
      found)))

(defun bn-end-of-defun (&optional arg)
  "Move forward to the end of a BNF definition.
With numeric ARG, repeat that many times."
  (interactive "p")
  (let ((count (or arg 1))
        found)
    (if (< count 0)
        (bn-beginning-of-defun (- count))
      (dotimes (_ count)
        (forward-line 1)
        (setq found
              (re-search-forward bn-left-nonterminal-definition nil t))
        (if found
            (beginning-of-line)
          (goto-char (point-max)))))
    found))

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
(defun bn-first-line-of-definition-p ()
  "Return non-nil when the current line starts a BNF definition."
  (string-match-p bn-left-nonterminal-definition
                  (bn--buffer-current-line)))

(defun bn-definition-start-line-p (line)
  "Return non-nil when LINE starts a BNF definition."
  (string-match-p bn-left-nonterminal-definition line))

(cl-defun bn-definition-start-lines (&optional _start)
  (cl-remove-if-not
   #'bn-definition-start-line-p
   (string-lines
    (buffer-substring-no-properties (point-min) (point-max)))))

(defun bn-alignment-column (lines)
  "Return the assignment column needed to align BNF definition LINES."
  (if lines
      (apply #'max
             (mapcar (lambda (line)
                       (or (string-match bn-assignment line) 0))
                     lines))
    0))
(defun bn-indent-assignment-first-line (column)
  "Align the assignment on the current definition line to COLUMN."
  (save-excursion
    (end-of-line)
    (unless (re-search-backward bn-assignment (line-beginning-position) t)
      (user-error "Current line has no BNF assignment"))
    (let ((assignment-start (point)))
      (skip-chars-backward " \t" (line-beginning-position))
      (when (< column (current-column))
        (user-error "Column %d would overlap the left nonterminal" column))
      (delete-region (point) assignment-start)
      (insert (make-string (- column (current-column)) ?\s)))))

(defalias 'bn-indent-assigment-first-line
  #'bn-indent-assignment-first-line)

(defun bn-indent-assignment-rest-lines (column)
  "Indent the current continuation line relative to COLUMN."
  (save-excursion
    (beginning-of-line)
    (just-one-space (+ column (length bn-assignment)))))

(defalias 'bn-indent-assigment-rest-lines
  #'bn-indent-assignment-rest-lines)

(defun bn-indent-current-line (column)
  "Indent the current BNF line using assignment COLUMN."
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at-p "[ \t]*$")
      (delete-horizontal-space))
     ((bn-commented-line-p))
     ((bn-first-line-of-definition-p)
      (bn-indent-assignment-first-line column))
     (t
      (bn-indent-assignment-rest-lines column)))))

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

(defun bn-goto-previous-assignment ()
  "Move to the previous BNF assignment and return its column."
  (when (re-search-backward bn-assignment nil t)
    (current-column)))

(defalias 'bn-goto-previous-assigment #'bn-goto-previous-assignment)

(defun bn-previous-assignment-column ()
  "Return the column of the previous BNF assignment, or nil."
  (save-excursion
    (bn-goto-previous-assignment)))

(defalias 'bn-previous-assigment-column
  #'bn-previous-assignment-column)

(cl-defun bn-indent ()
  "Indent to previous"
  (interactive)
  (let ((column (bn-previous-assignment-column)))
    (unless column
      (user-error "No previous BNF assignment"))
    (bn-indent-region (line-beginning-position)
                      (line-end-position)
                      column)))

(provide 'backus-naur-mode)

;;; backus-naur-mode.el ends here
