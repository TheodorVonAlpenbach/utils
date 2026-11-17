;;; mb-evil.el --- Personal Evil commands and setup -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal Evil commands and explicit setup used by `mb-evil-map'.

;;; Code:

;;;; My Evil adaptations
;;;; See also http://wikemacs.org/wiki/Evil

;;;; I want 'df' to be key chord for leaving insert state.
;;;; In normal mode I want 'vn' (or something) to be ido-switch-buffer.
;;;; In normal mode I want 'vs' (or something) to be smart-swap.
;;;; For some buffers, I want to switch to Norwegian keyboard in
;;;; insert state.
;;;; For some modes, I want normal Emacs behavior.
;;;; Also, I want colors to indicate state

;;;; All of these wishes have been fullfilled with the implementation
;;;; below. Also, I have managed to use evil-cleverparens for lisp

;;;; TODO: auto loading of this file
;;;; TODO: auto loading of evil-cleverparens for lisp modes
;;;; TODO: tweek keys. L <--> l and H <--> h in evil-cleverparens.
;;;; TODO: norsk i insert-mode og overwrite-mode

;;;; Also useful to look at key bindings in
;;;; ~/.emacs.d/elpa/evil-20160525.1148/evil-maps.el
(require 'cl-lib)
(require 'ffap)
(require 'evil)
(require 'evil-cleverparens)
(require 'evil-cleverparens-text-objects)
(require 'evil-textobj-line)
(require 'evil-exchange)

(declare-function gnuplot-run-buffer nil)
(declare-function gnuplot-send-buffer-to-gnuplot nil)
(declare-function mb-lilypond-compile "mb-lilypond")
(declare-function slime-compile-and-load-file "slime")
(declare-function slime-eval "slime")
(declare-function slime-eval-defun "slime")
(declare-function slime-eval-last-expression "slime")
(declare-function slime-pprint-eval-last-expression "slime")
(declare-function split-5-points-string "quiz-park")

(defvar mb-evil--setup-complete nil
  "Non-nil after `mb-evil-setup' has completed.")

(defvar mb-evil--default-mode-line-colors nil
  "Mode-line background and foreground saved by `mb-evil-setup'.")

(cl-defun set-mb-lisp-locals ()
  "Enable personal behavior for Lisp modes."
  (evil-cleverparens-mode)
  (setf evil-symbol-word-search t)
  (display-fill-column-indicator-mode))

;;; Keyboard shortcuts
(cl-defun evil-key-chord-define (state keymap key def &rest bindings)
  "Bind KEY to DEF in Evil STATE in KEYMAP.
STATE can take the same values as in `evil-define-key'."
  (let ((aux-maps
         (cond ((listp state)
                (cl-loop for st in state
                         collect (evil-get-auxiliary-keymap keymap st t)))
               (state
                (list (evil-get-auxiliary-keymap keymap state t)))
               (t
                (list keymap)))))
    (while key
      (dolist (map aux-maps)
	(key-chord-define map key def))
      (setq key (pop bindings)
            def (pop bindings)))
    ;; ensure the prompt string comes first
    (dolist (map aux-maps)
      (evil-set-keymap-prompt map (keymap-prompt map)))))

(cl-defun evil-move-past-close ()
  "Is made for insert state."
  (interactive)
  (up-list 1)
  (cl-case major-mode
    (emacs-lisp-mode
     (insert " "))))

(cl-defun save-buffer-file ()
  "Same as `save-buffer', but only if buffer-file exists."
  (interactive)
  (when (buffer-file-name)
    (save-buffer)))
;;; Save on exit insert state ("df")

(cl-defun mb-show-process-buffer ()
  "Display the process buffer associated with the current major mode."
  (interactive)
  (cl-case major-mode
    (octave-mode (octave-show-process-buffer))
    (emacs-lisp-mode (display-buffer "*scratch*" '(display-buffer-use-some-window)))
    (mb-lisp-mode (mblisp-show-process-buffer))))

(evil-define-motion evil-goto-line-keep-column (count)
  "Go to the first non-blank character of line COUNT.
By default the last line."
  :jump t
  :type line
  (let ((column (current-column)))
    (if (null count)
      (with-no-warnings (end-of-buffer))
      (forward-line (- count (line-number-at-pos))))
    (eol)
    (cl-loop while (> (current-column) column) do (backward-char))))

(cl-defun ffap-read-file-or-url-no-prompt (_prompt guess)
  "Return the expanded file or URL from GUESS without prompting."
  (or guess (setq guess default-directory))
  (unless (or (ffap-url-p guess)
              (ffap-file-remote-p guess))
    (setq guess (abbreviate-file-name (expand-file-name guess))))
  (or (ffap-url-p guess)
      (substitute-in-file-name guess)))

(cl-defun ffap-no-prompt (&optional filename)
  "Visit FILENAME using `ffap' without prompting."
  (interactive)
  (advice-add #'ffap-read-file-or-url :override #'ffap-read-file-or-url-no-prompt)
  (unwind-protect
      (ffap filename)
    (advice-remove #'ffap-read-file-or-url
                   #'ffap-read-file-or-url-no-prompt)))

(cl-defun ffap-no-prompt-read-only (&optional filename)
  "Visit FILENAME using `ffap-no-prompt' and make its buffer read-only."
  (interactive)
  (ffap-no-prompt filename)
  (setf buffer-read-only t))

(cl-defun ffap-previous ()
  "Visit the previous file or URL recognized by FFAP."
  (interactive)
  (ffap-next t))

(cl-defun transpose-split-orientation ()
  "If there is a simple horizontal split, change to vertical, and vice versa.
A simple split consists of two windows only."
  (interactive)
  (when (= 2 (count-windows))
    (let ((func (if (window-full-height-p)
                  #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
	(other-window 1)
	(switch-to-buffer (other-buffer))))))

(cl-defun rotate-windows ()
  "Rotate windows."
  (interactive)
  (let ((cb (current-buffer)))
    (cl-loop with buffers = (cl-loop for w in (window-list) collect (window-buffer w))
	  for b in (rotate-list buffers)
	  for w in (window-list)
	  do (set-window-buffer w b))
    (select-window (get-buffer-window cb))))

(cl-defun swap-windows ()
  "Rotate windows."
  (interactive)
  (rotate-windows)
  (other-window -1))
;;(swap-windows)

(cl-defun find-tag-no-prompt ()
  "Visit the tag at point without prompting."
  (interactive)
  (let* ((tagname (find-tag-default))
	 (buf (find-tag-noselect tagname))
	 (pos (with-current-buffer buf (point))))
    (condition-case nil
	(switch-to-buffer buf)
      (error (pop-to-buffer buf)))
    (goto-char pos)))
;;(find-tag-no-prompt)

(cl-defun find-tag-no-prompt-read-only ()
  "Visit the tag at point and make a newly opened buffer read-only."
  (interactive)
  (let ((n (length (buffer-list))))
    (find-tag-no-prompt)
    (when (> (length (buffer-list)) n)
      (setf buffer-read-only t))))

(require 'mb-emacs-lisp)
(require 'mb-utils-buffer)
(require 'mb-metafont)

(cl-defun minor-mode-p (mode)
  "Check if symbol MODE is an active minor-mode in the current buffer."
  (condition-case nil
      (and (symbolp mode)
	   (symbol-value mode)
	   (cl-find mode minor-mode-list))
    (error nil)))
;;(mapcar #'minor-mode-p '(slime-mode undo-tree-mode))

(cl-defun slime-p ()
  "Return non-nil when `slime-mode' is active."
  (minor-mode-p 'slime-mode))
;;(slime-p)

;;; Eval machinery
(cl-defun mb-eval-string (string &rest args)
  "Evaluate STRING for the current major mode.
Pass ARGS to the mode-specific evaluator when it accepts them."
  (cl-case major-mode
    (mb-lisp-mode
     (if (slime-p)
       (slime-eval string)
       (apply #'mb-lisp-eval-1 string args)))
    (emacs-lisp-mode (eval (read string)))
    (octave-mode (octave-send-string string))
    (js-mode (apply #'js-eval-string string args))))
;;(mb-eval-string "(+ 2 2)")

(cl-defun eval-form ()
  "This is special to Lisp languages only."
  (interactive)
  (cl-case major-mode
    ((emacs-lisp-mode mb-lisp-mode)
     (save-excursion
       (backward-up-list 1)
       (forward-sexp 1)
       (mb-eval-last-sexp)))
    (octave-mode (octave-send-block))))

(cl-defun mb-eval-last-sexp (&rest args)
  "Evaluate the expression preceding point.
Pass ARGS to the mode-specific evaluator."
  (interactive)
  (cl-case major-mode
    (emacs-lisp-mode (eval-last-sexp args))
    (mb-lisp-mode (if (slime-p)
		    (if args
		      (slime-pprint-eval-last-expression)
		      (slime-eval-last-expression))
		    (mb-lisp-eval-sexp args)))
    (python-mode (apply #'python-shell-send-region
		   (mb-python-last-sexp-region)))
    (octave-mode (apply #'octave-eval-last-sexp args))
    (t (apply #'mb-eval-region (append (last-sexp-region) args)))))

(cl-defun eval-current-sexp ()
  "Evaluate the expression at point for the current major mode."
  (interactive)
  (cl-case major-mode
    ((emacs-lisp-mode mb-lisp-mode)
     (save-excursion
       (forward-sexp 1)
       (if (slime-p)
	 (slime-eval-last-expression)
	 (eval-last-sexp nil))))
    (octave-mode
     (save-excursion (eol) (mb-eval-last-sexp)))
    (otherwise
     (princ (mb-eval-string
	     (string-match* "\\(?:#\\|//+\\|[[:space:]]\\)*\\(.*\\)"
	       (line-string) :num 1))))))
;;(+ (+ 111 2) 3)

(cl-defun eval-defun-test (&optional no-eval-p)
  "Evaluate the current definition and its following test.
With NO-EVAL-P non-nil, evaluate only the test."
  (interactive)
  (unless no-eval-p (mb-eval-defun))
  (save-excursion
    (cl-case major-mode
      ((emacs-lisp-mode mb-lisp-mode python-mode)
       (evil-cp-end-of-defun)
       (eol :offset 1)
       (mb-eval-last-sexp))
      (metafont-mode (meta-eval-buffer))
      (otherwise
       (end-of-defun)
       (eol)
       (mb-eval-last-sexp)))))
;;(eval-defun-test)

(cl-defun gp-eval-buffer ()
  "Run the current Gnuplot buffer and visit its output file."
  (interactive)
  (gnuplot-run-buffer)
  (let ((filename (string-match* "set[\t ]+output[\t ]+['\"]\\([^'\"]*\\)"
		    (buffer-string-no-properties) :num 1)))
    (find-file-other-window filename)))

(cl-defun strip-ssh (filename)
  "Strip an old-style SSH prefix from FILENAME."
  (or (string-match* "/ssh:[^:]*:\\(.*\\)" filename :num 1)
      filename))
;;(strip-ssh "/ssh:pf:/home/mats_progfab_no/git/problem-server/")

(cl-defun mb-eval-buffer (&optional args)
  "Evaluate or compile the current buffer.
Pass optional ARGS to evaluators that accept additional arguments."
  (interactive)
  (cl-case major-mode
    (emacs-lisp-mode (apply #'eval-buffer args))
    (gnuplot-mode (gnuplot-send-buffer-to-gnuplot))
    (mb-lisp-mode (if (slime-p)
		    (slime-compile-and-load-file)
		    (lisp-load-file (strip-ssh (buffer-file-name)))))
    (metafont-mode (meta-compile-file (buffer-file-name)))
    (octave-mode (octave-source-buffer))
    (python-mode (python-shell-send-buffer nil))
    ((c++-mode cc-mode makefile-mode) (compile "make -k"))
    (latex-mode (TeX-command-run-all nil))
    (LilyPond-mode (mb-lilypond-compile))
    (otherwise (mb-eval-region (point-min) (point-max)))))

(cl-defun mb-eval-defun ()
  "Evaluate the current definition for the current major mode."
  (interactive)
  (cl-case major-mode
    (emacs-lisp-mode (eval-defun current-prefix-arg))
    (mb-lisp-mode (when (slime-p)
		    (slime-eval-defun)))
    (python-mode (apply #'python-shell-send-region (mb-python-defun-region)))
    (otherwise (mb-eval-region (bod*) (eod*) t))))

(cl-defun mb-eval-region (start end &optional printflag read-function)
  "Evaluate the region from START to END for the current major mode.
PRINTFLAG and READ-FUNCTION are passed to Emacs Lisp evaluation."
  (interactive "r")
  (cl-case major-mode
    (emacs-lisp-mode (eval-region start end printflag read-function))
    (python-mode (python-shell-send-region start end nil))
    (sh-mode (sh-execute-region start end))
    (latex-mode (TeX-command-run-all-region))
;;    (octave-mode (octave-send-region start end))
    (otherwise
     (princ (mb-eval-string
	     (buffer-substring-no-properties start end)
	     printflag)))))

(cl-defun mb-eval-region-from-point (&optional printflag read-function)
  "Evaluate from the current line through the end of the buffer.
Pass PRINTFLAG and READ-FUNCTION to `mb-eval-region'."
  (interactive)
  (mb-eval-region (bol) (point-max) printflag read-function))

(cl-defun mb-eval-region-to-point (&optional printflag read-function)
  "Evaluate from the start of the buffer through the current line.
Pass PRINTFLAG and READ-FUNCTION to `mb-eval-region'."
  (interactive)
  (mb-eval-region (point-min) (eol*) printflag read-function))

(cl-defun mb-compile-buffer ()
  "Compile the current buffer when its major mode supports compilation."
  (interactive)
  (cl-case major-mode
    (octave-mode (octave-source-buffer))))

(cl-defun mb-normal-state-init ()
  "Enable key chords when entering Evil normal state."
  (key-chord-mode 1))
;;(add-hook 'evil-normal-state-entry-hook 'mb-normal-state-init)

(cl-defun quailify-key-chord-input-method (result)
  "Modify the RESULT of `key-chord-input-method'.
If RESULT is a key-chord, i.e. it is a list on the form
\(KEYCHORD CHAR1 CHAR2 ...\), RESULT is returned unmodified. If
not, RESULT should be a list of one character (an error is
signaled otherwise). In this case, if `current-input-method' is
\"norwegian-keyboard\", this character is passed to
`quail-input-method' which result is returned. If
`current-input-method' is nil \(or another language the the above
mentioned\) RESULT is returned unmodified."
  (if (or (not current-input-method)
	  (not (cl-member current-input-method
			  '("norwegian-keyboard" "croatian")
			  :test #'string=))
	  (and (listp result)
	       (> (length result) 2)
	       (eql (first result) 'key-chord)))
    result
    ;; else translate RESULT with quail:
    (if (and (listp result)
	     (= (length result) 1))
      (quail-input-method (first result))
      (error "Unexpected output from KEY-CHORD-INPUT-METHOD: %S" result))))

(cl-defun mb-key-chord-advice (_input-method)
  "Restore `key-chord-input-method' after activating an input method."
  (setq input-method-function 'key-chord-input-method))

(cl-defun mb-insert-state-init ()
  "Set up input methods and key chords for Evil insert state."
  ;;(key-chord-mode 1)
  (when (member (buffer-name) '("arbeidslog" "log.org"))
    (activate-input-method 'norwegian-keyboard))
  (advice-add #'key-chord-input-method
	      :filter-return #'quailify-key-chord-input-method))

(cl-defun mb-insert-state-cleanup ()
  "Remove insert-state key-chord input translation."
  (advice-remove #'key-chord-input-method #'quailify-key-chord-input-method))

(defun mb-evil-update-mode-line ()
  "Update mode-line colors to reflect the current Evil state."
  (let ((color
         (cond ((minibufferp) mb-evil--default-mode-line-colors)
               ((evil-insert-state-p) '("#e80000" . "#ffffff"))
               ((evil-emacs-state-p) '("#444488" . "#ffffff"))
               ((buffer-modified-p) '("#006fa0" . "#ffffff"))
               (t mb-evil--default-mode-line-colors))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))

(defconst mb-evil--initial-states
  '((inferior-emacs-lisp-mode . emacs)
    (inferior-lisp-mode . emacs)
    (inferior-octave-mode . emacs)
    (shell-mode . emacs)
    (git-commit-mode . emacs)
    (git-rebase-mode . emacs)
    (term-mode . emacs)
    (help-mode . emacs)
    (helm-grep-mode . emacs)
    (grep-mode . emacs)
    (bc-menu-mode . emacs)
    (magit-branch-manager-mode . emacs)
    (rdictcc-buffer-mode . emacs)
    (dired-mode . emacs)
    (wdired-mode . normal))
  "Initial Evil states for modes used by the personal configuration.")

(cl-defun alf/key-chord-undefine (keys)
  "Undefine the key chord identified by KEYS.
This should be done by `key-chord-unset-global', however that
does not work for me."
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (if (eq key1 key2)
        (global-unset-key (vector 'key-chord key1 key2))
      ;; else
      (global-unset-key (vector 'key-chord key1 key2))
      (global-unset-key (vector 'key-chord key2 key1)))))
;;(alf/key-chord-undefine "df")

(evil-define-text-object mb-evil-buffer
    (cl-count &optional _beg _end _type)
  "Select the entire buffer."
  (evil-range (point-min) (point-max) 'inclusive :expanded t))

(evil-define-text-object mb-evil-inner-defun
    (cl-count &optional _beg _end _type)
  "Select the inside of a definition in C-like languages."
  (evil-range (bol* :point (bod*) :offset 1)
	      (eol* :point (eod*) :offset -2)
	      'inclusive :expanded t))

(evil-define-text-object mb-evil-inner-variable-name
    (cl-count &optional _beg _end _type)
  "Select the inside of a delimited variable name."
  (cl-destructuring-bind (beg end) (last-sexp-region)
    (evil-range (1+ beg) (1- end) 'inclusive :expanded t)))

(evil-define-operator evil-yank-line (beg end type register yank-handler)
  "Yank to end of line.
This overrides `evil-yank-line' to conform to `evil-delete-line':
Y yanks the same region as C changes and D deletes.

This function is simply a clone of `evil-delete-line', with every
occurrence of \"delete\" replaced with \"yank\"."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  ;; act linewise in Visual state
  (let* ((beg (or beg (point)))
         (end (or end beg)))
    (when (evil-visual-state-p)
      (unless (memq type '(line block))
        (let ((range (evil-expand beg end 'line)))
          (setq beg (evil-range-beginning range)
                end (evil-range-end range)
                type (evil-type range))))
      (evil-exit-visual-state))
    (cond
     ((eq type 'block)
      ;; equivalent to $d, i.e., we use the block-to-eol selection and
      ;; call `evil-yank'. In this case we fake the call to
      ;; `evil-end-of-line' by setting `temporary-goal-column' and
      ;; `last-command' appropriately as `evil-end-of-line' would do.
      (let ((temporary-goal-column most-positive-fixnum)
            (last-command 'next-line))
        (evil-yank beg end 'block register yank-handler)))
     ((eq type 'line)
      (evil-yank beg end type register yank-handler))
     (t
      (evil-yank beg (line-end-position) type register yank-handler)))))

(evil-define-motion evil-goto-line-preserve-column (count)
  "Go to the first non-blank character of line COUNT.
By default the last line."
  :jump t
  :type line
  (let ((c (current-column)))
    (if (null count)
      (with-no-warnings
	(end-of-buffer)
	(evil-previous-line))
      (goto-char (point-min))
      (forward-line (1- count)))
    (evil-goto-column c)))

(cl-defun print-last-pdf-in-Messages ()
  "Visit the last PDF file mentioned in the *Messages* buffer."
  (interactive)
  (with-buffer "*Messages*"
    (save-excursion
      (eob)
      (re-search-backward "\\.pdf\"")
      (ffap-no-prompt-read-only))))
;;(print-last-pdf-in-Messages)

;;;; Insert quotes (put this somewhere else when finished)
(cl-defun mb-surround-region-1 (region left right n)
  "Surround REGION with N copies of LEFT and RIGHT."
  (insert-at left (first region) n)
  (insert-at right (+ (second region) (* n (length left))) n))

(cl-defun mb-surround-lookup-right (left)
  "Return the closing delimiter corresponding to LEFT."
  (string-case left
    ("(" ")")
    ("`" "'")
    ("«" "»")
    ("<" ">")
    ("[" "]")
    ("{" "}")
    ("/" "/")
    (otherwise left)))
;;(mapcar #'mb-surround-lookup-right (list "`" "'" "(" "<"))

(cl-defun mb-surround-region (region left n)
  "Surround REGION with N copies of LEFT and its matching delimiter."
  (mb-surround-region-1 region left (mb-surround-lookup-right left) n))

(cl-defun mb-surround-word (left n)
  "Surround N words at point with LEFT and its matching delimiter."
  (mb-surround-region (cons (bow*) (eow* n)) left 1))

(cl-defun mb-surround-symbol (left n)
  "Surround N symbols at point with LEFT and its matching delimiter."
  (mb-surround-region (list (bos*) (eos* n)) left 1))

(cl-defun mb-surround (left n)
  "Surround the active region or N symbols with delimiter LEFT."
  (if (use-region-p)
    (mb-surround-region (region) left n)
    ;; (mb-surround-word left n)
    (mb-surround-symbol left n)))

;; Undo surround: strictly assume surround is 1 char wide on both
;; sides
(cl-defun mb-undo-surround (n)
  "Remove N delimiter characters around the word at point."
  (if (use-region-p)
    (error "Undo surround region not implemented!")
    (save-excursion
      (bow)
      (delete-char (- n))
      (eow)
      (delete-char n))))

(cl-defun evil-increment-rectangle ()
  "Replace a visual rectangle with consecutive integers."
  (interactive)
  (let ((p (point))
	(m (mark)))
    (let ((b (min p m))
	  (e (1- (max p m))))
      (message "%d %d" b e)
      (evil-apply-on-block
       (let (last)
	 #'(lambda (beg end)
	     (if last
	       (let ((s (number-to-string (cl-incf last))))
		 (overwrite-region s beg end))
	       (setf last (string-to-number (region-string beg (1+ end)))))))
       b e nil))))
;;(evil-increment-rectangle)

(evil-define-operator evil-rot13 (beg end type)
  "Convert text to its rot13 counterpart."
  (if (eq type 'block)
      (evil-apply-on-block #'evil-rot13 beg end nil)
    (rot13-region beg end)))

(evil-define-operator evil-rot47 (beg end type)
  "Convert text to its rot47 counterpart."
  (if (eq type 'block)
      (evil-apply-on-block #'evil-rot47 beg end nil)
    (rot47-region beg end)))

(evil-define-operator evil-split-5-points-string (beg end type)
  "Convert text to its rot47 counterpart."
  (if (eq type 'block)
      (evil-apply-on-block #'evil-split-5-points-string beg end nil)
    (region-replace-raw
     (split-5-points-string (region-string beg end))
     (list beg end))))

(defun mb-evil--configure-open-lisp-buffers ()
  "Enable personal Lisp behavior in already-open startup buffers."
  (cl-loop for buffer in
           (list (get-buffer ".emacs")
                 (and (boundp '+emacs-local+)
                      (get-file-buffer +emacs-local+)))
           when (buffer-live-p buffer)
           do (with-current-buffer buffer
                (set-mb-lisp-locals))))

(defun mb-evil-setup ()
  "Activate personal Evil behavior.
Repeated calls have no effect.  Return non-nil after successful setup."
  (interactive)
  (unless mb-evil--setup-complete
    (evil-mode 1)
    (setf evil-move-beyond-eol t
          evil-cross-lines t
          evil-cleverparens-swap-move-by-word-and-symbol t
          parens-require-spaces t)
    (evil-select-search-module 'evil-search-module 'evil-search)
    (evil-exchange-install)
    (add-hook 'emacs-lisp-mode-hook #'set-mb-lisp-locals)
    (add-hook 'lisp-mode-hook #'set-mb-lisp-locals)
    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
    (sp-local-pair 'emacs-lisp-mode "`" "'"
                   :when '(sp-in-string-p sp-in-comment-p))
    (mb-evil--configure-open-lisp-buffers)
    (add-hook 'evil-insert-state-exit-hook #'save-buffer-file)
    (advice-add #'activate-input-method :after #'mb-key-chord-advice)
    (add-hook 'evil-insert-state-entry-hook #'mb-insert-state-init)
    (add-hook 'evil-insert-state-exit-hook #'mb-insert-state-cleanup)
    (setq mb-evil--default-mode-line-colors
          (cons (face-background 'mode-line)
                (face-foreground 'mode-line)))
    (add-hook 'post-command-hook #'mb-evil-update-mode-line)
    (cl-loop for (mode . state) in mb-evil--initial-states
             do (evil-set-initial-state mode state))
    (define-key evil-normal-state-map "gG"
                #'evil-goto-line-preserve-column)
    (setq mb-evil--setup-complete t))
  mb-evil--setup-complete)

(provide 'mb-evil)

;;; mb-evil.el ends here
