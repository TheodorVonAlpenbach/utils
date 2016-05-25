(require 'evil)

;;; sexp movements
;;; beginning-of-next-sexp/beginning-of-previous-sexp w/b 
;;; first/last 0/$
;;; up-right/down-right k/j
;;; top/bottom H/L

;;; sexp visibility
;;; inner-sexp (with argument N to include the N outer sexps)

;;; insert-sexp
;;; replace-sexp

;;; transpose sexp, lift sexp, raise sexp

;;; expand-insert (a general evil command): This is insertion from
;;; normal mode using some kind of smart expansion mechanism.

;;; in lisp mode, toggle sexp keymap with s (or key chord ss) 
;;; Note these Emacs functions: goto-first-sexp

;;; Much of the clue is the variable evil-cross-lines. This should be
;;; set to t before applying the normal sexp movements. Maybe it
;;; should be set to t in the sexp mode alltogether.

"Not sure if I should define a certain state. Perhaps it is
better to keep it within Normal state and offer a
toggle-sexp-minor-mode method, or something. The toggle and
retoggle should be a nullimpotent operation.

Needs, then, to
· switch some keymaps
· define, undefine some text objects?
· change tag
· else?

Status 2014-02-12
Finished 
· basic sexp movements
· inner sexp (note the movements are different!)
TODO
· up and down lists, forwards and backwards
  forward up: u
  backward up: U
  forward down: b?
  backward down: B?

· transpose and lift

· inner defun (i.e. top level form)
"

(defun evil-normal-state-sexp-mode (arg)
  "Enters sexp minor mode in evil normal state. If prefixed, and
in sexp minor mode, exits mode.
TODO Handle crossline."

  (interactive "P")
  (if arg
    (evil-exit-normal-state-sexp-mode)
    (progn
      (define-key evil-normal-state-map "s" 'evil-exit-normal-state-sexp-mode)
     
      (define-key evil-motion-state-map "e" 'evil-forward-sexp-end)
      (define-key evil-motion-state-map "E" 'evil-backward-sexp-end)
      (define-key evil-motion-state-map "w" 'evil-forward-sexp-begin)
      (define-key evil-motion-state-map "W" 'evil-backward-sexp-begin)
      (define-key evil-outer-text-objects-map "s" 'evil-a-paren)
      (define-key evil-inner-text-objects-map "s" 'evil-inner-sexp))))

(defun evil-exit-normal-state-sexp-mode ()
  "Exits sexp minor mode in evil normal state."
  (interactive)

;;  (define-key evil-normal-state-map "s" 'evil-substitute)
  (define-key evil-motion-state-map "e" 'evil-forward-word-end)
  (define-key evil-motion-state-map "E" 'evil-forward-WORD-end)
  (define-key evil-motion-state-map "w" 'evil-forward-word-begin)
  (define-key evil-motion-state-map "W" 'evil-forward-word-begin)
  (define-key evil-outer-text-objects-map "s" 'evil-inner-sentence)
  (define-key evil-inner-text-objects-map "s" 'evil-inner-sentence))

(evil-define-motion evil-forward-sexp-begin (count) 
  "Move to the right by COUNT characters.
TODO: ensure that normal state is kept even if there are errors within function."
  :type exclusive
  (forward-sexp (1+ (or count 1)))
  (backward-sexp 1))

(defmacro with-char-offset (count body)
  `(prog1
       (progn
	(forward-char ,count)
	,body)
     (backward-char ,count)))
(cl-indent 'with-char-offset 'while)

(defmacro with-char-offset-if (count test body)
  `(if ,test 
     (with-char-offset ,count ,body)
     ,body))
(cl-indent 'with-char-offset-if 2)

(defun evil-bounds-of-sexp-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'sexp))
	(point (point)))
    (and bounds
	 (/= point (cdr bounds)) ;Emacs' right bounds does not exist in VI
	 (cons (car bounds) (1- (cdr bounds))))))

(defun evil-beginning-of-sexp ()
  (car (evil-bounds-of-sexp-at-point)))

(defun evil-end-of-sexp ()
  (cdr (evil-bounds-of-sexp-at-point)))

(defun evil-at-beginning-of-sexp-p ()
  (let ((eos (evil-beginning-of-sexp)))
    (and eos (= (point) eos))))

(defun evil-at-end-of-sexp-p ()
  (let ((eos (evil-end-of-sexp)))
    (and eos (= (point) eos))))

(defun evil-outside-sexp-p ()
  (not (evil-bounds-of-sexp-at-point)))

(evil-define-motion evil-backward-sexp-begin (count) 
  "Move to the right by COUNT characters.
TODO: ensure that normal state is kept even if there are errors within function."
  :type inclusive
  (if (evil-at-end-of-sexp-p)
    (forward-char 1))
  (backward-sexp (or count 1)))

(evil-define-motion evil-forward-sexp-end (count) 
  "Move to the right by COUNT characters.
TODO: ensure that normal state is kept even if there are errors within function."
  :type inclusive
  (when (evil-at-end-of-sexp-p)
    (forward-char 1))
  (forward-sexp (or count 1))
  (backward-char 1))

(evil-define-motion evil-forward-sexp-begin (count) 
  "Move to the right by COUNT characters.
TODO: ensure that normal state is kept even if there are errors within function."
  :type exclusive
  (if (or (evil-at-end-of-sexp-p)
	  (evil-outside-sexp-p))
    (progn (evil-forward-sexp-end (or count 1)) 
	   (evil-backward-sexp-begin 1))
    (progn (evil-forward-sexp-end (1+ (or count 1))) 
	   (evil-backward-sexp-begin 1))))

(evil-define-motion evil-backward-sexp-end (count) 
  "Move to the right by COUNT characters.
TODO: ensure that normal state is kept even if there are errors within function."
  :type exclusive
 (if (or (evil-at-beginning-of-sexp-p)
	  (evil-outside-sexp-p))
    (progn (evil-backward-sexp-begin (or count 1)) 
	   (evil-forward-sexp-end 1)) 
    (progn (evil-backward-sexp-begin (1+ (or count 1))) 
	   (evil-forward-sexp-end 1))))

(evil-define-motion evil-forward-down-sexp (count) 
  (down-list (or count 1)))

(evil-define-motion evil-backward-down-sexp (count) 
  (down-list (- (or count 1))))

(evil-define-motion evil-forward-up-sexp (count) 
  (up-list (or count 1)))

(evil-define-motion evil-backward-up-sexp (count) 
  (up-list (- (or count 1))))

;; Note that evil-a-sexp is set to evil-a-paren
(evil-define-text-object evil-inner-sexp (count &optional beg end type)
  ""
  :extend-selection t
  (evil-inner-object-range count beg end type #'forward-sexp #'backward-sexp))
