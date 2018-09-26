;;;; Taken from https://emacs.stackexchange.com/questions/29239/reflection-on-function-argument-signatures

(cond
 ;; XEmacs
 ((fboundp 'compiled-function-arglist)
  (defalias 'emacsen-compiled-function-arglist 'compiled-function-arglist))
 ;; GNU Emacs
 (t
  (defun emacsen-make-up-number-arglist (start end tail)
    (while (< start end)
      (setq end (1- end))
      (setq tail (cons (intern (format "a%d" end)) tail)))
    tail)
  (defun emacsen-compiled-function-arglist (func)
    (let ((a (aref func 0)))
      (if (integerp a)
          ;; An integer encoding the arity. Encountered in Emacs 24.3.
          ;; http://emacs.stackexchange.com/questions/971/argspec-or-arity-of-a-bytecode-function-in-emacs-24/973#973
          (let ((arglist (if (zerop (logand a 128))
                             nil
                           '(&rest rest)))
                (mandatory (logand a 127))
                (nonrest (lsh a -8)))
            (if (> nonrest mandatory)
                (setq arglist (cons '&optional (emacsen-make-up-number-arglist mandatory nonrest arglist))))
            (emacsen-make-up-number-arglist 0 mandatory arglist))
        ;; Otherwise: this is the arglist. The only format I've seen up to GNU 23.
        a)))))
(defun interactive-spec (function &optional safe)
  "Return the interactive calling specs of FUNCTION.
Signal an error if FUNCTION does not have interactive calling specs.
However, in this case, if optional second argument SAFE is non-nil,
return nil."
  (want-type 'functionp function)
  (while (symbolp function)
    (setq function (symbol-function function)))
  (condition-case e
      (progn
        (if (eq (car-safe function) 'autoload)
            (progn
              (if (null (fifth function))
                  (signal 'wrong-type-argument `(interactivep ,function)))
              (load (third function))))
        (cond
         ((byte-code-function-p function)
          (or (if (fboundp 'compiled-function-interactive)
                  (compiled-function-interactive function)
                (aref function 5))
              (signal 'wrong-type-argument `(interactivep ,function))))
         ((and (consp function)
               (eq 'lambda (car function)))
          (or (cdr (assq 'interactive (cdr (cdr function))))
              (signal 'wrong-type-argument `(interactivep ,function))))
         (t
          (signal 'failure `(interactive-spec ,function ,@(and safe (list safe)))))))
    (wrong-type-argument (if (and (eq (car-safe (cdr e)) 'interactivep) safe)
                             nil
                           (signal 'wrong-type-argument (cdr e))))))

(defun function-argspec (func)
  "Return a function's argument list.
For byte-compiled functions in Emacs >=24, some information may be lost as the
byte compiler sometimes erases argument names. In this case, fake argument names
are reconstructed."
  (if (symbolp func) (setq func (indirect-function func)))
  (cond
   ((or (subrp func)
        (and (consp func)
             (eq (car func) 'autoload)
             (consp (cdr func))
             (consp (cdr (cdr func)))
             (stringp (car (cdr (cdr func))))))
    (let ((docstring (documentation func)))
      (save-match-data
        (if (string-match "\n.*\\'" docstring)
            (let ((form (read (match-string 0 docstring))))
              (cdr form))
          nil))))
   ((byte-code-function-p func)
    (emacsen-compiled-function-arglist func))
   ((and (consp func)
         (eq (car func) 'lambda)
         (consp (cdr func)))
    (car (cdr func)))
   ((and (consp func)
         (eq (car func) 'closure)
         (consp (cdr func))
         (consp (cdr (cdr func))))
    (car (cdr (cdr func))))
   (t (signal 'wrong-type-argument
              (list 'functionp func)))))

(defun function-arity (func)
  "Return a function's arity as (MIN . MAX).
Return minimum and maximum number of args allowed for SUBR.
The returned value is a pair (MIN . MAX).  MIN is the minimum number
of args.  MAX is the maximum number or the symbol `many', for a
function with `&rest' args, or `unevalled' for a special form.

This function is like `subr-arity', but also works with user-defined
and byte-code functions. Symbols are dereferenced through
`indirect-function'."
  ;; TODO: keyword support
  (if (symbolp func) (setq func (indirect-function func)))
  (cond
   ((and (subrp func) (fboundp 'subr-arity))
    (subr-arity func))
   (t
    (let ((mandatory 0) (optional 0) (rest nil)
          (where 'mandatory))
      (when (and (consp func) (eq 'macro (car func)))
        (setq func (cdr func))
        (setq rest 'unevalled))
      (let ((argspec (function-argspec func)))
        (dolist (arg argspec)
          (cond
           ((eq arg '&optional) (setq where 'optional))
           ((eq arg '&rest) (unless rest (setq rest 'many)))
           (t (set where (+ (symbol-value where) 1)))))
        (cons mandatory (or rest (+ mandatory optional))))))))

(provide 'function-signature)
