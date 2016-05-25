(load-library "inf-lisp")
;; Define the program to be called by M-x run-lisp.
;; (setq inferior-lisp-program "clisp -I -q")

; (remove-hook 'inferior-lisp-mode-hook
; 	  #'(lambda () 
; 	      (set-buffer-multibyte nil)
; 	      (standard-display-european 1)))
;;inferior-lisp-mode-hook

(setq inferior-lisp-program
      (if (cygwin-emacs-p)
	"clisp"
	(concat *clisp-prog* " -M " *clisp-mem* " -B " *clisp-dir* " -q -I")))

;; Add new keybindings: C-x C-e evaluates the *next* form,
;; C-x C-m macroexpands the next form.
(defun lisp-eval-sexp (&optional and-go)
  "Send the next sexp to the inferior Lisp process.
            Prefix argument means switch to the Lisp buffer afterwards."
  (interactive "P")
  (lisp-eval-region (point)
		    (save-excursion (backward-sexp) (point))
		    and-go
		    ) )
(defun lisp-macroexpand-region (start end &optional and-go)
  "Macroexpand the current region in the inferior Lisp process.
            Prefix argument means switch to the Lisp buffer afterwards."
  (interactive "r\nP")
  (comint-send-string
   (inferior-lisp-proc)
   (format "(macroexpand-1 (quote %s))\n" (buffer-substring start end))
   )
  (if and-go (switch-to-lisp t))
  )
(defun lisp-macroexpand-sexp (&optional and-go)
  "Macroexpand the next sexp in the inferior Lisp process.
            Prefix argument means switch to the Lisp buffer afterwards."
  (interactive "P")
  (lisp-macroexpand-region (point)
			   (save-excursion (forward-sexp) (point))
			   and-go
			   ) )
;; Define the great keybindings.
(inferior-lisp-install-letter-bindings)
(define-key global-map "\C-hd" 'common-lisp-hyperspec)
(define-key global-map "\C-c\C-e" 'lisp-eval-sexp)
(define-key lisp-mode-map "\C-x\C-d" 'lisp-eval-defun)
(define-key lisp-mode-map "\C-x\C-e" 'lisp-eval-sexp)
(define-key inferior-lisp-mode-map "\C-x\C-e" 'lisp-eval-sexp)
(define-key lisp-mode-map "\C-x\C-m" 'lisp-macroexpand-sexp)
(define-key inferior-lisp-mode-map "\C-x\C-m" 'lisp-macroexpand-sexp)

;; Common Lisp indentation.
(load-library "cl-indent")
(add-hook 'lisp-mode-hook
	  (lambda ()
	    (setq lisp-indent-function 'common-lisp-indent-function)))

;; Additional definitions by Pierpaolo Bernardi.
(defun cl-indent (sym indent)
  (put sym 'common-lisp-indent-function
       (if (symbolp indent)
	 (get indent 'common-lisp-indent-function)
	 indent)))

(cl-indent 'if '1)
;; (cl-indent 'setvector 'dolist)
;; (cl-indent 'defclass '((&whole 4 &rest (&whole 2 &rest 1))
;; 		       &rest (&whole 2 &rest 1)))
;; (cl-indent 'defgeneric 'defun)
;; (cl-indent 'defmethod '(4 4 (&whole 4 &rest 1) &body))
;; (cl-indent 'generic-flet 'flet)
;; (cl-indent 'generic-labels 'labels)
;; (cl-indent 'symbol-macrolet 'multiple-value-bind)
;; (cl-indent 'with-accessors 'multiple-value-bind)
;; (cl-indent 'with-added-methods '((1 4 ((&whole 1))) (2 &body)))
;; (cl-indent 'with-slots 'multiple-value-bind)
;; (cl-indent 'handler-bind '((&whole 4 &rest 1) 2 &body))
;; (cl-indent 'handler-case '((1 4) (&whole 2 ((0 1) (1 3) (2 &body)))))
;; (cl-indent 'define-condition '((1 6)
;; 			       (2 6 ((&whole 1)))
;; 			       (3 4 ((&whole 1)))
;; 			       (4 &body)))
;; (cl-indent 'restart-bind '(((&whole 2 (0 1) (&whole 1))) (2 &body)))
;; (cl-indent 'restart-case '((1 4) (&whole 2 ((0 1) (&whole 1)))))
;; (cl-indent 'with-condition-restarts '((1 4 ((&whole 1))) (2 &body)))
;; (cl-indent 'with-simple-restart '((1 4 ((&whole 1))) (2 &body)))

;; (defun common-lisp-indent-function (indent-point state)
;;   "Redefintion of standard emacs cl function."
;;   ;; Pekka 04Jul96: Adapted from common-lisp-indent-function in cl-indent.el
;;   (let ((normal-indent (current-column)))
;;     ;; Walk up list levels until we see something
;;     ;;  which does special things with subforms.
;;     (let ((depth 0)
;;           ;; Path describes the position of point in terms of
;;           ;;  list-structure with respect to containing lists.
;;           ;; `foo' has a path of (0 4 1) in `((a b c (d foo) f) g)'
;;           (path ())
;;           ;; set non-nil when somebody works out the indentation to use
;;           calculated
;;           (last-point indent-point)
;;           ;; the position of the open-paren of the innermost containing list
;;           (containing-form-start (elt state 1))
;;           ;; the column of the above
;;           sexp-column)
;;       ;; Move to start of innermost containing list
;;       (goto-char containing-form-start)
;;       (setq sexp-column (current-column))
;;       ;; Look over successively less-deep containing forms
;;       (while (and (not calculated)
;;                   (< depth lisp-indent-maximum-backtracking))
;;         (let ((containing-sexp (point)))
;;           (forward-char 1)
;;           (parse-partial-sexp (point) indent-point 1 t)
;;           ;; Move to the car of the relevant containing form
;;           (let (tem function method)
;;             (if (not (looking-at "\\sw\\|\\s_"))
;;                 ;; This form doesn't seem to start with a symbol
;;                 (setq function nil method nil)
;;               (setq tem (point))
;;               (forward-sexp 1)
;;               (setq function
;; 		    (downcase (buffer-substring-no-properties tem (point))))
;;               (goto-char tem)
;;               (setq tem (intern-soft function)
;;                     method (get tem 'common-lisp-indent-function))
;;               (cond ((and (null method)
;;                           (string-match ":[^:]+" function))
;;                      ;; The pleblisp package feature
;;                      (setq function (substring function
;;                                                (1+ (match-beginning 0)))
;;                            method (get (intern-soft function)
;;                                        'common-lisp-indent-function)))
;;                     ((and (null method))
;;                      ;; backwards compatibility
;;                      (setq method (get tem 'lisp-indent-function)))))
;;             (let ((n 0))
;;               ;; How far into the containing form is the current form?
;;               (if (< (point) indent-point)
;;                   (while (condition-case ()
;;                              (progn
;;                                (forward-sexp 1)
;;                                (if (>= (point) indent-point)
;;                                    nil
;;                                  (parse-partial-sexp (point)
;;                                                      indent-point 1 t)
;;                                  (setq n (1+ n))
;;                                  t))
;;                            (error nil))))
;;               (setq path (cons n path)))

;;             ;; backwards compatibility.
;;             (cond ((null function))
;;                   ((null method)
;;                    (if (null (cdr path))
;;                        ;; (package prefix was stripped off above)
;;                        (setq method (cond ((string-match "\\`def"
;;                                                          function)
;;                                            '(4 (&whole 4 &rest 1) &body))
;;                                           ((string-match "\\`\\(with\\|do\\)-"
;;                                                          function)
;;                                            '(4 &body))))))
;;                   ;; backwards compatibility.  Bletch.
;;                   ((eq method 'defun)
;;                    (setq method '(4 (&whole 4 &rest 1) &body))))

;;             (cond ((and (memq (char-after (1- containing-sexp)) '(?\'))
;;                         (not (eql (char-after (- containing-sexp 2)) ?\#)))
;;                    ;; No indentation for "'(...)" elements
;; 		   ;; Pekka 04Jul96: But do indent `(...)
;;                    (setq calculated (1+ sexp-column)))
;; 		  ;; Pekka 04Jul96: Exception for ",(...)" or ",@(...)" removed
;;                   ((eql (char-after (1- containing-sexp)) ?\#)
;;                    ;; "#(...)"
;;                    (setq calculated (1+ sexp-column)))
;;                   ((null method))
;;                   ((integerp method)
;;                    ;; convenient top-level hack.
;;                    ;;  (also compatible with lisp-indent-function)
;;                    ;; The number specifies how many `distinguished'
;;                    ;;  forms there are before the body starts
;;                    ;; Equivalent to (4 4 ... &body)
;;                    (setq calculated (cond ((cdr path)
;;                                            normal-indent)
;;                                           ((<= (car path) method)
;;                                            ;; `distinguished' form
;;                                            (list (+ sexp-column 4)
;;                                                  containing-form-start))
;;                                           ((= (car path) (1+ method))
;;                                            ;; first body form.
;;                                            (+ sexp-column lisp-body-indent))
;;                                           (t
;;                                            ;; other body form
;;                                            normal-indent))))
;;                   ((symbolp method)
;;                    (setq calculated (funcall method
;;                                              path state indent-point
;;                                              sexp-column normal-indent)))
;;                   (t
;;                    (setq calculated (lisp-indent-259
;;                                       method path state indent-point
;;                                       sexp-column normal-indent)))))
;;           (goto-char containing-sexp)
;;           (setq last-point containing-sexp)
;;           (if (not calculated)
;;               (condition-case ()
;;                    (progn (backward-up-list 1)
;;                           (setq depth (1+ depth)))
;;                 (error (setq depth lisp-indent-maximum-backtracking))))))
;;       calculated)))

(provide 'lisp-config)
