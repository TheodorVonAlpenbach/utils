(cl-defun ada-login-infix-1 (org-digit group-digit)
  (format "%s_%d"
    (if (= org-digit 9) "499" (format "50%d" org-digit))
    group-digit))
;;(ada-login-infix-1 9 7)

(cl-defun ada-login-infix (prefix)
  "Return user name for a PU user.

By default, return \"499_1\".

Called with a one-digit prefix Y, return
\"499_Y\".

Called with a two-digit prefix SY, return
\"50S_Y\".
"
  (apply #'ada-login-infix-1 (if prefix (cl-floor prefix 10) '(9 1))))
;;(ada-login-infix 22)

(cl-defun ada-login-teacher ()
  "Return user name for a PU teacher.

The name is on the format \"claerer_no456326<INFIX>\", where
INFIX can be controlled by prefix argument. See `ada-login-infix'
for a description of how the prefix argument is converted to
INFIX.
"
  (interactive)
  (string-to-clipboard
   (format "claerer_no456326%s" (ada-login-infix current-prefix-arg))))
;;(ada-login-teacher)
;;(string-to-clipboard "qwe")

(cl-defun ada-login-pupil ()
  "Return user name for a PU pupil.

The name is on the format \"celev_no456326<INFIX><SUFFIX>\", where
INFIX can be controlled by prefix argument. See `ada-login-infix'
for a description of how the prefix argument is converted to
INFIX.
"
  (interactive)
  (string-to-clipboard
   (if current-prefix-arg
     (if (< current-prefix-arg 100)
       (format "celev_no456326%sa_1" (ada-login-infix current-prefix-arg))
       (cl-destructuring-bind (pupil-prefix-arg pupil-number)
	   (cl-floor current-prefix-arg 10)
	 (format "celev_no456326%sa_%d"
	   (ada-login-infix pupil-prefix-arg) pupil-number)))
     "celev_no456326499_1a_1")))

;; prefix for *clipboard-map* is "cv"
(defvar *ada-login-map* (make-sparse-keymap))

(define-key *ada-login-map* "p" #'ada-copy-password-to-clipboard)
(define-key *ada-login-map* "u" #'ada-copy-username-to-clipboard)
(define-key *ada-login-map* "e" #'ada-login-pupil)
(define-key *ada-login-map* "l" #'ada-login-teacher)

(define-key *clipboard-map* "a" *ada-login-map*)

(provide 'ada-logins)
