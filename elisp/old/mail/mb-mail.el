(defun mb-make-arglist-string (args)
  "If ARGS is a tree of strings, converts it to a commaseparated
string. Else return ARGS."
  (if (atom args)
    (if (stringp args) args
	(error "args is neither string nor a list of strings."))
    (if (= (length args) 1)
      (mb-make-arglist-string (first args))
      (concat (mb-make-arglist-string (first args)) ","
	      (mb-make-arglist-string (rest args))))))
;;(mb-make-arglist-string (list "test" (list "qwe")))

(defun* mb-mail-convert-blat-arguments
    (&key (subject "no subject")
	  (message "")
	  (to "mb@transistix.com")
	  cc bcc
	  (from "mb@transistix.com")
	  (organization "Transistix")
	  (smpt "mta.nextra.no"))
  (let ((args (list "blat" "*blat*"
		    "c:/unix/www/mail/blat/blat.exe"
		    "c:/unix/www/mail/blat/empty.txt"
		    "-subject" (format "'%s'" (string-make-unibyte subject))
		    "-to" (mb-make-arglist-string to)
		    "-body" (format "'%s'" (string-make-unibyte message))
		    "-i" from
		    "-q"
		    "-server" smpt)))
    (when cc
      (setq args (nconc args (list "-cc" (mb-make-arglist-string cc)))))
    (when bcc
      (setq args (nconc args (list "-bcc" (mb-make-arglist-string bcc)))))
    args))
;;(mb-convert-arguments :to "mb@transistix.com" :message "ø a")
;;(string-make-unibyte "å")
;;(with-buffer (get-buffer "clue.lsp") (set-buffer-multibyte t))
;;(with-buffer (get-buffer "*inferior-lisp*") (set-buffer-multibyte t))

(defun* mb-mail-send (&rest args)
  "The mother of all send mail functions."
  (apply #'start-process-shell-command (apply #'mb-mail-convert-blat-arguments args)))
;;(mb-mail-send :to "mb@transistix.com" :message "ø a")
;;(mb-mail-convert-blat-arguments :to "mb@transistix.com" :message "ø")

(defvar *mb-address-book-extra* nil)
(setq *mb-address-book-extra*
      '(("Lars Mobil Sveen" "4793437261@sms.netcom.no")
	("Mats Mobil Bergstrøm" "4793433496@sms.netcom.no")
	("Lars Hjemme Sveen" "l.b.sveen@c2i.net")
	("Christine Mobil Munch" "4793499597@sms.netcom.no")))
;;(setq *mb-address-book-extra* nil)

(defun* mb-parse-addresses ()
  "Parses corresponding NAME and EMAIL fields in current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
;    (re-search-forward "=================")
    (let ((res ())
	  (e-address nil)
	  (point 0)) 
      (while (re-search-forward "^EMAIL\\s-*:\\s-*\\([^\n\t ]*\\)$" nil t)
	(setq e-address (match-string 1))
	(setq point (point))
	(re-search-backward "^NAME\\s-*:\\s-*\\([^,\n]*\\),\\s-*\\(.*\\)\\s-*$" nil t)
	(push (list (concat (match-string 1) ", " (match-string 2)) e-address) res)
	(push (list (concat (match-string 2) " " (match-string 1)) e-address) res)
	(goto-char point)
	(re-search-forward "^NAME\\s-*:" nil t))
      (nreverse res))))
;;(re-search-forward "^NAME\\s-*:\\s-*\\([^,\n]*\\),\\s-*\\(.*\\)\\s-*$" nil t)

(defun* mb-refresh-addresses (&optional (address-buffer "adresser.txt"))
  "Returns a refreshed address list."
  (setq *mb-address-book*
	(append *mb-address-book-extra*
		(with-buffer address-buffer (mb-parse-addresses)))))
;;(mb-refresh-addresses)

(defvar *mb-address-book* nil)

(defun mb-address-book (refresh)
  "Returns *mb-address-book*. If *mb-address-book* is nil or refresh
is non-nil, *mb-address-book* is reparsed."
  (interactive "P")
  (when (or refresh (not *mb-address-book*)) 
    (setf *mb-address-book* (mb-refresh-addresses)))
  *mb-address-book*)
;;(mb-address-book t)

(defun mb-query-address-base (&optional prompt refresh)
  "Completes an address NAME from `mb-address-book' in the minibuffer
and returns the corresponding e-mail address. For prefix argument, see
mb-address-book."
  (let* ((address-book (mb-address-book refresh))
	 (name (completing-read 
	       (or prompt "Name: ")
	       address-book nil nil 
	       (or (word-at-point) ""))))
    (or (assoc name address-book)
	(list nil name))))
;;(mb-query-address-base)

(defun mb-query-address (&optional prompt refresh)
  "Completes an address NAME from `mb-address-book' in the minibuffer
and returns the corresponding e-mail address. For prefix argument, see
mb-address-book."
  (multiple-value-bind (name address) (mb-query-address-base prompt refresh)
    (if name (format "%s <%s>" name address) address)))
;;(mb-query-address nil)

(defun mb-query-address-insert (refresh)
  "Inserts at point the result of `mb-query-address' \(see this
function for prefix argument)."
  (interactive "P")
  (insert (mb-query-address refresh)))
;;(mb-query-address-insert nil)

(defconst *mb-sent-messages-db*
  '(('to-list 'cc-list 'bcc-list 'from 'host 'time 'subject 'attachment-list)
    ("Mats Mobil Bergstrøm" "4793433496@sms.netcom.no")))

(defun mail-buffer-collect-parts ()
  "Converts mail buffer to a list (TO SUBJECT MESSAGE). Assumes the mail buffer is active"
  (let ((to "") 
	(subject "")
	(message ""))
    (goto-char (point-min))
    (re-search-forward "^To: \\(.*\\)$")
    (setq to (match-string 1))
    (re-search-forward "^Subject: \\(.*\\)$")
    (setq subject (match-string 1))
    (re-search-forward "^--text follows this line--\n")
    (setq message (buffer-substring (point) (point-max)))
    (list to subject message)))

(defun message-send-and-exit-hydro ()
  "TODO replace re-search with message-goto- methods"
  (interactive)
  (let ((dir "C:/projects/Mats/vb/MailPerNotes/")
	(temp-name "temp.txt")
	(parts (mail-buffer-collect-parts)))
    (setq prm (string-make-unibyte (format "%s\n%s\n%s" to subject message)))
    (when (>= (length prm) (expt 2 15))
      (print* message (concat dir temp-name))
      (setq prm (string-make-unibyte (format "%s\n%s\n%s" to subject (format "<%s>" temp-name)))))    
    (message prm)
    (call-process (concat dir "MailPerNotes.exe") nil "*Messages*" nil prm)
    (bury-buffer)))

(defun message-send-and-exit-outlook ()
  "TODO replace re-search with message-goto- methods"
  (interactive)
  (let ((dir "c:/Documents and Settings/matsb/My Documents/projects/vb/SendMail/")
	(temp-name "temp.txt")
	(parts (mail-buffer-collect-parts))
	(prm ""))
    (setq prm (string-make-unibyte (apply #'format "%s\n%s\n%s" parts)))
    (when (>= (length prm) (expt 2 15))
      (print* (third parts) (concat dir temp-name))
      (setq prm (string-make-unibyte (format "%s\n%s\n%s" (first parts) (second parts) (format "<%s>" temp-name)))))    
    (call-process (concat dir "SendMail.exe") nil "*Messages*" nil prm)
    (bury-buffer)))
;(call-process "C:/projects/Mats/vb/notescom/NotesCom.exe" nil "*Messages*" nil (format "mb;luringer;%s" (make-string (expt 2 15) ?a)))

;(setq message-send-and-exit-method #'message-send-and-exit-hydro)
;(setq message-send-and-exit-method #'message-send-and-exit-outlook)
;(setq message-send-and-exit-method nil)

(require 'gnus)
(require 'message)
(define-key message-mode-map "\C-c\C-c"
  #'(lambda () (interactive) (funcall message-send-and-exit-method)))

(provide 'mb-mail)

;;(call-process "C:/projects/Mats/vb/MailPerNotes/MailPerNotes.exe" nil "*Messages*" nil (string-make-unibyte "mats.bergstrom@hydro.com\nhello world\nasødlf aøs døalk døalkd\naslkd øalkdf sdf aødjøalkjd ølakjdfølkadøflkjadjføladøljajdfløka a lkfdjlskdf lksdjf"))


;;     (when (>= (length prm) (expt 2 15))
;;       (print* message (concat dir temp-name))
;;       (setq prm (string-make-unibyte (format "%s\n%s\n%s" to subject (format "<%s>" temp-name)))))

;;   (let ((to "") (subject "") (message "") (prm "")
;; 	(dir "C:/projects/vb/SendMail/") (temp-name "temp.txt"))
;;     (goto-char (point-min))
;;     (re-search-forward "^To: \\(.*\\)$")
;;     (setq to (match-string 1))
;;     (re-search-forward "^Subject: \\(.*\\)$")
;;     (setq subject (match-string 1))
;;     (re-search-forward "^--text follows this line--\n")
;;     (setq message (buffer-substring (point) (point-max)))
    
;;     (when (>= (length prm) (expt 2 15))
;;       (print* message (concat dir temp-name))
;;       (setq prm (string-make-unibyte (format "%s\n%s\n%s" to subject (format "<%s>" temp-name)))))    
;;     (message prm)
;;     (call-process (concat dir "SendMail.exe") nil "*Messages*" nil prm)
;;     (bury-buffer))
