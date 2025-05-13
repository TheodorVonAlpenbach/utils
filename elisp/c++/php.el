(require 'simple)
(require 'php-mode)

(add-hook 'sgml-mode-hook
  (lambda ()
    ;; Default indentation is usually 2 spaces, changing to 4.
    ;; (setq indent-line-function 'indent-relative)
    (setq sgml-basic-offset 4)))
;;(nilf sgml-mode-hook)

;; (add-to-list 'c-default-style '(php-mode . "php"))
(add-hook 'php-mode-hook 'mb-php-mode-hook)
(cl-defun mb-php-mode-hook ()
  "My PHP mode configuration."
  (display-line-numbers-mode)
  (setq indent-tabs-mode nil
        c-default-style "pear"
        tab-width 4
        c-basic-offset 4))

(cl-defun php-mode-p ()
  (equal major-mode 'php-mode))

(defadvice php-evil-indent (after qwe first (beg end))
  (if (and (php-mode-p) (php-in-sgml-p))
    (progn (sgml-mode)
	   (evil-indent beg end)
	   (php-mode))
    (evil-indent beg end)))

(cl-defun php-in-sgml-p ()
  "Return NIL if POINT is not in an SGML section."
  (>= (save-excursion (or (re-search-backward "[?]>" nil t) 0))
     (save-excursion (or (re-search-backward "<\\?php" nil t) 0))))
;;(php-in-sgml-p)

(cl-defun php-insert-log-string (s)
  (when s
    (move-beginning-of-line 1)
    (next-line 1)
    (insert (format "ldiv\\log(%s, '%s');" s s))
    (c-indent-line-or-region)
    (insert "\n")))

(cl-defun php-insert-Logger-string (s)
  (when s
    (move-beginning-of-line 1)
    (next-line 1)
    (insert (format "LabCommon_Logger::dlog(%s, '%s::%s');" s (php-scope-string) s))
    (c-indent-line-or-region)
    (insert "\n")))

(cl-defun php-insert-echo-log-string (s)
  (when s
    (move-beginning-of-line 1)
    (next-line 1)
    (insert (format "echo '%s: ' . \"%s\\n\";" s s))
    (c-indent-line-or-region)
    (insert "\n")))

(cl-defun php-insert-echo-string (s)
  (when s
    (move-beginning-of-line 1)
    (next-line 1)
    (insert (format "$this->log(%s, '%s: ');" s s))
    (c-indent-line-or-region)
    (insert "\n")))

(cl-defun php-insert-log-symbol-at-point (&optional (method #'php-insert-echo-log-string))
  "Inserts an log from symbol at point"
  (funcall method (symbol-at-point)))
;;(php-insert-log-symbol-at-point nil)

(cl-defun php-insert-log-region (&optional (method #'php-insert-echo-log-string))
  "Inserts an log from active region string"
  (funcall method
   (buffer-substring-no-properties (region-beginning) (region-end))))

(cl-defun php-insert-echo-kilroy ()
  (interactive "")
  (move-beginning-of-line 1)
  (next-line 1)
  (insert (format "$this->log('Kilroy was at line %d in %s');" (line-number-at-pos) (buffer-name)))
  (c-indent-line-or-region)
  (insert "\n"))

(cl-defun php-insert-log (&optional (method #'php-insert-Logger-string))
  (interactive "")
  (save-excursion
    (if (use-region-p)
      (php-insert-log-region method)
      (php-insert-log-symbol-at-point method))))
;;(php-insert-log-region)

;; mb adds
(cl-defun php-backward-kill-word (arg)
  "Same as backward-kill-word, but stops at $ if this is the
first character"
  (interactive "p")
  (let ((end (point)))
    (backward-word arg)
    (when (eq (char-after) ?$)
      (forward-char 1))
    (kill-region (point) end)))

(cl-defun php-insert-curly-brackets (n)
  (interactive "P")
  (insert-curly-brackets n)
  (indent-for-tab-command)
  (newline 2)
  (indent-for-tab-command)
  (previous-line 1)
  (indent-for-tab-command))

(cl-defun php-prepend-this ()
  (interactive)
  (save-excursion
    (backward-word 1)
    (insert "$this->")))

(cl-defun php-current-scope ()
  (save-excursion
    (unless (condition-case nil
		(backward-up-list 1)
	      (error t))
      (backward-sexp 1)
      (let ((string (thing-at-point 'sexp)))
	(if (member* string '("try") :test #'string=)
	  (intern string)
	  (if (eq ?\( (char string 0)) ;;left parenthesis: function, if foreach etc
	    (progn 
	      (backward-sexp 1)
	      (let ((token (substring-no-properties (thing-at-point 'sexp))))
		(if (member* token '("foreach" "if" "else") :test #'string=)
		  (list (intern token))
		  (progn
		    (backward-sexp 1)
		    (let ((token2 (thing-at-point 'sexp)))
		      (if (member* token2 '("function") :test #'string=)
			(list (intern token2) token)
			(progn
			  (re-search-backward "class")
			  (forward-sexp 1)
			  (forward-char 1)
			  (substring-no-properties (thing-at-point 'sexp)))))))))
	    (progn
	      (re-search-backward "class")
	      (forward-sexp 1)
	      (forward-char 1)
	      (list 'class (substring-no-properties (thing-at-point 'sexp))))))))))

(cl-defun php-scope ()
  (nreverse
   (save-excursion
     (cl-loop for scope = (php-current-scope)
	   while scope
	   do (backward-up-list 1)
	   collect scope))))

(cl-defun php-scope-string ()
  (let* ((scope (mapcar #'second (subseq (php-scope) 0 2)))
	 (outer-scope (first scope))
	 (inner-scope (second scope)))
      (unless outer-scope
	(setq outer-scope (buffer-name)))
      (if inner-scope
	(format "%s::%s()" outer-scope inner-scope)
	(format "%s" outer-scope))))

(cl-defun php-format-debug-print-backtrace ()
  "Assumes that the buffer contains output from the PHP function
  debug_print_backtrace()." 
  (interactive)
  (let ((lines (split-string-2 (buffer-string) "\#[0-9]+ " nil nil)))
    (message "%s" lines))
  )
;;(split-string-2 "asdf #1 asdfa ølakf #2 " "\#[0-9]+ " nil nil)

(define-key php-mode-map
  '[(meta backspace)]
  'php-backward-kill-word)

(define-key php-mode-map
  "{"
  'php-insert-curly-brackets)

(define-key php-mode-map
  "["
  'insert-square-brackets)

(define-key php-mode-map
  "å"
  'php-prepend-this)

(cl-defun lab-base-url-archived-article (environment)
  "Returns the front article base url for an archived (escenic)
article in ENVIRONMENT."
  (case environment
    (:vbox "http://localhost/labrador2.git/front/2013/11/15/kristenliv/maria/jesus")
    (:dev "http://www.tv2.no.dev.lbrdr.com/2013/11/21/nyheter")
    (:stage (error))
    (:prod (error))))
;;(lab-base-url-archived-article :stage)

(cl-defun lab-base-path-archived-article (environment)
  "Returns the front article base url for an archived (escenic)
article in ENVIRONMENT."
  (case environment
    (:vbox "/cygdrive/c/Users/mat_ber/Documents/WinShare/labrador2.git/escenic_storage/published")
    (:dev "ssh pbl tidy -xml -indent -quiet /www/publish/customers/tv2/stage/escenic_storage/published")
    (:stage (error))
    (:prod (error))))
;;(lab-base-url-archived-article :stage)

(cl-defun open-tv2-escenic-html (id &key (debug nil) (environment :vbox))
  "ENVIRONMENT is one of :VBOX (default), :DEV, :STAGE, :PROD."
  (let* ((debug-http-query-parameter (if debug "&XDEBUG_SESSION_START=1" ""))
	 (base-url (lab-base-url-archived-article environment))
	 (url (format "%s/%s?tv2escenic%s" base-url id debug-http-query-parameter)))
    (browse-url url)))

(cl-defun tv2-escenic-directory (&optional (environment :vbox))
  (case environment
    (:vbox "/cygdrive/c/Users/mat_ber/Documents/WinShare/labrador2.git/escenic_storage/published")
    (:dev "/www/publish/customers/tv2/stage/escenic_storage/published")
    (t (error "Environment %S is not supported!" environment))) )

(cl-defun open-tv2-escenic-xml (id &key (environment :vbox))
  "TODO: encodings"
  (let* ((buffer-name (format "*%d.xml (%s)*" id (upcase (substring (symbol-name environment) 1))))
	 (sub-dir-number (/ id 10000))
	 (base-path (tv2-escenic-directory environment))
	 (path (format "%s/%d/%d.xml" base-path sub-dir-number id))
	 (script (case environment
		      (:vbox "tidy")
		      (:dev "ssh")))
	 (tidy-args (list "-xml" "-indent" "-quiet" path))
	 (args (case environment
		 (:vbox tidy-args)
		 (:dev (append '("pbl" "tidy") tidy-args)))))
    (unless (get-buffer buffer-name)
      (message "call-process %s %s" script (concat* args :in " "))
      (apply #'call-process script nil (get-buffer-create buffer-name) t args))
    (switch-to-buffer buffer-name)
    (xml-mode)
    (goto-char 1)))
;;(open-tv2-escenic-xml 3204370 :environment :dev)

(cl-defun tv2-get-ids (&optional (environment :vbox))
  (let* ((dir (tv2-escenic-directory environment))
	 (files (case environment
		  (:vbox (shell-command-to-string (format "find %s -type f" dir)))
		  (:dev (tv2-get-ids :vbox)) ;; same
		  (t (error "Environment %S is not supported" environment))))
	 (ids (mapcar #'(lambda (x) (nth* -2 (split-string x "[/.]"))) (string-lines files))))
    ids))
;;(tv2-get-ids :vbox)

(cl-defun is-nontrivial-directory (x)
  (and (file-directory-p x)
       (let ((basename (file-name-nondirectory x)))
	 (and (string/= basename ".")
	      (string/= basename "..")))))
;;(is-nontrivial-directory "/cygdrive/c/")

(cl-defun rscandir (directory from-level to-level current-level)
  (when (< from-level to-level)
    (let* ((items (directory-files directory t))
	 (subitems (cl-loop for x in items
			 if (and (< current-level to-level)
				 (is-nontrivial-directory x))
			 append (rscandir x from-level to-level (1+ current-level)))))
    (if (< current-level from-level)
      subitems
      (append items subitems)))))
;;(rscandir "/cygdrive/c/Users/mat_ber/Google Drive/site-lisp/mb-lisp/c++/" 1 2 0)

(provide 'php)
