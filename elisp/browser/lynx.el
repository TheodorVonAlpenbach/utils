(require 'shell)
(require 'mb-utils-div)
(require 'mb-utils-io)
(require 'mb-utils-strings)
(require 'mb-locale)

;; Define varibles, read config, set defaults
(defvar *lynx-highlight-always* nil)
(defvar *lynx-home-url* nil)
(defvar *lynx-proxy-dir* ())
(defvar *lynx-proxy-db-file* ())
(defvar *lynx-favorites-file* ())

(setf *lynx-proxy-dir* (expand-file-name "lynx-proxy" *local-data-dir*))
(setf *lynx-proxy-db-file* (expand-file-name ".proxy-db" *lynx-proxy-dir*))
(setf *lynx-favorites-file* ".lynx-favorites")
;;(setf *ie-prog* (expand-file-name "vb/VBGet/VBGet.exe" *local-projects-dir*))

(require 'lynx-utils)
(require 'lynx-proxy)
(require 'lynx-favorites)
(require 'lynx-quiz)
(require 'lynx-aftenposten)

(defvar *lynx-history* ())
(defconst *lynx-buffer* "*Lynx*")
(defvar *lynx-proxy-db*
  (make-lynx-proxy-db)
  "Lisp object representation of a proxy database. '(ITEM1 ITEM2 ...),
 where each item is a list of form '(URL LYNX-FILE POINT). LYNX-FILE is
 the result of 'lynx.exe -dump URL', where URL is a normal http url.
 POINT is the point position in the *Lynx* buffer the last time the
 LYNX-FILE was visited. This incurs smoother effect of some navigate
 functions, for instance LYNX-GOTO-LAST.
 ")

(setf *lynx-highlight-always* nil)
(setf *lynx-home-url* "http://www.aftenposten.no/nyheter/siste100/")

(defconst *lynx-debug-mode* nil)
(defconst *lynx-config* (format "-cfg=%s" (expand-file-name "lynx.cfg" *lynx-config-dir*)))
(defconst *lynx-lss* (format "-lss=%s" (expand-file-name "lynx.lss" *lynx-config-dir*)))

(defstruct (lynx-job (:conc-name lj-))
  "Structure for storing job info on job stack. URL is where to fetch
the file, REFRESHP is non-nil iff the data should be refreshed if it
exist, and SHOWP is non-nil iff the data should be view."
  (url *lynx-home-url*)
  (refreshp nil)
  (showp nil))

(defvar *lynx-url-jobs* ()
  "Stack with LYNX-JOB elements refreshp showp\), where ")

(defun lynx-mode () "Major mode for browsing urls with lynx. Creates a
   new lynx-mode buffer for each browse. At visit, the buffer is
   parsed, the url-refs are collected in file local varible local-url-refs
   (or something) and highlighted. The refs should be easy visited by
   either scroll with TAB + RET, or prefix + command.

   Later: 1. max-number of lynxbuffers; 2. history

 \\{lynx-mode-map}
 \\<lynx-mode-map>"
  (interactive)
  (kill-all-local-variables)
  (use-local-map lynx-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'lynx-mode)
  (setq mode-name "Lynx mode")
  (setq buffer-offer-save t)		;but why?
  (set (make-local-variable 'font-lock-defaults) '(lynx-font-lock-keywords))
  (set (make-local-variable 'lynx-local-url-refs) ())
  (set (make-local-variable '*lynx-current-url*) nil)
  (set (make-local-variable 'lynx-current-proxy-refs-regexp<) ())
  (set (make-local-variable '*lynx-history*) ())
  (set (make-local-variable '*lynx-home-url*) *lynx-home-url*)
  (set (make-local-variable '*lynx-url-jobs*) *lynx-url-jobs*)
  (setf fill-prefix "   ")
  (run-hooks 'text-mode-hook 'lynx-mode-hook))
(put 'lynx-mode 'font-lock-defaults '(lynx-font-lock-keywords))

(defvar lynx-font-lock-keywords		;(nil! lynx-font-lock-keywords)
  (purecopy
   (list (list *lynx-ref-regexp* 0 font-lock-variable-name-face 'append)
	 (list 'lynx-match-proxy-ref 0 font-lock-keyword-face 'prepend)))
  "Additional expressions to highlight in Lynx mode.")

(defun lynx-match-proxy-ref (bound)
  "Searches forward to POINT for lynx references in proxy. Used by
 `font-lock-mode'."
  (re-search-forward lynx-current-proxy-refs-regexp bound t))
;;(cancel-debug-on-entry 'lynx-match-proxy-ref)

(defun* lynx-interactive ()
  "Reads url from command line."
  `(,(let ((completion-ignore-case t)
	   (default (first *lynx-favorites*)))
	  (completing-read "sURL: " *lynx-favorites* nil t
			   (cons default (length default))))))
;; mode map
(defvar lynx-mode-map () "Keymap used in lynx mode.")
(when (not lynx-mode-map)		;(nilf lynx-mode-map)
  (let ((map (make-sparse-keymap))
	(print-map (make-sparse-keymap))
	(goto-map (make-sparse-keymap))
	(load-map (make-sparse-keymap))
	(edit-map (make-sparse-keymap))
	(clear-map (make-sparse-keymap)) ;ie. delete map
	(add-map (make-sparse-keymap))
	(save-map (make-sparse-keymap))
	(jobs-map (make-sparse-keymap))
	(process-map (make-sparse-keymap))
	(browse-map (make-sparse-keymap))
	(move-map (make-sparse-keymap)))

    ;; tree
    (define-key map [(control ?>)] #'lynx-tree-browse-next-sibling)
    (define-key map [(control ?<)] #'lynx-tree-browse-previous-sibling)

    (define-key map [(? )] #'scroll-up)
    (define-key map [(shift ? )] #'scroll-down)
    (define-key map "q" #'lynx-browse-silent-url-at-point)
    (define-key map "Q" #'lynx-refresh-silent-url-at-point)
    (define-key map "r" #'lynx-refresh)

    ;;; other keys
    (define-key map "M" #'lynx-mail-region)
    (define-key map "k" #'lynx-insert-region-in-quiz-outline)
    (define-key map "K" #'lynx-insert-region-in-quiz-scratch)
    (define-key map [(shift tab)] #'backward-lynx-reference)
    (define-key map "\t" #'forward-lynx-reference)
    (define-key map ">" #'forward-lynx-reference)
    (define-key map "<" #'backward-lynx-reference)

    ;; maps
    (define-key map "g" goto-map)
    (define-key map "l" load-map)
    (define-key map "p" print-map)
    (define-key map "P" process-map)
    (define-key map "e" edit-map)
    (define-key map "c" clear-map)
    (define-key map "a" add-map)
    (define-key map "s" save-map)
    (define-key map "j" jobs-map)
    (define-key map "m" move-map)
    (define-key map "b" browse-map) ; *DIC-MAP* is defined in dictionary.el
    (define-key map "d" *dic-map*) ; *DIC-MAP* is defined in dictionary.el

    ;; goto map (g)
    (define-key goto-map " " #'lynx-browse-url-at-point)
    (define-key goto-map "g" #'lynx-recenter)
    (define-key goto-map "f" #'lynx-goto-favorite)
    (define-key goto-map "h" #'lynx-goto-home)
    (define-key goto-map "l" #'lynx-goto-last)
    (define-key goto-map "a" #'lynx-browse) ;memo: a -> address
    (define-key goto-map "A" #'lynx-browse-url-no-proxy) ;memo: a -> address
    (define-key goto-map "r" #'lynx-goto-url-with-ref)
    ;tree
    (define-key goto-map "u" #'lynx-tree-goto-parent)
    (define-key goto-map "n" #'lynx-tree-goto-next-sibling)
    (define-key goto-map "p" #'lynx-tree-goto-previous-sibling)

    ;; move map (m)
    (define-key move-map "e" #'lynx-move-to-last-article)

    ;; load map (l)
    (define-key load-map "r" #'lynx-download-from-urls-in-region)
    (define-key load-map " " #'lynx-download-from-url-at-point)
    (define-key load-map "a" #'lynx-download-all-urls)

    ;; print map (p)
    (define-key print-map "t" #'lynx-print-test)
    (define-key print-map "u" #'lynx-print-current-url)
    (define-key print-map "U" #'lynx-print-referred-url)
    (define-key print-map "H" #'lynx-print-history)
    (define-key print-map "h" #'lynx-print-home)
    (define-key print-map "f" #'lynx-print-favorites)
    (define-key print-map "e" #'lynx-print-current-entry)
    (define-key print-map "E" #'lynx-print-entry-at-point)
    (define-key print-map "d" #'lynx-print-db)

    ;; jobs map (j)
    (define-key jobs-map "p" #'lynx-print-jobs)
    (define-key jobs-map "d" #'lynx-delete-jobs)
    (define-key jobs-map "s" #'lynx-skip-job)

    ;; edit map (e)
    (define-key edit-map "s" #'lynx-proxy-find-file-scratch)

    ;; clear proxy map (c)
    (define-key clear-map "d" #'lynx-proxy-clear-all)
    (define-key clear-map "A" #'lynx-proxy-clear-aftenposten)
    (define-key clear-map "f" #'lynx-delete-favorite)

    ;; add map (a)
    (define-key add-map "f" #'lynx-add-favorite)

    ;; save map (s)
    (define-key save-map "d" #'lynx-proxy-save-db)
    (define-key save-map "f" #'lynx-save-favorites)

    ;; process map (P)
    (define-key process-map "l" #'list-processes)
    (define-key process-map "p" #'list-processes)
    (define-key process-map "d" #'lynx-delete-lynx-processes)
    (define-key process-map "D" #'lynx-delete-all-processes)
    
    ;; forward to another browser (currently ie)
    (define-key browse-map "u" #'lynx-browse-current-url)
    (define-key browse-map "w" #'lynx-browse-wiki)
    (define-key browse-map "m" #'lynx-browse-map)
    (define-key browse-map "p" #'lynx-browse-url-at-point)

    (setq lynx-mode-map map)))

;; When a lynx buffer is killed, make sure the associated db
;; is killed too.
(defun lynx-kill-buffer ()
  (when (eql major-mode 'lynx-mode)
    (lynx-save-favorites)
    (lynx-proxy-save-db)))
(add-hook 'kill-buffer-hook 'lynx-kill-buffer)
(add-hook 'kill-emacs-hook 'lynx-kill-buffer)


;;; print methods
(defun lynx-browse-current-url ()
  (interactive)
  (browse-url *lynx-current-url*))

(defun lynx-browse-map ()
  (interactive)
  (browse-url (format "https://maps.google.no/maps?hl=no&q=maps+%s" (thing-at-point 'word))))

(cl-defun lynx-browse-wiki (&optional lang-number)
  (interactive "P")
  (browse-url (format (case current-prefix-arg
			(1 "https://no.wikipedia.org/wiki/%s")
			(t "http://en.wikipedia.org/wiki/%s"))
		(or (marked-text) (thing-at-point 'word)))))

(defun lynx-print (var) 
  (interactive "vVariable: ")
  "Prints variable VAR in temporary buffer *lynx-info*."
  (with-output-to-temp-buffer "*lynx-info*" (princ var))
  (when current-prefix-arg (kill-new var)))

(defun lynx-print-current-url () 
  (interactive)
  "Prints current url"
  (lynx-print *lynx-current-url*))

(defun lynx-print-referred-url () 
  (interactive)
  "Prints url referred to at point."
  (lynx-print (message "%s" (lynx-url-from-reference (lynx-reference-active-at-point)))))

(defun lynx-print-history ()
  (interactive)
  "Prints history of visited sites."
  (lynx-print *lynx-history*))

(defun lynx-print-home ()
  (interactive)
  "Prints home site url."
  (lynx-print *lynx-home-url*))

(defun desc-var (var) ""
       (interactive "SVariable: ")
       (message (setf var var)))

(defun* lynx (&optional url other-window)
  "Invokes lynx with START-URL and enters lynx-mode. Starts in OTHER-WINDOW
 if this option is non-nil."
  (interactive)
  (if other-window
    (switch-to-buffer-other-window (get-buffer-create *lynx-buffer*))
    ;;else
    (switch-to-buffer (get-buffer-create *lynx-buffer*)))
  (if (eq major-mode 'lynx-mode)
    (when url
      (lynx-browse url))
    ;;else
    (progn
      (lynx-mode)
      (setq *lynx-proxy-db* (lynx-proxy-read-db))
      (lynx-read-favorites)
      (lynx-browse (or url *lynx-home-url*)))))
;;(lynx "c:/unix/doc/HyperSpec/Body/fun_zerop.html" t)

(defun lynx-update-proxy-refs-regexp ()
  "Updates 'LYNX-CURRENT-PROXY-REFS-REGEXP."
  (setq lynx-current-proxy-refs-regexp
	(concat* (lynx-references-in-proxy) 
		 :pre "\\[\\(" :in "\\]\\|" :suf "\\]\\)")))

(defun* lynx-before-print-html (&optional refresh (url *lynx-current-url*))
  "Make this a hook later."
  (recenter 1)
  (when (or refresh (zerop (lpe-point (lpdb-entry url))))
    (lynx-recenter url)))

(defun* lynx-recenter (&optional (url *lynx-current-url*))
  "TODO: handling of site modes like aftenposten and sn should be
done transparently"
  (interactive)
  (cond 
     ((lynx-aftenposten-p *lynx-current-url*)
      (lynx-aftenposten-goto-article-begin)
      (lynx-aftenposten-format-paragraphs))
     ((url-sn-p *lynx-current-url*)
      (lynx-sn-goto-article-begin))))

(defun lynx-convert-path (path)
  (string-replace path "^.:/" "/cygdrive/c/"))

(defun* lynx-load-file (filename &key (buffer *lynx-buffer*) (url ""))
  "Processes local html file at FILENAME. Only exception is files
downloaded from Aftenposten which are already processed in proxy and
therefore loaded unmodified."
  (if (lynx-aftenposten-p url)
    (with-buffer buffer
      (insert-file-contents filename))
    (message (concat *lynx-config* "-dump" (lynx-convert-path filename) "-force_html"))
    (call-process *lynx-prog* nil buffer t *lynx-config* "-dump" (lynx-convert-path filename) "-force_html")))
;;(call-process "lynx" nil "qwe" t  *lynx-config* "-dump" (lynx-convert-path "c:/unix/data/lynx-proxy/13-artid=464394") "-force_html")
;;(process-coding-system process)

(defun* lynx-print-html (filename &key pos (buffer *lynx-buffer*) (url ""))
  "Processes html in FILENAME with Lynx and flushes output to buffer."
  (with-buffer* buffer
    (blank-buffer)
    (font-lock-mode -1)
    (lynx-load-file filename :buffer buffer :url url)
    (lynx-update-proxy-refs-regexp)
    (font-lock-mode)
    (unless (zerop pos)
      (goto-char pos))
    (lynx-before-print-html))
  (lynx-browse-jobs))
;;(lynx-print-html "c:/unix/clisp/doc/clisp.html" :buffer (get-buffer-create "out"))

(defun* lynx-download-to-buffer-lynx (buffer url &key (type :dump) (reload nil) (width 80) (process-name "lynx-b"))
  "Downloads content at URL to BUFFER using lynx. See `lynx-download'
for key parameters."
  (let ((lynx-arguments
	 (list *lynx-config*
	       (case type (:source "-source") (:dump "-dump"))
	       (if reload "-reload" "")
	       (format "-width=%d" width)
	       *lynx-config*
	       *lynx-auth*
	       url)))
    (message (concat* lynx-arguments :in " "))
    (apply #'start-process-shell-command 
	   process-name buffer *lynx-prog* lynx-arguments)))
;;(start-process-shell-command "lynx-b" "qwe" *lynx-prog* "-dump" "http://www.m-w.com/cgi-bin/dictionary?estuary")

(defun* lynx-download-ie (url filename &key (type :source) (reload nil) 
			      (width (if (lynx-aftenposten-article-p url) 50 80)))
  "Downloads content at URL to FILENAME using IE6. See `lynx-download'"
  (start-process-shell-command "ie" nil *ie-prog* url (format "\'%s\'" filename)))
;;(start-process-shell-command "ie" nil *ie-prog* "http://www.storenorskeleksikon.no/sa.aspx?artid=775719url" "qwe")

(defun* lynx-download-lynx (url filename &key
				(type :source)
				(reload nil)
				(width (if (lynx-aftenposten-article-p url) 50 80))
				(display-charset 'utf-8)
				(buffer nil))
  "Downloads content at URL to FILENAME using lynx. See `lynx-download'"
  (when (file-exists-p filename)
    (delete-file filename))
  (let* ((lynx-arguments
	  (list *lynx-config*
		*lynx-lss*
		(case type (:source "-source") (:dump "-dump"))
		"-reload"
		(format "-width=%d" width)
		(format "-display_charset=%s" display-charset)
		url ">" (format "\"%s\"" filename)))
	 (message (concat* lynx-arguments :in " ")))
    (apply #'start-process-shell-command 
	   "lynx" buffer *lynx-prog* lynx-arguments)))
;;(lynx-download-lynx "http://www.aftenposten.no/nyheter/iriks/Rektor-foler-seg-brukt-i-politisk-spill--7155282.html" (concat *mb-lisp-dir* "browser/test.txt") :type :dump :buffer "qwe")
;;(start-process-shell-command "lynx" "qwe" "lynx.exe" "-dump" "-reload" "http://www.aftenposten.no/nyheter/siste100/")
;;(start-process-shell-command "lynx" "qwe" "c:/cygwin/bin/lynx.exe" "-dump" "-reload" (url-encode "http://www2.nrk.no/spillelister40/sending.aspx?prog=41&tid=2003-05-27%2000:00:00Z"))


(defun* lynx-download (url filename &key (type :source) (reload nil)
			   (width (if (lynx-aftenposten-article-p url) 50 80)))
  "Downloads www page at URL and stores result in FILENAME. Asynchronous
process. Returns the process of #'START-PROCESS-SHELL-COMMAND. TYPE
may be either the default :SOURCE or :DUMP."
;  (if (lynx-aftenposten-p url)
    (lynx-download-lynx url filename :type :dump :reload reload :width width)
;    (lynx-download-ie url filename :type type :reload reload :width width)
;    )
)

(defun* lynx-show-html (filename &key pos (buffer *lynx-buffer*) (url ""))
  "Processes html in FILENAME with Lynx and flushes output to buffer."
  (message "lynx-show-html: printing %s, width=%s" url (if (lynx-aftenposten-article-p url) 50 80))
  (with-buffer* buffer
    (blank-buffer)
    (font-lock-mode -1)
    (call-process *lynx-prog* nil buffer t *lynx-config* "-dump" filename
		  "-force_html" (format "-width=%d" width))
    (lynx-update-proxy-refs-regexp)
    (font-lock-mode)
    (goto-char pos)
    (run-hooks 'lynx-show-html-hook)))
;;(lynx-show-html (concat *mb-lisp-dir* "/browser/test.txt") :buffer "ewq")

(defun lynx-show-entry (entry)
  (lynx-show-html (lpe-abs-filename entry) 
		  :pos (lpe-point entry) 
		  :url (substring-no-properties (lpe-url entry))))

(defun* lynx-update-lpdb (entry job)
  "Obsolete?"
  (setf (lpe-point entry) 0) ;"reset" point
  (set-process-sentinel
   (lynx-download (lpe-url entry) (lpe-abs-filename entry)
		  :type (if (lynx-aftenposten-p (lpe-url entry)) :dump :source))
   (lynx-update-lpdb-sentinel entry job)))

(defun lynx-print-entry (entry)
  (lynx-print-html (lpe-abs-filename entry)
		   :pos (lpe-point entry) 
		   :url (string-remove-props (lpe-url entry))))

(defun lynx-fontify ()
  (with-buffer *lynx-buffer*
    (lynx-update-proxy-refs-regexp)
    (font-lock-fontify-block)))

(defun lynx-update-lpdb-sentinel-body (event entry job) 
  "TODO: assure that process was ok."
  (if (string= event "finished\n")
    (progn
      (unless (lynx-aftenposten-p (lpe-url entry))
	;;code conversion
	(let ((coding-system-for-write 'no-conversion)) 
	  (with-file (lpe-abs-filename entry)
	    (html-subst (lpe-url entry)))))
      (if (lj-showp job)
	(lynx-print-entry entry)
	(lynx-fontify)))
    ;; else flag an error
    (message "Loading of entry %S failed. Continuing..." entry)))

(defun lynx-update-lpdb-sentinel (entry job) 
  "TODO: assure that process was ok."
  (lexical-let ((entry entry) (job job))
    (function
     (lambda (process event)
      (lynx-update-lpdb-sentinel-body event entry job)
      (lynx-browse-jobs)))))

(defun lynx-browse-jobs ()
  "Looks at *LYNX-URL-JOBS*. Creates processes and corresponding
sentinels for loading the urls in queue. See #'LYNX-BROWSE-SILENT."
  ;; skip already loaded urls
  (when (> (length *lynx-url-jobs*) max-lisp-eval-depth)
    (error "Job list (%d items) is longer than max-lisp-eval-depth (%d items)"
     (length *lynx-url-jobs*) max-lisp-eval-depth))
  (if *lynx-url-jobs*
    (let* ((job (pop *lynx-url-jobs*))
	   (entry (lpdb-entry (lj-url job))))
      ;; first handle global variables
      (awhen (lpdb-entry *lynx-current-url*)
	(setf (lpe-point it) (point)))
      (when (and (lj-showp job) 
		 (not (lj-refreshp job)))
	(push (lj-url job) *lynx-history*)
	(setq *lynx-current-url* (lj-url job)))

      ;; handle job
      (if (lpe-local-p entry)
	(if (lj-refreshp job)
	  (lynx-print-entry entry)) ;local, just view proxy
	;; extern URL
	(if (lj-refreshp job)
	  (lynx-update-lpdb entry job) ;external and refresh, sentinel takes care of job's view option
	  ;; not refresh
	  (if (file-exists-p (lpe-abs-filename entry)) 
	    (if (lj-showp job) 
	      ;; file exists in proxy, so
	      (lynx-print-entry entry) ;...view it if requested
	      ;; non in proxy
	      (lynx-browse-jobs)) ;... since in this case, there is nothing else to trigger popping from job stack
	    (lynx-update-lpdb entry job)))) ;same as operation as refresh
      (message "Finished prosessing job %S. %d jobs left in queue" 
	    job (length *lynx-url-jobs*)))
    (message "Finished. Job queue empty.")))

(defun lynx-browse-no-proxy (url)
  "TODO: make this an asynchronous process."
  (interactive "sURL: ")
  (with-buffer (get-buffer-create *lynx-buffer*)
    (goto-char 1)
    (call-process *lynx-prog* nil *lynx-buffer* t *lynx-config*
		  "-dump" url "-force_html")
    (blank-buffer :start (point))))

;;; navigatation methods
(defun lynx-goto-home ()
  "Loads home site"
  (interactive)
  (lynx-browse *lynx-home-url*))

(defun lynx-goto-last (n)
  "Loads last visited page."
  (interactive "P")
  (when (<= (length *lynx-history*) 1)
    (error "This is the first site node you visited. History is %s" *lynx-history*))
  ;; pop 1+ because #'LYNX-BROWSE push new url on history
  (lynx-browse (pop* *lynx-history* (1+ (or n 1)))))
;(cancel-debug-on-entry 'lynx-goto-last)

(defun lynx-goto-ref (ref)
  "Goes to cite [REF]."
  (interactive "nRef: ")
  (lynx-browse (lynx-url-from-reference ref)))

(defun lynx-url-at-point ()
  "Returns url referenced at point."
  (interactive)
  (lynx-url-from-reference (lynx-reference-active-at-point)))

(defun lynx-urls-in-region (beg end)
  "Returns list of all urls referenced in region."
  (interactive "r")
  (mapcar #'lynx-url-from-reference (lynx-references-in-region beg end)))

(defun lynx-make-jobs (urls &optional refreshp showp)
  "Makes a new lynx job from URLS."
  (loop for url in urls do
	(push (make-lynx-job :url url :refreshp refreshp :showp showp)
	      *lynx-url-jobs*)))

(defun lynx-make-jobs-region (beg end &optional refreshp showp)
  "Makes a new lynx job from all references in region between BEG and END"
  (lynx-make-jobs (lynx-urls-in-region beg end) refreshp showp))

(defun lynx-browse-urls-in-region (beg end)
  "Browses urls in region. Prefix argument forces refresh."
  (interactive "r")
  (lynx-make-jobs-region beg end current-prefix-arg t)
  (lynx-browse-jobs))

(defun lynx-download-from-urls-in-region (beg end)
  "Download silently pages from urls in region. Prefix argument forces refresh."
  (interactive "r")
  (lynx-make-jobs-region beg end current-prefix-arg)
  (lynx-browse-jobs))

(defun lynx-download-all-urls ()
  "Download silently pages from urls in region. Prefix argument forces refresh."
  (interactive)
  (when (not-empty (string-match* "aftenposten\\.no\\/nyheter.*\\/siste100\\/" *lynx-current-url*))
    (let ((region (lynx-aftenposten-siste-100-region)))
      (if (notany #'null region)
	(progn (lynx-make-jobs-region (first region) (second region) current-prefix-arg)
	       (lynx-browse-jobs))
	(message "Reference region is undefined.")))))

(defun lynx-browse (url)
  "Loads site at URL into current buffer (and deletes old site
content) using proxy-db. Later: Prefix argument forces refresh."
  (interactive "sURL: ")
  (lynx)
  (unless (and (eq url *lynx-current-url*)
	       (not current-prefix-arg))
    (lynx-make-jobs (list url) current-prefix-arg t)
    (lynx-browse-jobs)))

(defun lynx-skip-job (n)
  "Skips next job from QUEUE. If prefix argument N is given, skips
next N jobs."
  (interactive "P")
  (pop* *lynx-url-jobs* n)
  (lynx-browse-jobs))

(defun lynx-delete-jobs ()
  "Skips next job from QUEUE. If prefix argument N is given, skips
next N jobs."
  (interactive)
  (setq *lynx-url-jobs* ()))

(defun lynx-browse-url-at-point ()
  "Goes to cite referenced at point. Prefix argument forces refresh."
  (interactive)
  (lynx-browse (lynx-url-at-point)))

(defun lynx-download-from-url-at-point ()
  "Goes to cite referenced at point. Prefix argument forces refresh."
  (interactive)
  (lynx-make-jobs (list (lynx-url-at-point)) current-prefix-arg)
  (lynx-browse-jobs))

(defun* lynx-refresh ()
  "Refreshes current page."
  (interactive)
  (lynx-make-jobs (list *lynx-current-url*) t t)
  (lynx-browse-jobs))

;;; print methods
(defun lpe-print (url)
  "Prints lpdb entry corresponding to URL to help buffer."
  (with-output-to-temp-buffer "*lynx-entry*" 
    (princ (or (lpdb-entry url) "Not in proxy db."))))

(defun lynx-print-entry-at-point ()
  "Prints lpdb entry corresponding to url at point."
  (interactive)
  (lpe-print (lynx-url-at-point)))

(defun lynx-print-current-entry ()
  "Prints lpdb entry corresponding to current url."
  (interactive)
  (lpe-print *lynx-current-url*))

;; print
(defun lynx-print-db ()
  (interactive)
  (with-output-to-temp-buffer "*lynx-entry*"
    (princ (hash-table-print (lpdb-entries *lynx-proxy-db*)))))

(defun lynx-print-jobs ()
  (interactive)
  (lynx-print 
   (if *lynx-url-jobs* 
		*lynx-url-jobs*
		"No jobs in queue.")))
  
;;; post methods
(defun* lynx-request-post (url &key string args (name "lynx-post")
			       (buffer "*lynx-buffer*"))
  "Call lynx with form post arguments"
  (when (not string) 
    (setq string (concat* (mapcar #'(lambda (x)
				      (concat (first x) "=" (second x)))
				  args) 
			  :in "&")))
  (start-process-shell-command
   name buffer
   "echo -e " (concat "\"" string "\n---\n\"")  " | " 
   *lynx-prog* "-dump" *lynx-config* url "-post_data"))

(defun lynx-delete-processes (regexp)
  "Deletes all running processes whose name matches REGEXP"
  (loop for process in (process-list)
	if (or current-prefix-arg
	    (string-match regexp (process-name process)))
	do (delete-process process)))

(defun lynx-delete-all-processes ()
  "Deletes all running processes"
  (interactive)
  (lynx-delete-processes ""))

(defun lynx-delete-lynx-processes ()
  "Deletes all running lynx processes"
  (interactive)
  (lynx-delete-processes "lynx"))

;;(lynx-delete-all-processes)
(provide 'lynx)
