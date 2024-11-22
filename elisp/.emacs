;; Redirected from one of the following
;; c:/Users/Theodor/AppData/Roaming/.emacs
;; c:/Documents and Settings/matsb/Application Data/.emacs

;;; Classic global settingss
(eldoc-mode)
(setf debug-on-error t)
(tool-bar-mode -1)
(setq inhibit-startup-message t)
(column-number-mode 1)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time)
(setq max-lisp-eval-depth 1024)
(setq scroll-margin 2)
(global-font-lock-mode t)
(show-paren-mode 1) ;shows matching parenthesis
(transient-mark-mode 1)
;;find . -type f -name "*.el" -exec sed -i 's/(string-to-lines /(string-lines /g' {} +
(setf x-select-enable-clipboard t)
;; Enable clipboard support using xclip
(when (getenv "DISPLAY")
  (require 'xclip)
  (xclip-mode 1))

(require 'cl-lib)
(defalias 'subseq 'cl-subseq)
(defalias 'first 'cl-first)
(defalias 'second 'cl-second)
(defalias 'third 'cl-third)
(defalias 'fourth 'cl-fourth)
(defalias 'fifth 'cl-fifth)
(defalias 'sixth 'cl-sixth)
(defalias 'seventh 'cl-seventh)
(defalias 'eighth 'cl-eighth)
(defalias 'ninth 'cl-ninth)
(defalias 'rest 'cl-rest)
(defalias 'find-if 'cl-find-if)
(defalias 'getf 'cl-getf)
(defalias 'some 'cl-some)
(defalias 'minusp 'cl-minusp)
(defalias 'plusp 'cl-plusp)

;;; MB setups
(defun emacs-os ()
  (cond ((string-match "linux" (version)) :linux)
	((string-match "cygwin" (version)) :cygwin)
	(t :win32)))
;;(emacs-os)

(defun cygwin-emacs-p ()
  "Returns true if this emacs was invoked from cygwin bash (emacs-w32)"
  (eql (emacs-os) :cygwin))
;;(cygwin-emacs-p)

(defconst +win32-root+
  (cl-ecase (emacs-os)
    (:cygwin "/cygdrive/c")
    (:linux "/")
    (:win32 "C:/")))

;;;; global defaults (can be overrided in .emacs-local)
(defun win32-user-profile ()
  (let ((up (getenv "USERPROFILE")))
    (if up
	(concat "/cygdrive/c" (subseq (substitute ?/ ?\\ up) 2))
      "")))
;;(win32-user-profile)

(defconst +win32-home-dir+
  (if (string= system-name "NPXMBE")
      (format "%s/Documents and Settings/matsb" +win32-root+)
    (win32-user-profile)))

(defconst +home-dir+
  (cl-ecase (emacs-os)
    ((:cygwin :linux) "~")
    (:win32 +win32-home-dir+)))

(cl-defun cygpath (path &optional (type :win32))
  "This should definitetly be moved somewhere else."
  (if (cygwin-emacs-p)
    (let ((ctype (cl-case type
		   (:dos "dos")
		   (:win32 "windows")
		   (:unix "unix")
		   (:mixed "mixed")
		   (t "windows"))))
      (string-trim
       (call-process* "cygpath" "--type" ctype (expand-file-name path))))
    path))

(defconst +os-root+ (expand-file-name "~")
  "Default directory OS root")

(defconst +mb-utils-dir+ (expand-file-name "git/utils" +os-root+)
  "Default directory for utils files.")

(defconst +mb-lisp-dir+ (expand-file-name "elisp" +mb-utils-dir+)
  "Default directory for mb-lisp files.")

(defun mb-emacs-expected-local-filename ()
  (expand-file-name
   (format ".emacs-local-%s-%s"
     system-name
     (substring (symbol-name (emacs-os)) 1))
   +mb-lisp-dir+))
;;(mb-emacs-expected-local-filename)

(defun mb-emacs-local-filename ()
  (let* ((path (expand-file-name
		(format ".emacs-local-%s-%s"
		system-name
			(substring (symbol-name (emacs-os)) 1))
		+mb-lisp-dir+)))
    (if (file-exists-p path)
      path
      (expand-file-name ".emacs-local-default" +mb-lisp-dir+))))
;;(mb-emacs-local-filename)

(defconst +emacs-local+ (mb-emacs-local-filename)
  "Local emacs settings")

(defvar *local-load-paths* nil
  (format "Additional local load paths. Should be defined in\n%s"
	  (mb-emacs-local-filename)))

(defvar *local-requires* nil
  (format "Additional local `require'-ments. Should be defined in\n%s"
	  (mb-emacs-local-filename)))

(defvar *cygwin-root* (when (cygwin-emacs-p) "/")
  "Root directory for cygwin. Typically 'C:/cygwin/' or simply '/'")

(defvar *simple-swaps*
;; TODO: enable :search
  '()
  "A list of explicitly defined swap pairs. Each list element is
on the form

\(FILENAME-SANS-DIRECTORY1 FILENAME-SANS-DIRECTORY2 DIRECTORY\),

for example,

\(list \".emacs\" \".emacs-local-azure-cygwin\" \"~/projects/utils/elisp/\"\).")

(defvar *lilypond-home* nil
  "Default directory for LilyPond hierarchy")

(defvar *my-favorites* ()
  "List of files to be found initially. Each element in list is of
form (FILE ENDP), where FILE is the path of file to be loaded. If ENDP
is not nil, the point is set at end of the corresponding file.")

(defvar *clisp-dir* nil
  "Home path of standard clisp installation for windows.")

(defvar *lynx-prog* "lynx")

(defvar *lynx-dir* nil
  "Home path of standard lynx installation for windows.")

(defvar *lynx-config-dir* "/etc"
  "Home path of lynx configurations files")

(defvar *local-data-dir* (expand-file-name "data" +home-dir+)
  "Directory local data. Used for instance by the lynx proxy
  database and the dictionary database.")

(defvar *local-projects-dir*  (expand-file-name "projects" +home-dir+)
  "Root directory for private projects.
Not in use. Projects should be shared, at least until we are up
and running Git.")

(defvar *mb-hyperspec-root* nil)

(defvar *mb-hyperspec-function* nil
  "Method for browsing the hyperspec.")

(defvar *c++-mb-lib* nil
  "Path to mb-lib")


;;; non standard packages
(require 'package)
(cl-loop for x in '(("melpa" . "http://melpa.org/packages/")
		    ("gnu" . "https://elpa.gnu.org/packages/"))
	 do (unless (member x package-archives)
	      (push x package-archives)))

;; path
(cl-loop for x in (nconc 
		   *local-load-paths*
		   (cl-remove "old" (directory-files +mb-lisp-dir+ t)
			      :key #'file-name-nondirectory
			      :test #'string=)
		   (directory-files
		    ;; The regexp masks the '.' and '..' files
		    (expand-file-name ".emacs.d/elpa" +home-dir+) t ".*[^.]")
		   (list
		    (expand-file-name "games/chess" +mb-lisp-dir+)
		    (expand-file-name "games/cube" +mb-lisp-dir+)
		    (expand-file-name "games/maths" +mb-lisp-dir+)
		    (expand-file-name "games/cram" +mb-lisp-dir+)
	    (expand-file-name "projects/ada" +mb-lisp-dir+)))
	 if (and (file-directory-p x) (not (member x load-path)))
	 collect x into res
	 finally do (setf load-path (append res load-path)))

(length (directory-files (expand-file-name ".emacs.d/elpa" +home-dir+) t))
;; smartparens
(add-to-list 'load-path "~/.emacs.d/smartparens-master")

(let* ((my-mode-alist
       '(("\\.h$\\|\\.cpp$" . c++-mode)	; first overules of original alist
	 ("\\.c$" . c-mode)
	 ("\\.\\(lisp\\|asd\\|sbclrc\\)$" . mb-lisp-mode)
	 ("\\.el$\\|\\.eld$\\|\\.emacs$\\|\\.emacs-local-\\|\\.pwd" .
	  emacs-lisp-mode)
	 ("\\.bash\\(rc\\|_profile\\)\\|\\.sh\\|\\.profile$" . sh-mode)
	 ("\\.pdmkvars$" . makefile-mode)
	 ("\\.pdmkroot$" . makefile-mode)
	 ("Makefile$" . makefile-mode)
	 ("\\.pro$" . makefile-mode)
	 ("\\.\\(avsc\\|json\\)$" . json-mode)
	 ("\\.ejson$" . ejson-mode)
	 ("\\.js$" . js-mode)
	 ("\\.ts$" . js-mode)
	 ("\\.md$" . markdown-mode)
	 ("\\.sql$" . sql-mode)
	 ("\\.bmp$" . hexl-mode)
	 ("\\.mf$" . metafont-mode)
	 ("\\.dvi$" . doc-view-mode)
	 ("\\.gp$" . gnuplot-mode)
	 ("\\.m$\\|\\.octaverc$" . octave-mode)
	 ("\\.py$" . python-mode)
	 ("\\.rb$" . ruby-mode)
	 ("\\.ora$" . text-mode)
	 ("\\.xml$" . xml-mode)
	 ("\\.css$" . css-mode)
	 ("\\.html$" . html-mode)
	 ("\\.js$" . rjsx-mode)
	 ("\\.o$" . hexl-mode)
	 ("\\.exe$" . hexl-mode)
	 ("\\.shp$" . hexl-mode)
	 ("\\.shx$" . hexl-mode)
	 ("\\.dbf$" . hexl-mode)
	 ("\\.mmf$" . hexl-mode)
	 ;; ("\\.\\(inc\\|frm\\|bas\\|cls\\)$" . visual-basic-mode)
	 ("\\.php$" . php-mode)
	 ("\\.xsl$" . sgml-mode)
	 ("\\.cl$" . lisp-mode)
	 ("\\.qz$" . quiz-mode)
	 ("\\.ly$" . LilyPond-mode)
	 ("\\.ily$" . LilyPond-mode)
	 ("\\.exe\\.config$" . sgml-mode)
	 ("\\.bnf$" . backus-naur-mode)
	 ("\\.sc[ie]$" . mbscilab-mode)
	 ("\\.org$" . org-mode)
	 ("\\.el\\.gz$" . emacs-lisp-mode)
	 ("\\.pdf$" . doc-view-mode-maybe)
	 ("\\.texinfo$" . texinfo-mode)
	 ("\\.tex$" . latex-mode)
	 ("\\.yml$" . yaml-mode)

	 ;; safe default must come at the end
	 ("^[^.]*$" . text-mode)))

       (new-alist (cl-delete-if #'(lambda (x)
				 (member x auto-mode-alist))
			     my-mode-alist)))
  (setf auto-mode-alist (append new-alist auto-mode-alist)))
;;(length auto-mode-alist)
;;(copy 'mb-lisp-mode auto-mode-alist :key #'cdr :test #'neql)

;; encoding: the following specifies encoding based on file path
(setq file-coding-system-alist
      (append '(("\\.php\\'" utf-8 . utf-8)
		("\\.js\\'" utf-8 . utf-8)
		("\\arbeidslog\\'" utf-8 . utf-8)
		("\\sota.txt\\'" utf-8 . utf-8))
	      file-coding-system-alist))

;; text-mode
(add-hook 'text-mode-hook #'(lambda () (abbrev-mode 1)))
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'sh-mode-hook 'display-line-numbers-mode)

;;; What's this?! (2015-04-30, but now I have a Clue...)
;; for some reason the following must be done in addition to the above
(defun mb-dabbrev-friend-buffer-function (other-buffer)
  "Returns non-nil if OTHER-BUFFER is to be searched by dabbrev."
  (and (not (member (buffer-name other-buffer)
		    '("cluknomx.txt" "clnoukmx.txt")))
       (dabbrev--same-major-mode-p other-buffer)))
(setq dabbrev-friend-buffer-function #'mb-dabbrev-friend-buffer-function)

(find-file (expand-file-name ".emacs" (file-name-directory +emacs-local+)))
(find-file +emacs-local+)

;; My lisp, finally everything in .emacs should be split into similar
;; files. Also, the files should be byte-compiled too.
;; autoload?
(cl-loop for m in (append '(global-map
			    mb-utils-io
			    mb-evil-map
			    mb-things
			    radio-playlists
			    mb-lisp
			    mb-python
			    mb-octave
			    mb-ruby
			    mb-texinfo
			    list-db
			    mb-indent
			    quiz-park
			    mb-js-mode
			    mb-ert
			    mb-org)
			  *local-requires*)
	 do (message "Loading package %S..." m)
	 do (require m))

;;;; local lisp (overrides defaults)
(load-file +emacs-local+)

(emacs-lisp-mode)

;; autoloads
(autoload 'turn-on-eldoc-mode "eldoc" nil t)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

;; finding favourites
(mapcar
 #'(lambda (x) 
     (save-excursion
       (let ((path (expand-file-name (second x) (first x)))) 	     
	 (find-file path)
	 (awhen (getf (rest (rest x)) :point)
		(goto-char (cl-case it (:end (point-max)) (t it))))
	 (awhen (getf (rest (rest x)) :keyboard)
		(cl-case it 
		  (:no (activate-input-method 'norwegian-keyboard))))
	 (awhen (getf (rest (rest x)) :read-only)
		(toggle-read-only 1))
	 (awhen (getf (rest (rest x)) :read-only :none)
		(when (eql it :none)
		  (auto-fill-mode -1)))
	 (awhen (getf (rest (rest x)) :hook)
		(funcall it))
	 (awhen (getf (rest (rest x)) :time-paragraphs)
		(setf fill-paragraph-function #'fill-time-paragraph)))))
  *my-favorites*)

(custom-set-variables
 '(temp-buffer-resize-mode t)
 '(visible-bell t))
(custom-set-faces)

(setq-default display-fill-column-indicator-column 80)

;; tags revisited
(defun mb-tags-file ()
  (cl-case major-mode
    ((emacs-lisp-mode lisp-interaction-mode) "~/.MBTAGS")
    ((mb-lisp-mode lisp-mode common-lisp-mode) "~/.CLTAGS")
    (mbscilab-mode "~/.SCILABTAGS")
    (octave-mode "~/.OTAGS")
    ((rjsx-mode js-mode) nil)
    (t (error "Couldn't resolve major mode %S for buffer %s"
	      major-mode (buffer-name)))))
;;(mb-tags-file)

(defun mb-visit-tags-table ()
  "Set the current tags file.
Note that this function and its invokations must precede init
loading of files."
  (awhen (mb-tags-file)
    (setq-local tags-file-name it)))

;; set hooks (NB! should these hooks be set here?)
(add-hook 'emacs-lisp-mode-hook 'mb-visit-tags-table)
;; caters for mb-lisp-mode-hook as well:
(add-hook 'lisp-mode-hook 'mb-visit-tags-table)
(add-hook 'mbscilab-mode-hook 'mb-visit-tags-table)
(add-hook 'octave-mode-hook 'mb-visit-tags-table)

;;; Stuff for debugging init phase
(cl-defun disperse-log-messages-in-buffer (&optional (base-message "qwe"))
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-sexp 1)
    (cl-loop for i from 1
	     while (< (point) (point-max))
	     do (progn
		  (move-end-of-line 1)
		  (newline)
		  (insert (format "(message \"%s %d\")" base-message i))
		  (re-search-forward "[^[:space:]]")
		  (forward-sexp 1)))))

(cl-defun remove-log-messages-in-buffer (&optional (base-message "qwe"))
  (interactive)
  (let ((prefix (format "(message \"%s " base-message)))
    (save-excursion
       (goto-char (point-min))
       (while (re-search-forward prefix nil t)
	 (move-beginning-of-line nil)
	 (kill-line 1)))))


;;; ido and flx
(require 'ido)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)


;;; GC optimize
(setq gc-cons-threshold 20000000)

;;; the following should be moved to extension/mb-evil-mode.el
;;; Where to put this?
(cl-defun unix-find (ppath &key type regex name)
  (let ((args nil))
    (when type
      (push-list
       (list "-type" (cl-ecase type (:file "f") (:directory "d"))) args))
    (when name (push-list (list "-name" name) args))
    (when regex (push-list (list "-regex" regex) args))
    (cl-remove ""
      (string-to-lines
       (apply #'call-process* "find" (append (llist ppath) args)))
      :test #'string=)))
;;(last (unix-find "~/tmp" :name "*el"))

(defun compare-mb-libs (path1 path2)
  (let* ((apath1 (file-truename path1))
	 (paths1 (mapcar #'file-truename (unix-find path1 :name "*el")))
	 (diff-buffer (get-buffer-create "*mb-diff*")))
    (with-buffer diff-buffer
      (erase-buffer))
    (cl-loop for p1 in paths1
	     for relative-path = (substring p1 (length apath1))
	     for p2 = (expand-file-name relative-path path2)
	     do (with-buffer diff-buffer
		  (insert
		   (format "Comparing two version of %s:\n" relative-path)))
	     do (call-process "diff" nil diff-buffer t p1 p2))
    (switch-to-buffer diff-buffer)))
;; (compare-mb-libs
;;  "/cygdrive/c/Users/MBe.azure/Google Drive/site-lisp/mb-lisp/"
;;  "/home/MBe/tmp/tmp/package/mb-lisp")

(add-to-list 'Info-default-directory-list
	     (expand-file-name ".emacs.d/info" +home-dir+))
(add-to-list 'Info-additional-directory-list
	     (expand-file-name ".emacs.d/info" +home-dir+))

(defun eval-defun-with-test (orig-fun &rest args)
  (interactive "P")
  (save-excursion
    (apply orig-fun args)
    (end-of-defun 1)
    (eol)
    (eval-last-sexp nil)))
;;(advice-add #'eval-defun :around #'eval-defun-with-test)
;;(advice-remove #'eval-defun #'eval-defun-with-test)

(require 'mb-utils-strings)
(require 'mb-utils-math)
(require 'maths)
(require 'cram)
(require 'mb-gnuplot)

(defun backtrace-goto-error ()
  "Parses error message at current line in buffer *Backtrace*, and goes to indicated point."
  (interactive)
  (let ((regexp "^  eval-buffer(#<buffer  [^ ]* nil \"\\([^\"]*\\)\".*Reading at buffer position \\([0-9]+\\)")
	(curline (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (string-match regexp curline)
    (find-file (match-string 1 curline))
    (goto-char (string-to-number (match-string 2 curline)))))

(setf calendar-week-start-day 1)
;;(require 'pcvs)
(require 'mb-tex)
(require 'mb-octave)
;;(cvs-change-cvsroot ":ext:mbe@192.168.0.17:/ls/silver/repository")

;;; Make this emacs the client server
(require 'server)
(unless (server-running-p) (server-start))

;; ignore some tex output files
(cl-pushnew "\\.\\(dvi\\|aux\\|out\\|bbl\\|blg\\)\\'" ido-ignore-files)
(cl-pushnew "\\.\\([0-9]*gf\\|pk\\)\\'" ido-ignore-files)

(defgroup mb-elisp nil
       "Super group for all mb-elisp customizataions."
       :tag 'mb-elisp
       :group 'emacs)

(require 'json-mode)
(require 'markdown-mode)
