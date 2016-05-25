;; redirected from on of the following
;; c:/Users/Theodor/AppData/Roaming/.emacs
;; c:/Documents and Settings/matsb/Application Data/.emacs

;;; Classic global settingss
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
(transient-mark-mode t)

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

(require 'cl)
(defconst +win32-root+
  (ecase (emacs-os)
    (:cygwin "/cygdrive/c")
    (:linux "/")
    (:win32 "C:/")))

;;;; global defaults (can be overrided in .emacs-local)
(defun win32-user-profile ()
  (concat "/cygdrive/c" (subseq (substitute ?/ ?\\ (getenv "USERPROFILE")) 2)))
;;(win32-user-profile)

(defconst +win32-home-dir+
  (if (string= (getenv "COMPUTERNAME") "NPXMBE")
      (format "%s/Documents and Settings/matsb" +win32-root+)
    (win32-user-profile)))

(defconst +home-dir+
  (ecase (emacs-os)
    ((:cygwin :linux) "~")
    (:win32 +win32-home-dir+)))

(defconst +shared-dir+
  (if (string= (getenv "COMPUTERNAME") "NPXMBE")
      (expand-file-name "/My Documents/Google Drive/" +win32-home-dir+)
    (expand-file-name "Google Drive" +win32-home-dir+)))
;;(getenv "COMPUTERNAME")

(cl-defun cygpath (path &optional (type :win32))
  "This should definitetly be moved somewhere else."
  (let ((ctype (case type
		 (:dos "dos")
		 (:win32 "windows")
		 (:unix "unix")
		 (:mixed "mixed")
		 (t "windows"))))
    (string-trim (call-process* "cygpath" "--type" ctype (expand-file-name path)))))
;;(cygpath "/cygdrive/c/Users/MBe.azure/AppData/Roaming/Scilab/scilab-5.5.1/" :unix)

;;;; global defaults (can be overrided in .emacs-local)
(defvar *shared-data-dir* (expand-file-name "mb-data" +shared-dir+)
  "Directory for shared data, for instance arbeidslog and adresser.txt")

(defvar *shared-projects-dir* (expand-file-name "projects" +shared-dir+)
  "Directory for shared projects. See `+shared-dir+'")

(defvar *site-lisp-dir* (expand-file-name "site-lisp" +shared-dir+)
  "Directory containing extra lisp (eg. mb-lisp).
This is always the parent directory to the .emacs directory.
This directory is shared, see `+shared-dir+'")

(defvar *mb-lisp-dir* (expand-file-name "mb-lisp" *site-lisp-dir*)
  "Default directory for mb-lisp files.")

(defvar *local-load-paths* nil
  "Additional local load paths. Should be defined in .emacs-local-***.")

(defun mb-emacs-local-filename ()
  (let* ((path (expand-file-name (concat ".emacs-local-" (downcase (getenv "COMPUTERNAME"))) *mb-lisp-dir*)))
    (if (file-exists-p path)
	path (expand-file-name ".emacs-local-default" *mb-lisp-dir*))))
;;(mb-emacs-local-filename)

(defconst +emacs-local+ (mb-emacs-local-filename)
  "Local emacs settings")

(defvar *local-requires* nil
  "List of Emacs Lisp modules that are required by local emacs")

(defvar *cygwin-root* (when (cygwin-emacs-p) "/")
  "Root directory for cygwin. Typically 'C:/cygwin/' or simply '/'")

(defvar *lilypond-home* nil
  "Default directory for LilyPond hierarchy")

(defvar *my-favourites* ()
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
Not in use. Projects should be shared, at least until we are up and running Git.")

(defvar *mb-hyperspec-root* nil)

(defvar *mb-hyperspec-function* nil
  "Method for browsing the hyperspec.")

(defvar *c++-mb-lib* nil
  "Path to mb-lib")


;;;; local lisp (overrides defaults)
(load-file +emacs-local+)
(find-file +emacs-local+)
(emacs-lisp-mode)

;;; non standard packages
(require 'package)
(loop for x in '(("marmalade" . "http://marmalade-repo.org/packages/")
		 ("melpa"     . "http://melpa.milkbox.net/packages/"))
      do (unless (member x package-archives)
	   (push x package-archives)))

;; path
(loop for x in (nconc 
		*local-load-paths*
		(directory-files *mb-lisp-dir* t)
		(directory-files (expand-file-name ".emacs.d/elpa" +home-dir+) t)
		(list
		 (expand-file-name "games/maths" *mb-lisp-dir*)
		 (expand-file-name "external/scilab" *mb-lisp-dir*)
		 *site-lisp-dir*))
      if (and (file-directory-p x) (not (member x load-path)))
      collect x into res
      finally do (setf load-path (append res load-path)))

;; smartparens
(add-to-list 'load-path "~/.emacs.d/smartparens-master")

;;gnuplot mode
(require 'gnuplot-mode)

;;paredit
(require 'paredit)
(require 'mb-evil)

;; slime
;;(add-to-list 'load-path (concat *site-lisp-dir* "slime/")) ; your SLIME directory
;;(require 'slime)
;;(slime-setup)

;;(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
;;(autoload 'backus-naur-mode "backus-naur-mode" "Major mode for editing BNF definitions." t)
(autoload 'LilyPond-mode "lilypond-mode" "Major mode for editing BNF definitions." t)
(autoload 'mbscilab-mode "scilab-mode" "Major mode for editing Scilab files." t)

(let* ((my-mode-alist
       '(("\\.h$\\|\\.cpp$" . c++-mode)	; first overules of original alist
	 ("\\.\\(lisp\\|asd\\)$" . mb-lisp-mode)
	 ("\\.el$\\|\\.emacs$\\|\\.emacs-local-" . emacs-lisp-mode)
	 ("\\.bash\\(rc\\|_profile\\)\\|\\.sh$" . sh-mode)
	 ("\\.emacs-local-" . emacs-lisp-mode) ; then safe additions
	 ("\\.pdmkvars$" . makefile-mode)
	 ("\\.pdmkroot$" . makefile-mode)
	 ("\\.bmp$" . hexl-mode)
	 ("\\.gp$" . gnuplot-mode)
	 ("Makefile$" . makefile-mode)
	 ("\\.m$" . octave-mode)
	 ("\\.ora$" . text-mode)
	 ("\\.xml$" . text-mode)
	 ("\\.o$" . hexl-mode)
	 ("\\.exe$" . hexl-mode)
	 ("\\.shp$" . hexl-mode)
	 ("\\.shx$" . hexl-mode)
	 ("\\.dbf$" . hexl-mode)
	 ("\\.mmf$" . hexl-mode)
	 ("\\.\\(frm\\|bas\\|cls\\)$" . visual-basic-mode)
;;	 ("\\.inc$" . fundamental-mode) ; should be vb-mode
	 ("\\.php$" . php-mode)
	 ("\\.xsl$" . sgml-mode)
	 ("\\.cl$" . lisp-mode)
	 ("\\.qz$" . quiz-mode)
	 ("\\.ly$" . LilyPond-mode)
	 ("\\.exe\\.config$" . sgml-mode)
	 ("\\.bnf$" . backus-naur-mode)
	 ("\\.sc[ie]$" . mbscilab-mode)
	 ("\\.org$" . org-mode)
	 ("\\.el\\.gz$" . emacs-lisp-mode)
	 ("\\.pdf$" . doc-view-mode-maybe)
	 ("\\.tex$" . latex-mode)

	 ;; safe default must come at the end
	 ("[^.]*$" . text-mode)))
       (new-alist (delete-if #'(lambda (x) (member x auto-mode-alist)) my-mode-alist)))
  (setf auto-mode-alist (append new-alist auto-mode-alist)))
;;(length auto-mode-alist)

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

;; div modes (see external/mb-div-mode.el)
;; (add-hook 'php-mode-hook 'mb-php-mode-hook-function)
(add-hook 'js-mode-hook 'mb-js-mode-hook-function)
(add-hook 'sh-mode-hook 'linum-mode)

;;; What's this?! (2015-04-30, but now I have a Clue...)
;; for some reason the following must be done in addition to the above
(defun mb-dabbrev-friend-buffer-function (other-buffer)
  "Returns non-nil if OTHER-BUFFER is to be searched by dabbrev."
  (and (not (member (buffer-name other-buffer)
		    '("cluknomx.txt" "clnoukmx.txt")))
       (dabbrev--same-major-mode-p other-buffer)))
(setq dabbrev-friend-buffer-function #'mb-dabbrev-friend-buffer-function)

;; My lisp, finally everything in .emacs should be split into similar
;; files. Also, the files should be byte-compiled too.
;; autoload?

(loop for m in (append '(global-map
;;			 lisp-map
			 elisp-map
			 mb-things
			 radio-playlists
			 mb-lisp
			 list-db
			 mbscilab
			 dic-map
			 ;; maths
			 )
		       *local-requires*)
      do (require m))
;;(require 'radio-playlists)

(custom-set-variables
 '(temp-buffer-resize-mode t)
 '(visible-bell t))
(custom-set-faces)

;; autoloads
(autoload 'turn-on-eldoc-mode "eldoc" nil t)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; tags revisited
(defun mb-tags-file ()
  (case major-mode
    ((emacs-lisp-mode lisp-interaction-mode) "~/.MBTAGS")
    ((mb-lisp-mode lisp-mode common-lisp-mode) "~/.CLTAGS")
    (mbscilab-mode "~/.SCILABTAGS")
    (octave-mode "~/.OTAGS")
    (t (error "Couldn't resolve major mode %S for buffer %s" major-mode (buffer-name)))))

(defun mb-visit-tags-table ()
  "Note that this function and its invokations must precede init loading of files."
  (setq-local tags-file-name (mb-tags-file)))

;; set hooks (NB! should these hooks be set here?)
(add-hook 'emacs-lisp-mode-hook 'mb-visit-tags-table)
(add-hook 'lisp-mode-hook 'mb-visit-tags-table) ;caters for mb-lisp-mode-hook as well
(add-hook 'mbscilab-mode-hook 'mb-visit-tags-table)
(add-hook 'octave-mode-hook 'mb-visit-tags-table)

;; finding favourites
(mapcar #'(lambda (x) 
	    (save-excursion
	      (let ((path (expand-file-name (second x) (first x)))) 	     
		(find-file path)
		(awhen (getf (rest (rest x)) :point)
		  (goto-char (case it (:end (point-max)) (t it))))
		(awhen (getf (rest (rest x)) :keyboard)
		  (case it 
		    (:no (activate-input-method 'norwegian-keyboard))))
		(awhen (getf (rest (rest x)) :read-only)
		  (toggle-read-only 1))
		(awhen (getf (rest (rest x)) :read-only :none)
		  (when (eql it :none)
		    (auto-fill-mode -1)))
		(awhen (getf (rest (rest x)) :hook)
		  (funcall it)))))
  *my-favourites*)

;;(require 'elkem ".elkem")
;;(set-locals-arbeidslog)
(require 'qp)

;;;; Stuff for debugging init phase
(cl-defun disperse-log-messages-in-buffer (&optional (base-message "qwe"))
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-sexp 1)
    (loop for i from 1
	  while (< (point) (point-max))
	  do (progn
	       (move-end-of-line 1)
	       (newline)
	       (insert (format "(message \"%s %d\")" base-message i))
	       (re-search-forward "[^[:space:]]")
	       (forward-sexp 1)))))
					;(switch-to-buffer "*Messages*")

(require 'ido)
(ido-mode t)

;; the following should be moved to extension/mb-evil-mode.el
;;; Where to put this?
(cl-defun unix-find (ppath &key type regex name)
  (let ((args nil))
    (when type (push-list (list "-type" (ecase type (:file "f") (:directory "d"))) args))
    (when name (push-list (list "-name" name) args))
    (when regex (push-list (list "-regex" regex) args))
    (cl-remove "" (string-to-lines (apply #'call-process* "find" (append (llist ppath) args))) :test #'string=)))
;;(last (unix-find "~/tmp" :name "*el"))

(defun compare-mb-libs (path1 path2)
  (let* ((apath1 (file-truename path1))
	 (paths1 (mapcar #'file-truename (unix-find path1 :name "*el")))
	 (diff-buffer (get-buffer-create "*mb-diff*")))
    (with-buffer diff-buffer
      (erase-buffer))
    (loop for p1 in paths1
	  for relative-path = (substring p1 (length apath1))
	  for p2 = (expand-file-name relative-path path2)
	  do (with-buffer diff-buffer
	       (insert (format "Comparing two version of %s:\n" relative-path)))
	  do (call-process "diff" nil diff-buffer t p1 p2))
    (switch-to-buffer diff-buffer)))
;;(compare-mb-libs "/cygdrive/c/Users/MBe.azure/Google Drive/site-lisp/mb-lisp/" "/home/MBe/tmp/tmp/package/mb-lisp")

(defun copy-libs (path1 path2)
  (let* ((apath1 (file-truename path1))
	 (paths1 (mapcar #'file-truename (unix-find path1 :name "*sc[ie]"))))
    (loop for p1 in paths1
	  for relative-path = (substring p1 (length apath1))
	  for p2 = (expand-file-name relative-path path2)
	  do (when (file-newer-than-file-p p1 p2)
	       (copy-file p1 p2)))))
;;(copy-libs "~/tmp/SciLab" "~/sources/SciLab")

(defun rename-files (directory pattern replacement)
  "Renames all files containing PATTERN in DIRECTORY."
  (let* ((paths (file-expand-wildcards  (expand-file-name (format "*%s*" pattern) directory))))
    (loop for path in paths
	  for fn = (file-name-nondirectory path)
	  for new-fn = (replace-regexp-in-string pattern replacement fn)
	  for new-path = (expand-file-name new-fn directory)
	  do (rename-file path new-path))))
;;(rename-files "~/data/FFIAOG/MCMV/M341_Karmoy/MomPks/" "351" "341")

(add-to-list 'Info-default-directory-list (expand-file-name ".emacs.d/info" +home-dir+))
(add-to-list 'Info-additional-directory-list (expand-file-name ".emacs.d/info" +home-dir+))
(require 'clhs)

(defun unit-test-regexp (fn-name)
  (format ";;(.*%s") fn-name)

(defun unit-tests (&optional fn-name)
  "Returns a list of all unit tests for the function with name
  FN-NAME. By default this function is the defun-at-point.

A unit test is a line prefixed by ';;(' and of the form given by
`unit-test-regexp'")

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
(require 'mb-gnuplot)

(defun backward-down-list (&optional arg)
  (interactive "^p")
  (down-list (- (or arg 1))))
(define-key global-map (kbd "C-M-S-d") 'backward-down-list)

(defun backtrace-goto-error ()
  "Parses error message at current line in buffer *Backtrace*, and goes to indicated point."
  (interactive)
  (let ((regexp "^  eval-buffer(#<buffer  [^ ]* nil \"\\([^\"]*\\)\".*Reading at buffer position \\([0-9]+\\)")
	(curline (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (string-match regexp curline)
    (find-file (match-string 1 curline))
    (goto-char (string-to-number (match-string 2 curline)))))

(eldoc-mode)
;;(require 'pcvs)
(require 'mb-tex)
(require 'mb-octave)
;;(cvs-change-cvsroot ":ext:mbe@192.168.0.17:/ls/silver/repository")

;;; Make this emacs the client server
(require 'server)
(unless (server-running-p) (server-start))

(loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
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
      do (evil-set-initial-state mode state))

(with-buffer "arbeidslog"
  (setf fill-paragraph-function #'fill-time-paragraph))

(pushnew "\\.\\(dvi\\|aux\\|out\\|bbl\\|blg\\)\\'" ido-ignore-files)
