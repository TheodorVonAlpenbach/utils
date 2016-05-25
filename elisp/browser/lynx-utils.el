(require 'mb-utils-strings)
(require 'mb-utils-regexp)
(require 'mb-locale)
(require 'mb-things)

(defconst *lynx-ref-regexp* (concat "\\[" *natural-number-regexp* "\\]"))
(defthing 'lynx-reference *lynx-ref-regexp*)

(defvar lynx-tree-sites nil
  "A list of elements on the form (SITE-P TREE-BROWSE-PARENT
  TREE-FORWARD-CHILD), where SITE-P is a boolean method on
  form (lambda (URL)). If it returns non nil for the current lynx
  URL, the two following list items will be used as functions for
  LYNX-TREE-BROWSE-PARENT and LYNX-TREE-FORWARD-CHILD
  respectively. TREE-BROWSE-UP is a method that browses the
  parent URL of the current URL, TREE-FORWARD-CHILD is a method
  that moves point to the reference that corresponds to the next
  child in the lynx tree. The latter two methods are on the
  form (lambda (N)), where N is the standard number prefix
  argument.")

;; Navigation
(cl-defun lynx-find-site (&optional (url *lynx-current-url*))
  (find-if (lambda (site)
	     (funcall (first site) url))
	   lynx-tree-sites))

(cl-defun lynx-tree-goto-parent (&optional (n 1))
  "Uncommented"
  (interactive "p")
  (let ((site (lynx-find-site)))
    (if site
      (funcall (second site) n)
      ;else default
      (lynx-goto-last n))))

(cl-defun lynx-tree-forward-child (&optional (n 1))
  "Uncommented"
  (interactive "p")
  (let ((site (lynx-find-site)))
    (if site
      (funcall (third site) n)
      ;else default
      (forward-lynx-reference n))))

(cl-defun lynx-tree-goto-next-sibling (&optional (n 1))
  "Uncommented"
  (interactive "p")
  (lynx-tree-goto-parent 1)
  (lynx-tree-forward-child n)
  (lynx-browse-url-at-point))

(cl-defun lynx-tree-goto-previous-sibling (&optional (n 1))
  "Uncommented"
  (interactive "p")
  (lynx-tree-goto-next-sibling (- n)))

(cl-defun lynx-go-last-and-forward-old (&optional (n 1))
  (interactive "p")
  (lynx-goto-last 1)
  (backward-char 1)
  (forward-lynx-reference n)
  (lynx-browse-url-at-point))

(cl-defun lynx-go-last-and-backward-old (&optional (n 1))
  (interactive "p")
  (lynx-goto-last 1)
  (backward-lynx-reference n)
  (lynx-browse-url-at-point ))

(defun lynx-move-to-last-article ()
  (interactive)
  (goto-char (second (lynx-aftenposten-siste-100-region))))


(cl-defun string-encode (string &optional (encoding *iso-latin1-encoding*))
  (string-replace-map string (assoc-project encoding 0 2)))
;;(url-encode "2-08 Bizet, Georges (1838-1875) - Les Pecheurs De Perles_ A cette voix...Je crois entendre encore (Nadir).ogg")
;;(string-encode "/")
;;(string-encode "øre")

(cl-defun url-file-encode (string &optional (encoding *win32-file-encoding*))
  (string-encode string encoding))
;;(url-file-encode "øre")

(defun lynx-reference-active-at-point ()
  "Returns integer of current lynx reference."
  (interactive)
  (save-excursion
    (backward-lynx-reference 1)
    (re-search-forward *natural-number-regexp*)
    (princ (string-to-int (match-string 0)))))
(definteractive lynx-reference-active-at-point)

(defun lynx-references-in-region (beg end)
  "Returns integer of current lynx reference."
  (interactive "r")
  (setq end (max beg end))
  (save-excursion
    (goto-char beg)
    (loop for pos = (re-search-forward "\\[\\([0-9]+\\)\\]" end t)
	  while pos
	  collect (string-to-int (match-string 1)))))

(defun lynx-references-section ()
  "Returns substring in lynx buffer covering the references section
\(at the end\)."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (buffer-substring (re-search-backward "^References")
		      (point-max))))

; (defun qwe () (interactive) (re-search-backward "[/?]\\([^/?\n]+\\)/?$" nil t))
; (defun qwe () (interactive) (re-search-backward "/[^/\n]+[/]?$" nil t))
; (defun qwe () (interactive) (re-search-backward "/\\([^/\n]+[/]?\\)$\\|\?\\(.*\\)$" nil t))
; (defun qwe () (interactive) (re-search-backward "\?\\(.*\\)$" nil t))
; (defun qwe () (interactive) (re-search-backward "^\\s-*\\([0-9]+\\)\\. \\(.*\\)$" nil t))
; (defun qwe () (interactive) (re-search-backward "[/?]\\([^/?\n]+/?$\\)" nil t))
; (defun qwe () (interactive) (re-search-backward "^ *\\([0-9]+\\)\\. \\(.*\\)$" nil t))
(defvar *lynx-parse-references-regexp* "^ *\\([0-9]+\\)\\. \\(.*\\)$")

(defun ewq () 
  (interactive)
  (re-search-backward *lynx-parse-references-regexp* nil t)
;  (message "%s" (list (match-string 1) (match-string 2)))
  (pr (gethash (match-string 2) (lpdb-entries *lynx-proxy-db*))))

(cl-defun lynx-references-in-proxy (&optional (db *lynx-proxy-db*))
  "Returns a list of strings representing Lynx references to entries
in proxy DB."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (let ((res ()))
      (while (re-search-backward *lynx-parse-references-regexp* nil t)
	(when (gethash (match-string 2) (lpdb-entries db))
	  (push (match-string 1) res)))
      res)))
(definteractive lynx-references-in-proxy)

(defun lynx-url-from-reference (ref)
  "Returns url corresponding to REF."
  (interactive "P")
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward (format "^ *%d\\. *\\(.*\\)$" ref) nil t)
      (string-remove-props (match-string 1))
      (progn
	(message "Couldn't find url!")
	*lynx-current-url*))))

(defun url= (url1 url2)
  "Checks if URL1 and URL2 are equivalent. TODO: use some trim
function."
  (let ((lasti-1 (1- (length url1)))
	(lasti-2 (1- (length url2))))
    (string-equal* 
     url1 url2
     :end1 (if (= (char url1 lasti-1) ?/) lasti-1 nil)
     :end2 (if (= (char url2 lasti-2) ?/) lasti-2 nil))))
;;(url= "aaasdf/" "aaasdf/")

;;(expand-file-name "asdf/" "c:/foo/bar/quux")

(defun html-subst (url)
  "In current buffer subistutes all urls relative to URL with absolute
urls. Eg. /foo -> http://www.bar.com/foo."
  (goto-char (point-min))
  (re-search-forward "\\<body")
  (while (re-search-forward
	  "\<a href\\s-*=\\s-*\"?\\(\\([^h \t\"]\\|h\\([^t]\\|t\\([^t]\\|t[^p]\\)\\)\\)[^ \t\n\"]*\\)"
	  nil t)
    (replace-match (expand-file-name* (match-string-no-properties 1) url) nil nil nil 1)))
;;(cancel-debug-on-entry 'html-subst)
;;(html-subst "http://www.foo.no/")
;;<a href = http <a href =http <a href ="http <a href=./oslo/
;;(match-data)
;;(match-string-no-properties 1)
;;(expand-file-name "../oslo" "/http://www.foo.no/a/b")
;(replace-match (substring (expand-file-name (string-left-trim ?/ (match-string 1)) "/http://www.foo.no/") 1) nil nil nil 1)

(defun expand-file-name* (file directory)
  (with-constant-match-data
      (let (pos
	    (string (if (nor (= (last-elt directory) (first-elt file)))
		      (concat directory "/" file)
		      (concat directory file))))
	(string-match ":/+" string)
	(setq pos (match-end 0))
	(while (string-match "/[^/]+/\\.\\./\\|/\\./\\|//" string pos)
	  (setq string (replace-match "/" nil nil string)))
	string)))
;;(expand-file-name* "foo" "http:/bar")
;;(match-data)

;;(completing-read "Which dictionary? " '(("aa" 1) ("ab" 2)))
;;(all-completions "ab" '(("ab") ("ac")))
;;(try-completion "ab" '(("a") ("ac")))

(require 'mb-mail)
(defun lynx-mail-region (begin end refresh)
  "Insert region as the text in a new message."
  (interactive "r\nP")
  (if refresh (mb-refresh-addresses))
  (let* ((text (buffer-substring begin end))
	(url *lynx-current-url*)
	(url-formatted (format (if (eq message-send-and-exit-method #'message-send-and-exit-hydro) "(%s)" "<%s>") url)))
    (gnus-msg-mail (mb-query-address) "")
    (goto-char (point-max))
    (insert text)
    (insert (format (if (= (preceding-char) ?\n)
		      "   %s\n"
		      "\n   %s\n")
		    url-formatted))
    (message-goto-subject)))
;;(lynx-mail-region)

(provide 'lynx-utils)
