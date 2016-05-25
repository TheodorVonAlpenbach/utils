(defconst lynx-aftenposten-date "[0-3]?[0-9]\\.[0-1][0-9]" "Regexp that matches a date string in lynx-aftenposten")
(defconst lynx-aftenposten-time *iso-time* "Regexp that matches a time string in lynx-aftenposten")

(defconst lynx-aftenposten-skip-articles-list '("golf" "bildeserie" "oslopuls" "n24" "spill" "bolig" "popuphilsen" "player" "video")
  "Liste av ord som omdannes til regexp i
`lynx-aftenposten-skip-articles'. Andre filterkandidater er:
oslopuls")

(defun remove-aftenposten-site ()
  (setq lynx-tree-sites
	(remove-if #'(lambda (site)
		     (equal #'lynx-aftenposten-p (first site)))
		 lynx-tree-sites)))
;;(remove-aftenposten-site)

(cl-defun lynx-tree-goto-parent-aftenposten (&optional (n 1))
  "Boring, just the default"
  (lynx-goto-last n))

(cl-defun lynx-tree-forward-child-aftenposten (&optional (n 1))
  "Uncommented"
;;  (loop with step = (* 2 (signum n)) ;why did I multiply with 2?!
  (cl-loop with step = (signum n)
	   for i below (abs n)
	   do (forward-lynx-reference step)
	   do (while (not-empty (string-match* 
				 (lynx-aftenposten-skip-articles)
				 (lynx-url-at-point)))
		(forward-lynx-reference step))))

(cl-defun lynx-aftenposten-skip-articles (&optional (additional-regexp nil))
  "Genererer regexp som URI-filter. Resurser med URI som passer med
denne konstanten vil bli ignorert av funksjonen
`lynx-go-last-and-forward-base'. Se ogsÅÂ
`lynx-aftenposten-skip-articles-list'."
  (concat* (if additional-regexp 
	       (cons additional-regexp lynx-aftenposten-skip-articles-list)
	       lynx-aftenposten-skip-articles-list)  
	     :in "\\|" 
	     :key #'(lambda (x) (format "\\(%s\\)" x))))
;;(lynx-aftenposten-skip-articles "qwe")

(cl-defun lynx-tree-forward-child-old (&optional (n 1))
  "Uncommented"
  (interactive "p")
  (lynx-goto-last 1)
  (when (> n 0) (backward-char 1))
  (forward-lynx-reference n)
  (while (not-empty (string-match* skip (lynx-url-at-point)))
    (forward-lynx-reference (signum n))))

(defun lynx-aftenposten-p (url)
  (regexp-equal ".*\\.\\(aftenposten\\|osloby\\)\\.no.*" url))
;;(lynx-aftenposten-p "http://fotball.aftenposten.no/landslaget/article148659.ece")
;;(lynx-aftenposten-p "http://www.osloby.no/landslaget/article148659.ece")

(defun lynx-aftenposten-article-p (url)
  (regexp-equal ".*www\\.aftenposten\\.no.*articleID=.*" url))
;;(lynx-aftenposten-article-p "http://www.aftenposten.no/nyheter/siste100/")
;;(if (lynx-aftenposten-article-p "http://www.aftenposten.no/nyheter/siste100/") 40 80)

(defun lynx-aftenposten-format-paragraphs ()
  "Assume point is at beginning of article."
  (let ((article-beginning (point)))
    (lynx-aftenposten-goto-article-end)
    (loop for p = (progn (backward-paragraph 1) (point))
	  while (> p article-beginning)
	  do (fill-paragraph nil))
    (recenter 1)))

(defun lynx-aftenposten-goto-article-begin ()
  "Recenters buffer window according to content. Typically, it skips
header links and commercial links at top of page and goes directly to
the start of article, article listing or whatever part of page that is
considered to be most interesting."
  (goto-char (point-min))
  (cond
   ((and (re-search-forward "^Mest delt$" nil t)
	 (re-search-forward "^[-[:alnum:]]" nil t))
    (beginning-of-line))
   
   ((or (re-search-forward "^[[:space:]]+Siste nytt: " nil t)
	(re-search-forward "Fotball (_) Aftenposten.no" nil t))
    (next-line 4))

   ((re-search-forward "^Min leseliste" nil t)
    (re-search-forward "^[[:word:]-]" nil t)
    (bol))


   ((re-search-forward "(Submit) SÅ¯k" nil t)
    (next-line 6))   

   ((or (re-search-forward (format "Tips oss") nil t)
	(re-search-forward "^RSS Aftenposten RSS" nil t))
    (forward-line 2) (beginning-of-line))

   ((and (re-search-forward "^Oslopuls - guide for Oslo" nil t)
	 (re-search-forward "^\\sw" nil t))
    (forward-paragraph 3))

   ((re-search-forward "^[-[:alnum:]]" nil t)
    (beginning-of-line)))

  (recenter 1)
  (point))
;(lynx-aftenposten-goto-article-begin)

(defun lynx-aftenposten-goto-article-end ()
  (when (or (re-search-forward "^Kommentarer$" nil t)
	    (re-search-forward "^Siste fra seksjon$" nil t)
	    (re-search-forward "^Relaterte bilder" nil t)
	    (re-search-forward "^PÅÂ forsiden akkurat nÅÂ" nil t)
	    (re-search-forward "^Si din mening" nil t)
	    ;; (re-search-forward "^Velg reisemÅÂl" nil t)
	    (re-search-forward "^[[:space:]]+Siste nytt" nil t)))
    (backward-paragraph 2)
    (point))

(add-hook 'lynx-show-html-hook #'lynx-aftenposten-goto-article-begin)

(defun lynx-proxy-clear-aftenposten (n)
  "If prefix is given, the whole aftenposten is cleared."
  (interactive "P")
  (lynx-proxy-clear-db
   :regexp "www\\.aftenposten\\.no/.*/$"
   :time (now :hour -12))
  (lynx-proxy-clear-db
   :regexp "www\\.aftenposten\\.no/nyheter/.*"
   :time (now :day -1))
  (when n
      (lynx-proxy-clear-db
   :regexp "www\\.aftenposten\\.no"
   :time (now :day -2)))
  (lynx-update-proxy-refs-regexp)
  (font-lock-fontify-block))
;;(lpe-abs-filename [cl-struct-lynx-proxy-entry "http://www.aftenposten.no/nyheter/iriks/oslo/" 2 3221])
;;(string-match "www\\.aftenposten\\.no/.*/$" "http://www.aftenposten.no/kul_und/")
;;(string-match (format "^   %s kl.%s" *aftenposten-date* *iso-time*) "   30.10 kl.14:58: [135]HÅ¯yesterett opphevet drapsdom (Innenriks)")

(cl-defun lynx-aftenposten-siste-100-region ()
  "Returns region in Lynx buffer containing all the article links in
'http://www.aftenposten.no/nyheter/siste100/'"
  (let ((begin 0)
	(end 0)
	(regexp-end "^PÅÂ forsiden akkurat nÅÂ\\|Paa forsiden akkurat naa"))
    (save-excursion
      (list (lynx-aftenposten-goto-article-begin)
	    (lynx-aftenposten-goto-article-end)))))

(cl-defun lynx-aftenposten-siste-100-region-old (&optional (regexp-beg "siste 100 sport") (regexp-end "^\\["))
  "Returns region in Lynx buffer containing all the article links in
'http://www.aftenposten.no/nyheter/siste100/'"
  (let ((begin 0)
	(end 0))
    (save-excursion
      (goto-char (point-min))
      (setq begin (re-search-forward regexp-beg nil t))
      (setq end (re-search-forward regexp-end nil t))
      (list begin end))))

;;; Add aftenposten site
(push-unique 
 (list #'lynx-aftenposten-p #'lynx-tree-goto-parent-aftenposten #'lynx-tree-forward-child-aftenposten)
 lynx-tree-sites
 #'equal)

(provide 'lynx-aftenposten)
