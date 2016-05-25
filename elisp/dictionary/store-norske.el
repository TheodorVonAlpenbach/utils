(defconst sn-links-dir "c:/projects/vb/VBGet2/data/")
(defconst sn-filename (concat *mb-lisp-dir* "dictionary/sn-links.el"))
(defconst sn-links ())
;;(print* sn-links sn-filename)

(defun sn-filename (initial)
  (format "%slinks%s.txt" sn-links-dir (upcase initial)))
;;(sn-filename "AA")

(defun sn-read-links (filename)
  (parse-csv-file filename "\t"))
;;(setq links-b (sn-read-links "c:/projects/vb/VBGet2/data/testB.txt"))
;;(assoc (completing-read "" links-b) links-b)
;;(print* links-b "qwe")
;;(setq links-b (read* "qwe"))

(defun* sn-collect-unprocessed-links (&optional (filename sn-filename))
  (let ((msg "Reading unprocessed SN links ")
	(chars (append (split-string "abcdefghijklmnopqrstuvwxyz" "")
		       '("ae" "oe" "aa"))))
    (message msg)
    (loop for c in chars
	  do (setq msg (concat msg "."))
	  do (message msg)
	  append (sn-read-links (sn-filename c)))))
;;(sn-collect-unprocessed-links)

;;;; init links
(defun sn-init ()
  (when (null sn-links) 
    (unless (file-exists-p sn-filename)
      (let ((coding-system-for-read 'iso-latin-1)) 
	(print* (sn-collect-unprocessed-links) sn-filename)))
    (setq sn-links (read* sn-filename))))
;;(sn-init)

;;(assoc (completing-read "" sn-links) sn-links)
;;(nilf sn-links)
;;(delete-file sn-filename)
(defun sn-lookup-word-interactive ()
  `(,(let ((completion-ignore-case t)
	   (w (or (word-at-point) "")))
       (completing-read "Look up in Store Norske: " 
			sn-links nil nil (cons w (length w))))))
;;(sn-lookup-word-interactive)

(defun sn-lookup-word (word)
  (interactive (sn-lookup-word-interactive))
  (sn-goto-article word))

(defun url-sn-p (url)
  (string-match "www\\.storenorskeleksikon\\.no" url))

(defun sn-goto-article (word)
  (aif (assoc word sn-links)
    (lynx-browse (format "http://www.storenorskeleksikon.no/sa.aspx?artid=%s" (second it)))
    (message "%s is not a lookup entry in Store Norske!" word)))

(defun lynx-sn-goto-article-begin ()
  (goto-char (point-min))
  (re-search-forward "^[^[:alnum:]]*Vis[^[:alnum:]]*" nil t)
  (beginning-of-line)
  (recenter 1))

(defun qwe ()
  (with-file "qwe"
    (loop for x in sn-links
	  do (insert (format "%S\n" x)))))
;;(qwe)

(provide 'store-norske)
