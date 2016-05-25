;;; This is a tiny modification of clhs.el, see this file for copyrights and other information.
(eval-when-compile (require 'cl)) ; push
(require 'browse-url)
(require 'thingatpt)
(require 'url)

(defcustom common-lisp-hyperspec-root
  "http://www.lispworks.com/documentation/HyperSpec/"
  "*The root of the Common Lisp HyperSpec URL.
If you copy the HyperSpec to your local system, set this variable to
something like \"file:/usr/local/doc/HyperSpec/\"."
  :group 'lisp
  :type 'string)
;;(setf common-lisp-hyperspec-root "file:~/doc/HyperSpec")

(defvar clhs-history nil
  "History of symbols looked up in the Common Lisp HyperSpec so far.")

(defvar clhs-symbols nil)

(defun hs-local-root (root)
  "Returns the OS path of ROOT or nil if the path cannot be resolved.
If ROOT is on form \"file:/somepath\", then \"/somepath\" is returned.
Else it returns ROOT as is if it is recognized by then current OS as a valid path.
Otherwise nil is returned"
  (if (string-match* "^file:" root)
    (substring root 5)
    (when (file-exists-p root)
      root)))
;;(hs-local-root common-lisp-hyperspec-root)

(defun mb-clhs-table-buffer (&optional root)
  (unless root (setq root common-lisp-hyperspec-root))
  ;; change 1
  (aif (hs-local-root root)
    ;; IT is bound to an existing path in OS
    (with-current-buffer (get-buffer-create " *clhs-tmp-buf*")
      (insert-file-contents-literally
       (let* ((d (concat it "/Data/"))
	      (f (concat d "Map_Sym.txt")))
           (if (file-exists-p f) f
             (setq f (concat d "Symbol-Table.text"))
             (if (file-exists-p f) f
		 (error "no symbol table at %s" root))))
       nil nil nil t)
      (goto-char 0)
      (current-buffer))
    ;; Else try IT as an URL
    (let* ((d (concat root "/Data/"))
           (f (concat d "Map_Sym.txt")))
      (set-buffer (url-retrieve-synchronously f))
      (goto-char 0)
      (unless (looking-at "^HTTP/.*200 *OK$")
        (kill-buffer (current-buffer))
        (setq f (concat d "Symbol-Table.text"))
        (set-buffer (url-retrieve-synchronously f))
        (goto-char 0)
        (unless (looking-at "^HTTP/.*200 *OK$")
          (kill-buffer (current-buffer))
          (error "no symbol table at %s" root)))
      ;; skip to the first symbol
      (search-forward "\n\n")
      (current-buffer))))
;;(mb-clhs-table-buffer)

(defun clhs-read-symbols ()
  "read `clhs-symbols' from the current position in the current buffer"
  (while (not (eobp))
    (puthash (buffer-substring-no-properties ; symbol
              (line-beginning-position) (line-end-position))
             (progn (forward-line 1) ; file name
                    (buffer-substring-no-properties ; strip "../"
                     (+ 3 (line-beginning-position)) (line-end-position)))
             clhs-symbols)
    (forward-line 1)))

(defun mb-clhs-symbols ()
  "Get `clhs-symbols' from `common-lisp-hyperspec-root'."
  (if (and clhs-symbols (not (= 0 (hash-table-count clhs-symbols))))
      clhs-symbols
    (with-current-buffer (mb-clhs-table-buffer)
      (unless clhs-symbols
        (setq clhs-symbols (make-hash-table :test 'equal :size 1031)))
      (clhs-read-symbols)
      (kill-buffer (current-buffer))
      clhs-symbols)))

(defun hash-table-complete (string table how)
  "This makes it possible to use hash-tables with `completing-read'.
Actually, `completing-read' in Emacs 22 accepts hash-tables natively."
  (let ((res nil) (st (upcase string)) (len (length string)))
    (maphash (lambda (key val)
               (when (and (<= len (length key))
                          (string= st (substring key 0 len)))
                 (push key res)))
             table)
    (if how
        res                       ; `all-completions'
        (if (cdr res)
            (try-completion st (mapcar #'list res))
            (if (string= st (car res))
                t
                (car res))))))

;;;###autoload
(defun mb-common-lisp-hyperspec (symbol-name)
  "Browse the Common Lisp HyperSpec documentation for SYMBOL-NAME.
Finds the HyperSpec at `common-lisp-hyperspec-root'."
  (interactive (list (let ((sym (thing-at-point 'symbol))
                           (completion-ignore-case t))
                       (completing-read
                        "Look-up symbol in the Common Lisp HyperSpec: "
                        #'hash-table-complete (mb-clhs-symbols)
                        t sym 'clhs-history))))
  (let ((subpath (gethash (upcase symbol-name) (mb-clhs-symbols))))
    (browse-url (aif (hs-local-root common-lisp-hyperspec-root)
		  (cygpath (expand-file-name subpath it) :mixed)
		  (concat common-lisp-hyperspec-root subpath)))))

(provide 'mb-clhs)
