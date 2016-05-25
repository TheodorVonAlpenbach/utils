;;; This lib contains functions that handle invoking appropriate buffer

(defun c++-swap-h-cpp-filename (filename)
  "Returns FILENAME with suffix swapped from .h to .cpp (or viceversa)
If suffix is neither .h nor .cpp, function returns FILENAME unchanged."
  (if (string-match "\\.cpp$" filename)
      (concat (file-basename filename) ".h")
    (if (string-match "\\.h$" filename)
	(concat (file-basename filename) ".cpp")
      filename)))

(setq font-lock-maximum-decoration
      '((c-mode . t) (c++-mode . 3) (t . 1)))

(require 'cc-mode)

;; customizing c++ style
(c-add-style ;; since mbe doesn't approve the gnu style
 "mbe"
 '((c-basic-offset . 2)
   (c-comment-only-line-offset . (0 . 0))
   (c-offsets-alist . ((statement-block-intro . +)
		       (substatement-open . 0) ;; gnu -> mbe
		       (label . 0)
		       (statement-case-open . +)
		       (statement-cont . +)
		       (innamespace . 0)
		       (arglist-intro . c-lineup-arglist-intro-after-paren)
		       (arglist-close . c-lineup-arglist)
		       ))
   )
)

(c-set-style "mbe") ;; activate new style

(defvar *c++-std-tags* nil 
  "Sorted list of of std c++ tags where most important tag list comes
last. A prefix argument to `c++-std-ify-region' specifies which level
of std-ification. If P=1 only last tag list is considered, if P=2 only
the two last tag lists an so on. If P=nil then tags are specified with
read-minibuffer.") ;(setq *c++-std-tags* nil)
(unless *c++-std-tags*
  (setq *c++-std-tags* '((numeric_limits)
			 (count for_each transform) 
			 (unary_function binary_function identity not1 ptr_fun back_inserter)
			 (vector list map pair valarray slice)
			 (string)
			 (ostream istream istringstream ostringstream ios)
			 (cout cin cerr endl flush))))

(defun c++-std-ify-region (beg end)
  "Inserts `std::' before each std symbol, except when preceded by
`::', `<' \(templates\). TODO: collect all tags in a list. Takes
prefix argument, see `*c++-std-tags*'."
  (interactive "*r")
  (save-excursion
    (when (< end beg) (rotatef beg end))
    (goto-char beg)
    (let* ((gen-prefix-arg (or current-prefix-arg (length *c++-std-tags*)))
	   (tags (if (= gen-prefix-arg 0)
		   (let ((arg (read-minibuffer "tags: ")))
		     (if (atom arg) (list arg) arg))
		   (apply #'append (last *c++-std-tags* gen-prefix-arg))))
	   (regexp (concat* tags 
			    :pre "\\([^:_a-zA-Z0-9]\\)\\(" 
			    :in "\\|" 
			    :suf "\\)\\([^_a-zA-Z0-9]\\)" 
			    :key #'symbol-name))
	   (namespace-tag "std::")
	   (case-fold-search nil))
      (while (re-search-forward regexp end t)
	(replace-match (concat "\\1" namespace-tag "\\2" "\\3") nil nil)
	(c-indent-command)
	(setf end (max end (point)))))))

;;(add-hook 'c++-mode-hook 'auto-fill-mode)
;;(remove-hook 'c++-mode-hook 'auto-fill-mode)

(provide 'c++-buffer)
