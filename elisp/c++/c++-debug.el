(provide 'c++-debug)

;; My redefinition of fun in gud.el
(defun gud-find-file (file) "Redefined by mbe"
  ;; mb-hack: remove double directory references, eg. f://f/projects/...
  (if (string-match "//f/" file)
    (setq file (replace-match "/" t t file)))
  ;; Don't get confused by double slashes in the name that comes from GDB.
  (while (string-match "//+" file)
    (setq file (replace-match "/" t t file)))
  (funcall gud-find-file file))

(defun c++-insert-cout-expression (arg expr)
  "Inserts line that flushes expression 'm: $m' to cout. If prefix
argument is given, endl is flushed instead of space."
  (interactive "*P\nsexpression: ")
  (let ((text-expr (if (stringp expr)
		     (replace-regexp expr "\"" "\\\"" nil t)
		     expr)))
    (if arg
      (smart-insert "std::cout << \"" text-expr ":\\n\" << " expr " << std::endl;")
      (smart-insert "std::cout << \"" text-expr ": \" << " expr " << std::endl;"))))

