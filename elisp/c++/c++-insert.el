(defun indentation-column ()
  "Returns point "
  (save-excursion
    (back-to-indentation)
    (current-column)))
;;(definteractive indentation-column)
;;(indentation-column)

(require 'mb-insert)
;(definteractive word-at-point)
;(definteractive symbol-at-point)
(defun symbol-at-point* (&optional point)
  (save-excursion
    (goto-char (or point (point)))
    (symbol-at-point)))

(defun c++-insert-parentheses (arg)
  (interactive "*P")
  (let* ((space-symbols '(if for while))
	 (space (if (find (symbol-at-point* (1- (point))) space-symbols)
		  :before :none)))
    (insert-parentheses* arg :ensure-space space)))
;(insert-parentheses* 0 :ensure-space :before)

(defun c-electric-brace (arg)
  "Overrides standard c-electric-brace, see
c:/unix/emacs-20.7/lisp/progmodes/cc-cmds.el."
  (interactive "*P")
  (print arg)
  (if (eql arg '-)
    (insert "{}")
    (c-indent-command)
    (let ((begp (= (current-column) (indentation-column))))
      (insert-curl-brackets arg)
      (when (not begp)
	(insert "\n\n")
	(c-indent-command)
	(previous-line 1)
	(c-indent-command)))))

(provide 'c++-insert)

