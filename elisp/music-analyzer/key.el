(require 'chrome)
(require 'mu-mode "mode.el")

(defstruct (key :named (:conc-name k-))
  (root (make-chrome))
  (mode 'major))

(defun* k-new (&optional (root (make-chrome)) (mode 'major))
  (make-key :root root :mode mode))
;;(k-to-string (k-new))

(defun* k-scale (key &optional (with-alterations t))
  (maptree (bind #'chrome-transpose (k-root key)) (mode-scale (k-mode key) with-alterations)))
;;(maptree #'chrome-to-string (k-scale (k-new (chrome-new 4) 'mixolydian) t))
;;(maptree #'chrome-to-string (k-scale (k-from-string "G minor") t))

(defun k-leading-tone (key)
  "Returns the pitch class of the KEY's leading tone."
  (chrome-transpose (m-leading-tone (k-mode key)) (k-root key)))
;;(chrome-to-string (k-leading-tone (k-new (chrome-new 4) 'aeolian)))

;;; read/write
(defun* k-to-string (k &optional (print-style mu-default-print-style))
  (when k
    (case print-style
      (otherwise (format "%s %S" (chrome-to-string (k-root k) print-style) (k-mode k))))))
;;(k-to-string nil)

(defun* k-from-strings (chrome-string mode-string &optional (print-style mu-default-print-style))
  (make-key :root (chrome-from-string chrome-string print-style)
	    :mode (mode-from-string mode-string print-style)))

(defun* k-from-string (key-string &optional (print-style mu-default-print-style))
  (apply #'k-from-strings (append (split-string key-string) (list print-style))))
;;(k-from-string "ges     minor" 'lilypond)

(provide 'key)
