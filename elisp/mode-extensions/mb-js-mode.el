;;; mb-js-mode.el --- Personal JavaScript mode extensions -*- lexical-binding: t; -*-

;;; Code:

(require 'js)
(require 'subr-x)

(declare-function nodejs-repl "nodejs-repl" ())
(declare-function nodejs-repl-execute "nodejs-repl" (command &optional buffer))

(defvar nodejs-repl-process-name)
(defvar nodejs-repl-temp-buffer-name)
(defvar parens-require-spaces)

(defcustom mb-js-nodejs-repl-process-name "node -i"
  "Process name to use for the optional Node.js REPL."
  :type 'string
  :group 'js)

(defun nodejs-repl-buffer-name ()
  "Return the configured Node.js REPL buffer name."
  (format "*%s*" mb-js-nodejs-repl-process-name))

(defun nodejs-repl-buffer ()
  "Return the configured Node.js REPL buffer, or nil if absent."
  (get-buffer (nodejs-repl-buffer-name)))

(defun mb-js-mode-hook-function ()
  "Apply personal buffer-local settings for JavaScript modes."
  (electric-pair-local-mode 1)
  (setq-local js-indent-level 4)
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)
  (setq-local js-switch-indent-offset 4)
  (setq-local parens-require-spaces nil))

(add-hook 'js-mode-hook #'mb-js-mode-hook-function)
(add-hook 'rjsx-mode-hook #'mb-js-mode-hook-function)

(defun mb-nodejs-repl-mode-hook-function ()
  "Apply personal buffer-local settings for `nodejs-repl-mode'."
  (setq-local parens-require-spaces nil))

(add-hook 'nodejs-repl-mode-hook #'mb-nodejs-repl-mode-hook-function)

(defun mb-js--nodejs-execute (string)
  "Evaluate STRING through the optional nodejs-repl package."
  (unless (require 'nodejs-repl nil t)
    (user-error
     "JavaScript evaluation requires the optional nodejs-repl package"))
  (setq nodejs-repl-process-name mb-js-nodejs-repl-process-name)
  (let ((process (get-process nodejs-repl-process-name)))
    (unless (and process (process-live-p process))
      (nodejs-repl)))
  (nodejs-repl-execute string)
  (let ((buffer (get-buffer nodejs-repl-temp-buffer-name)))
    (unless buffer
      (error "nodejs-repl produced no output buffer"))
    (with-current-buffer buffer
      (string-trim (buffer-substring-no-properties
                    (point-min) (point-max))))))

(defun js-eval-string-raw (string)
  "Evaluate JavaScript STRING and return the raw result text."
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (mb-js--nodejs-execute string))

(defun js-eval-string (string &optional printflag)
  "Evaluate JavaScript STRING and return its result.
When PRINTFLAG is non-nil, also display the result in the echo
area.  The optional nodejs-repl package and Node.js executable are
required only when this function is called."
  (let ((result (js-eval-string-raw string)))
    (when printflag
      (message "%s" result))
    result))

(provide 'mb-js-mode)

;;; mb-js-mode.el ends here
