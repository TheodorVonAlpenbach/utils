(require 'js)
(require 'nodejs-repl)

(setf nodejs-repl-process-name "node -i")

(cl-defun nodejs-repl-buffer-name ()
  "Return the active node REPL buffer name."
  (format "*%s*" nodejs-repl-process-name))
;;(nodejs-repl-buffer-name)

(cl-defun nodejs-repl-buffer ()
  "Return the active node REPL buffer."
  (get-buffer (nodejs-repl-buffer-name)))
;;(nodejs-repl-buffer)

(cl-defun mb-js-mode-hook-function ()
  (electric-pair-local-mode)
  ;;(setq tab-width 4)
  ;; to setup tabs
  (setq js-indent-level 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq js-switch-indent-offset 4)
  (setq-local parens-require-spaces nil))
(add-hook 'js-mode-hook 'mb-js-mode-hook-function)
(add-hook 'rjsx-mode-hook 'mb-js-mode-hook-function)

(cl-defun mb-nodejs-repl-mode-hook-function ()
  (setq-local parens-require-spaces nil))
(add-hook 'nodejs-repl-mode 'mb-nodejs-repl-mode-hook-function)

(cl-defun js-eval-string-raw-1 (string)
  "Helper for `js-eval-string-raw'."
  ;; TODO make sure REPL is already running with (nodejs-repl). Also,
  ;; note that nodejs-repl--send-string is not perfectly implemented.
  (unless (get-buffer "*nodejs*") (nodejs-repl))
  (string-lines (nodejs-repl--send-string (concat string "\n"))))

(cl-defun js-eval-string-raw-old (string)
  "Evaluate string in nodejs REPL."
  ;; TODO make sure REPL is already running with (nodejs-repl). Also,
  ;; note that nodejs-repl--send-string is not perfectly implemented.
  (concat* (cl-loop with re = "\\[[0-9]+m\\([^]*\\)\\[[0-9]+m"
		    for l in (subseq (js-eval-string-raw-1 string) 1 -1)
		    collect (string-trim (replace-regexp-in-string re "\\1" l) ""))
    :in))
;;(js-eval-string "bbRecenter(lrbtBB(0, 10, 1, 2), [0, 0])")
;;(js-eval-string "arrColumn([[1, 217, 3], [1, 218, 3]], 1)")

(cl-defun js-eval-string-raw (string)
  "Helper for `js-eval-string-raw'."
  ;; TODO make sure REPL is already running with (nodejs-repl). Also,
  ;; note that nodejs-repl--send-string is not perfectly implemented.
  (unless (get-buffer "*node -i*")
    (let ((default-directory "~/"))
      (nodejs-repl)))
  (let ((proc (get-process nodejs-repl-process-name)))
    (process-send-string proc (concat string "\n"))
    (while (not (accept-process-output proc 0 10 t)))
    (with-buffer (nodejs-repl-buffer)
      (eob)
      (buffer-substring-no-properties
       (bol)
       (or (re-search-backward "^> " nil t) 1)))))
;;(js-eval-string-raw "1+1")

(cl-defun js-eval-string (string &optional printflag)
  "Evaluate string and show result in minibuffer."
  (let ((res (js-eval-string-raw string)))
    (if printflag (intern "") res)))
;;(js-eval-string "333+333")

(provide 'mb-js-mode)
