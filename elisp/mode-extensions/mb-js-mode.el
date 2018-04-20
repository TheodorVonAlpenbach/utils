(require 'js)
(require 'nodejs-repl)

(defun mb-js-mode-hook-function ()
  (linum-mode)
  ;;(setq tab-width 4)
  ;; to setup tabs
  (setq js-indent-level 4)
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (setq-local parens-require-spaces nil))
(add-hook 'js-mode-hook 'mb-js-mode-hook-function)

(defun mb-nodejs-repl-mode-hook-function ()
  (setq-local parens-require-spaces nil))
(add-hook 'nodejs-repl-mode 'mb-nodejs-repl-mode-hook-function)

(defun js-eval-string-raw-1 (string)
  "Helper for `js-eval-string-raw'."
  ;; TODO make sure REPL is already running with (nodejs-repl). Also,
  ;; note that nodejs-repl--send-string is not perfectly implemented.
  (unless (get-buffer "*nodejs*") (nodejs-repl))
  (string-lines (nodejs-repl--send-string (concat string "\n"))))
;;(second (js-eval-string-raw-1 "arrColumn([[1, 217, 3], [1, 218, 3]], 1)"))

(defun js-eval-string-raw (string)
  "Evaluate string in nodejs REPL."
  ;; TODO make sure REPL is already running with (nodejs-repl). Also,
  ;; note that nodejs-repl--send-string is not perfectly implemented.
  (concat* (loop with re = "\\[[0-9]+m\\([^]*\\)\\[[0-9]+m"
		 for l in (subseq (js-eval-string-raw-1 string) 1 -1)
		 collect (string-trim (replace-regexp-in-string re "\\1" l) ""))
    :in))
;;(js-eval-string "bbRecenter(lrbtBB(0, 10, 1, 2), [0, 0])")
;;(js-eval-string "arrColumn([[1, 217, 3], [1, 218, 3]], 1)")

(defun js-eval-string (string &optional printflag)
  "Evaluate string and show result in minibuffer."
  (let ((res (js-eval-string-raw string)))
    (if printflag (intern "") res)))
;;(js-eval-string "arrColumn([[1, 217, 3], [1, 218, 3]], 1)")
;;(js-eval-string "'333+333'")

(provide 'mb-js-mode)
