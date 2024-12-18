(require 'mb-utils-io)

;;;; Process utils (this particular one could be moved elsewhere)
(cl-defun call-process* (program &rest args)
  "Returns the output of PROGRAM with ARGS. The method is based
on `call-process', so the underlying process is treated
synchronously. See `call-process' for more control of processes" 
  (with-temp-buffer
    (apply #'call-process program nil (current-buffer) nil args)
    (buffer-string-no-properties)))
;;(call-process* "file" (expand-file-name "~/projects/utils/elisp/external/excel.el"))

(cl-defun call-process-shell-command* (command &rest args)
  "Same as `call-process*' but calling
`call-process-shell-command' instead of `call-process'"
  (with-temp-buffer
    (apply #'call-process-shell-command command nil (current-buffer) nil args)
    (buffer-string-no-properties)))
;;(call-process-shell-command* "ls")

;;;; Wget methods
(cl-defun wget-unique-filename ()
  "Creates a quasi unique filename based on current time with resolution of a microsecond.
TODO: should be made a global util, or substitued with one if not already existing"
  (make-temp-file "wget" nil))

(cl-defun wget-basic (url filename &optional synchronous-p header)
  (if synchronous-p
    (start-process "wget" nil "wget" url "-O" filename)
    (call-process "wget" nil "*wget*" nil url "-O" (file-truename filename))))
;;(wget-basic "http://www.musedata.org/cgi-bin/mddata?composer=bach&edition=rasmuss&genre=inventio&work=0774&format=stage2&multi=zip" "~/data/musedata/BWV-0774.zip")

(cl-defun wget (url coding-system wget-after-method &rest args)
  "WGET-AFTER-METHOD is a method that is called when the
asynchronous process has finished. It's signature should be
\(HTML-STRING ARGS...\), where HTML-STRING is the html content just
downloaded by wget."
  (let ((tmp (wget-unique-filename)))
    (if wget-after-method
      (set-process-sentinel
       (wget-basic url tmp t)
       (apply #'wget-html-sentinel tmp coding-system wget-after-method args))
      (wget-basic url tmp nil))
    nil))

(cl-defun wget-html-sentinel (filename coding-system html-sentinel-function &rest args)
  "TODO: assure that process was ok."
  (let ((filename filename)
		(coding-system coding-system)
		(html-sentinel-function html-sentinel-function)
		(args args))
    #'(lambda (process event)
	(apply html-sentinel-function (file-string filename coding-system) args)
	(delete-file filename)
	(message "Finished!"))))

(cl-defun wget-to-temp-buffer (url coding-system buffer-name string-converter &rest args)
  '??)

(cl-defun wget-to-string (url)
  "TODO: handle coding-system"
 (let ((tmp (wget-unique-filename)))
   (wget-basic url tmp nil)
   (let ((res (file-string tmp)))
     (delete-file tmp)
     res)))
;;(wget-to-string "radio.nrk.no/direkte/klassisk")

(provide 'wget)
