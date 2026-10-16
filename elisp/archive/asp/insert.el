(require 'mb-utils-div)
(cl-defun asp-insert-html-sec (sec) ""
  (interactive "*sSection: ")
  (insert "<" sec ">")
  (insert)
  (insert "</" sec ">"))

(cl-defun insert-html-include (file) ""
  (interactive "*FFile: ")
  (insert "<!-- #include file = \"" file "\" -->"))

(defvar asp-client-side-script-languge "VBScript")

(cl-defun asp-insert-cs-script () ""
  (interactive "*")
  (smart-insert "<SCRIPT LANGUAGE = \""
		asp-client-side-script-languge "\">")
  (smart-insert)
  (smart-insert "</SCRIPT>")
  (previous-line 2))

(cl-defun asp-insert-ss-script () ""
  (interactive "*")
  (smart-insert "<%  %>")
  (backward-char 4))

(cl-defun asp-insert-endl () ""
  (interactive "*")
  (insert "<br>"))
