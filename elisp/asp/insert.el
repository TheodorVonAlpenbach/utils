(require 'mb-utils-div)
(defun asp-insert-html-sec (sec) ""
  (interactive "*sSection: ")
  (insert "<" sec ">")
  (insert)
  (insert "</" sec ">"))

(defun insert-html-include (file) ""
  (interactive "*FFile: ")
  (insert "<!-- #include file = \"" file "\" -->"))

(defvar asp-client-side-script-languge "VBScript")

(defun asp-insert-cs-script () ""
  (interactive "*")
  (smart-insert "<SCRIPT LANGUAGE = \""
		asp-client-side-script-languge "\">")
  (smart-insert)
  (smart-insert "</SCRIPT>")
  (previous-line 2))

(defun asp-insert-ss-script () ""
  (interactive "*")
  (smart-insert "<%  %>")
  (backward-char 4))

(defun asp-insert-endl () ""
  (interactive "*")
  (insert "<br>"))
