;;; test-mb-utils-xml.el --- Tests for mb-utils-xml -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-utils-xml)

(defconst mb-utils-xml-test-node
  "<tr itemprop=\"albums\" itemscope data-id = '42'><a name=\"6159\"></a>Some inner text<a></a></tr>")

(ert-deftest mb-utils-xml-attributes ()
  (should (equal (xml-attribute "itemprop" mb-utils-xml-test-node)
		 "albums"))
  (should (equal (xml-attribute "data-id" mb-utils-xml-test-node) "42"))
  (should-not (xml-attribute "itemscope" mb-utils-xml-test-node))
  (should-not (xml-attribute "missing" mb-utils-xml-test-node))
  (let ((regexp (xml-attribute-regexp "data-id" "[0-9]+")))
    (should (string-match regexp mb-utils-xml-test-node))))

(ert-deftest mb-utils-xml-attribute-filters ()
  (let ((xml "<x other=\"yes\" class=\"epg-entry news\" disabled>text</x>"))
    (should (xml-extract-nodes xml "x" '(("class" "epg-entry.*"))))
    (should (xml-extract-nodes xml "x" '("disabled" "*")))
    (should-not (xml-extract-nodes xml "x" '(("class" "track.*"))))))

(ert-deftest mb-utils-xml-balanced-node-extraction ()
  (let ((xml
	 "<root><div class=\"outer\">A<div class=\"inner\">B</div>C</div><div class=\"last\">D</div></root>"))
    (should
     (equal (xml-extract-nodes xml "div")
	    '("<div class=\"outer\">A<div class=\"inner\">B</div>C</div>"
	      "<div class=\"last\">D</div>")))
    (should
     (equal (xml-extract-nodes xml "div" '(("class" "outer")))
	    '("<div class=\"outer\">A<div class=\"inner\">B</div>C</div>")))
    (should (equal (xml-extract-nodes xml "div" nil 1 t)
		   '("A<div class=\"inner\">B</div>C")))))

(ert-deftest mb-utils-xml-extraction-direction ()
  (let ((xml "<x>1</x><x>2</x><x>3</x>"))
    (should (equal (xml-extract-nodes xml "x" nil 2)
		   '("<x>1</x>" "<x>2</x>")))
    (should (equal (xml-extract-nodes xml "x" nil -2)
		   '("<x>3</x>" "<x>2</x>")))
    (should-not (xml-extract-nodes xml "x" nil 0))))

(ert-deftest mb-utils-xml-inner-content ()
  (should (equal (xml-inner mb-utils-xml-test-node) 
		 "<a name=\"6159\"></a>Some inner text<a></a>"))
  (should (equal (xml-inner-text mb-utils-xml-test-node)
		 "Some inner text"))
  (should-not (xml-inner "<x>missing close")))

(ert-deftest mb-utils-xml-decode-characters ()
  (should (equal (xml-decode-characters
		  "&lt;&gt;&amp;&quot;&apos;&#248;&#xF8;")
		 "<>&\"'øø")))

(ert-deftest mb-utils-xml-temp-buffer-sentinel ()
  (let ((buffer-name " *mb-utils-xml-output*"))
    (unwind-protect
	(progn
	  (funcall
	   (html-to-temp-buffer-sentinel
	    buffer-name
	    (lambda (html suffix) (concat html suffix))
	    "!")
	   "&quot;text&quot;")
	  (should (equal (with-current-buffer buffer-name (buffer-string))
			 "\"text\"!\n")))
      (when (get-buffer buffer-name)
	(kill-buffer buffer-name)))))

(provide 'test-mb-utils-xml)

;;; test-mb-utils-xml.el ends here
