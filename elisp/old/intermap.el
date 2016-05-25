;; Methods for enabling interactive maps.
;; ADDRESS usually means a city, DISTRICT usually means a country.
(require 'mb-utils-strings)
(require 'district)

(defun create-N-query (string)
  "See javascript 'CreateNQuery' in http://www.hvor.no/kart.aspx."
  (concat* (loop for char across string collect "N" collect (number-to-string char))))
;;(create-N-query "oslo")

(defun im-url (address &optional district)
  "Returns correct url to lookup ADDRESS in the intermap corresponding
to DISTRICT."
  (case (or district (second *district-current*))
    (no (im-hvor-url address))
    (otherwise (im-google-maps-url address))))
;;(im-url "oslo")

(defun im-lookup (address &optional district)
  "Display location of ADDRESS in DISTRICT."
  (let ((url (im-url (string-encode address) district)))
    (browse-url url)))
;;(im-lookup "henån" "se")

(defun im-mapquest-url (address &optional district small)
  "Returns the url to lookup ADDRESS in DISTRICT with size BIG in mapquest."
  (format "http://www.mapquest.com/maps/map.adp?size=%s&city=%s&country=%s"
    (if small "small" "big") address (or district 
					 (second *district-current*))))

(defun im-google-maps-url (address)
  "Returns the url to lookup ADDRESS in Google Maps."
  (format "http://maps.google.com/maps?f=q&hl=en&geocode=&q=%s&mrt=all" address))

(defun im-hvor-url (address)
  "Returns the url to lookup ADDRESS at site hvor.no."
  (format "http://www.hvor.no/kart/sok/?q=%s&x=0&y=0" address))
;;(im-hvor-url (url-encode "årnes"))

(defun im-show-place (district address)
  "Shows the sexp at point string in the intermap corresponding to
DISTRICT."
  (interactive 
   (list (when current-prefix-arg (district-set-current))
	 (read-string (format "Show place (%s): " (first *district-current*))
		      (or (word-at-point) ""))))
  (im-lookup address))
;;(im-show-place "Italy" "Parma")

(provide 'intermap)
