(defconst +ru-locations+
  '((home . (599296 6644653))
    (work . (598970 6644149))
    (kindergarten . (598970 6644149))))

(defun ru-location (key) (cdr (assoc key +ru-locations+)))
;;(mapcar #'ru-location (mapcar #'car +ru-locations+))

(defconst +ru-default-location+ 'home
  "Default location for searches. Either a symbol or (X Y) in
  UTM)")

(defun qcommand (url)
  (format "curl -s -H \"Accept: application/json\" http://reisapi.ruter.no/%s | python -m json.tool" url))
;;(qcommand "Place/GetPlaces/Sofienberg?location=\\(x=600000,y=6642000\\)")

(defun qcurl (url)
  (shell-command-to-string (qcommand url)))
;;(qcurl "Place/GetPlaces/Sofienberg?location=\\(x=600000,y=6642000\\)")
;;(qcurl "Travel/GetTravels?fromPlace=Blindern%20\\(omr%C3%A5de\\)&toPlace=Carl%20Berners%20plass%20\\(Chr.Mich.g.\\)%20\\(Oslo\\)&isAfter=100220161526")

(defun qjson (url)
  (json-read-from-string (shell-command-to-string (qcommand url))))
;;(qjson "Place/GetPlaces/Sofienberg?location=\\(x=600000,y=6642000\\)")
;;(qjson "Place/GetClosestStops?coordinates=\\(x=599296,y=6644653\\)&proposals=25&maxdistance=2000")
;;(qjson "Place/GetClosestStops?coordinates=\\(x=599296,y=6644653\\)&proposals=25&maxdistance=2000")
;;(qcurl "Trip/GetTrip/10/time=100220161526")

(defun format-location (location)
  (destructuring-bind (x y)
      (if (symbolp location) (ru-location location) location)
    (format "\\(x=%d,y=%d\\)" x y)))
;;(mapcar #'format-location '(home (1 2)))

(cl-defun ru-get-places-url (id &optional (location +ru-default-location+)
				(counties '(oslo)))
  "It seems either LOCATION or COUNTIES is possible not both."
  (url-encode-url (format "Place/GetPlaces/%s?location=%s&%s"
      id (format-location location) "counties=oslo")))
;;(ru-get-places-url "Sofienberg")

(cl-defun ru-get-places (id &rest args)
  (qjson (apply #'ru-get-places-url id args)))
;;(ru-get-places "Sofienberg")

(cl-defun ru-get-closest-stops-url (&optional
				    (location +ru-default-location+)
				    (max-distance 2000)
				    (n 25))
  (format "Place/GetClosestStops?coordinates=%s&proposals=%d&maxdistance=%d"
     (format-location location) n max-distance))
;;(ru-get-closest-stops-url)

(cl-defun ru-get-closest-stops (&rest args)
  (qjson (apply #'ru-get-closest-stops-url args)))
;;(ru-get-closest-stops)

(defun stop-name (stop) (cdr (assoc 'Name stop)))
;;(mapcar #'stop-name (ru-get-closest-stops 'home))

(defun ru-time (dttm)
  (if dttm
    (destructuring-bind (s mi h d mo y z w x) dttm
      (format "%02d%02d%04d%02d%02d%02d" d mo y h mi s))
    ""))
;;(mapcar #'ru-time (list (now) nil (weekstart)))



(cl-defun ru-get-travels-url (from to after ...
			   &key
			     time
			     (change-margin 2)
			     (change-punish 8)
			     (walking-factor 70)
			     (n 10)
			     (transport-types nil)
			     (max-walking-minutes nil)
			     (line-names nil)
			     (walk-reluctance 5))
  "Not in use. See `ru-get-travels'."
  (url-encode-url
   (format "Travel/GetTravels?fromPlace=%s&toPlace=%s&isAfter=%s"
     from to after)))
;;(ru-get-travels-url "Blindern (område)" "Carl Berners plass (Chr.Mich.g.) (Oslo)" "100220161526")

(defun ru-get-travels (from to after &rest args)
  "Not in use. All requests seem to be rejected by Ruter's API."
  (qjson (apply #'ru-get-travels-url from to after args)))
;;(ru-get-travels "Blindern (område)" "Carl Berners plass (Chr.Mich.g.) (Oslo)" "100220161526")

(provide 'ruter)
