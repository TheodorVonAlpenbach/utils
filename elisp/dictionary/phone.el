;; Methods for enabling interactive maps.
;;http://online.telefonkatalogen.no/katalog/katalog.php4?streng=mats+bergstr%F8m&medium=&privat=on&nlivoff=on&tmob=on&intern=on#
(require 'mb-utils-strings)
(require 'district)

(defun ph-url (name &optional district)
  "Returns correct url to lookup NAME in the phonemap corresponding
to DISTRICT."
  (setq district (or district *district-current*))
  (string-case district
    ("Norway" (format "http://online.telefonkatalogen.no/katalog/katalog.php4?streng=%s&medium=&privat=on&nlivoff=on&tmob=on&intern=on#" name))
    (otherwise (error (format "Do not support phone district %s" district)))))
;;(ph-url "Ketil Næss Kristensen" nil)

(defun* ph-lookup-base (name &optional (district *district-current*))
  "Display location of NAME in the phonemap corresponding to DISTRICT."
  (let ((url (ph-url (string-encode name) district)))
    (browse-url url)))
;;(ph-lookup-base "Mats Bergstrøm")

(defun ph-lookup (name)
  "Display location of NAME in the phonemap corresponding to DISTRICT."
  (interactive "sName: ")
  (ph-lookup-base))
;;(ph-lookup "Mats Bergstrøm")

(defun ph-search (arg)
  "Shows the sexp at point string in the intermap corresponding to
DISTRICT. If optional prefix argument is a number, the address base is
updated. If given and not a number, the user may change distict before
continuing."
  (interactive "P")
  (when (and arg (not (numberp arg)))
    (message "type: %S" (type-of arg))
    (district-set-current))
  (ph-lookup (first (mb-query-address-base
		     (format "Name (%s): " *district-current*)
		     (and arg (numberp arg))))))
;;(ph-search nil)

(provide 'phone)
