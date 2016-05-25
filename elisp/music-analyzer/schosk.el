(require 'chord-skeleton)

(defun schosk-new () '(4 7))

(defun schosk-from-chosk (chosk)
  (mapcar (compose #'i-semitones #'i-parse) chosk))
;;(mapcar #'schosk-from-chosk (mapcar #'chosk-info-chosk chord-skeleton-info-list))

(defun schosk-to-chosk (schosk)
  (chosk-info-chosk (chosk-info-from-chord-type (schosk-to-chord-type schosk))))
;;(schosk-to-chosk '(3 7))

(defun schosk-info-from-chosk-info (chosk-info)
  (cons (schosk-from-chosk (chosk-info-chosk chosk-info))
	(rest chosk-info)))
;;(schosk-info-from-chosk-info (first chord-skeleton-info-list))

(defconst schord-skeleton-info-list
  (mapcar #'schosk-info-from-chosk-info chord-skeleton-info-list))

(defalias 'schosk-info-chosk 'first)
(defalias 'schosk-info-chord-type 'second) ;;TODO: rename to chord-type
(defalias 'schosk-info-chord-abbrev 'third) ;;TODO: rename to chord-abbrev or chord-symbol

(defun schosk-info-from-schosk (schosk)
  (find schosk schord-skeleton-info-list :key #'first :test #'equal))
;;(schosk-info-from-schosk '(3 7))

(defun* schosk-exists-p (schosk &optional legal-chord-types)
  (awhen (schosk-info-from-schosk schosk)
    (if legal-chord-types
      (find (schosk-info-chord-type it) legal-chord-types)
      it)))
;;(schosk-exists-p '(4 7) '(major-triad))

(defun schosk-to-chord-type (schosk)
  "Returns SCHOSK's chord-type"
  (schosk-info-chord-type (schosk-info-from-schosk schosk)))
;;(schosk-to-chord-type '(3 7))

(defun schosk-chord-type-group-p (schosk chord-type-group)
  "Returns non-nil iff schordx belongs to CHORD-TYPE-GROUP"
  (chord-type-group-p (schosk-to-chord-type schosk)
		      chord-type-group))
;;(schosk-chord-type-group-p '(4 7) 'dominants)

(provide 'schosk)
