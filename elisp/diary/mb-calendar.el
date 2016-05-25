(provide 'mb-calendar)

(require 'calendar)

(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'list-diary-entries-hook 'sort-diary-entries t)
(setq calendar-latitude 59.93)
(setq calendar-longitude 10.72)
(setq calendar-location-name "Oslo")
(setq calendar-time-zone +60)
(setq calendar-standard-time-zone-name "CET")
(setq calendar-daylight-time-zone-name "CET+1")
(setq calendar-daylight-savings-starts '(calendar-nth-named-day 1 0 4 year))
(setq calendar-daylight-savings-ends '(calendar-nth-named-day -1 0 10 year))
(setq european-calendar-style t)
(setq all-christian-calendar-holidays t)

(setq display-time-24hr-format t)
(display-time)

;;(add-hook 'today-visible-calendar-hook 'calendar-star-date)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(setq view-diary-entries-initially t)
(setq mark-diary-entries-in-calendar t)

(set-face-foreground 'calendar-today-face "cyan")
(set-face-underline-p 'calendar-today-face nil)
(setq number-of-diary-entries [4 3 3 3 3 6 5])

;(setq calendar-day-name-array
;  ["S,Ax(Bndag" "Mandag" "Tirsdag" "Onsdag" "Tordag" "Fredag" "L,Ax(Brdag"])

;(setq calendar-month-name-array
;  ["Januar" "Februar" "Mars" "April" "Mai" "Juni"
;   "Juli" "August" "September" "Oktober" "November" "Desember"])
