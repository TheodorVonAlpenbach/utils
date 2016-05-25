;; -*- emacs-lisp -*-

;;; DEL 1: Innkommende meldinger

;(setq gnus-select-method '(nntp "news.chello.no"))
;(setq gnus-select-method '(nntp "news.online.no"))

(setq mail-sources
      '((file)
;  	(pop :server "pop3.online.no"
;  	 :user "matbergs"
;  	 :password "263esky")
;  	(pop :server "mail.chello.no"
;  	 :user "mbergstr"
;  	 :password "ecelmyok8")
  	(pop :server "pop.normail.no"
  	 :user "pop34856"
  	 :password "atn-6h-c")
 	(pop :server "pop.mail.yahoo.no"
 	 :user "mats_bergstrom"
 	 :password "BeatusVir")))

;; oppretter en lokal folder-basert server under "~/Mail/Mats"
;; én mail-gruppe vil tilsvare én fil i denne katalogen
(setq gnus-secondary-select-methods
      '((nnfolder ""
	 (nnfolder-directory "c:/unix/Mail/Mats")
	 (nnfolder-active-file "c:/unix/Mail/Mats/active")
	 (nnfolder-newsgroups-file "c:/unix/Mail/Mats/newsgroups"))))

;; Tilsvarende for utgående mail og news
(setq gnus-message-archive-method
      '(nnfolder "archive"
	(nnfolder-inhibit-expiry t)
	(nnfolder-directory "c:/unix/Mail/Mats/archive")
	(nnfolder-active-file "c:/unix/Mail/Mats/archive/active")))

;; Utgående news havner i folderen "sent-news", mail i "sent-mail"
(setq gnus-message-archive-group
      '((if (message-news-p)
	  "sent-news"
          "sent-mail")))

;;(string-match "@elkem\\.no\\|Steinar Rune Eriksen\\|Liv\\.Kvalsvik" "Frm: huy.hoang@elkem.no")

;; Et par instillinger
(setq gnus-permanently-visible-groups nil)
(add-hook 'gnus-summary-prepare-hook 'gnus-summary-sort-by-date)

;;; DEL 2: Utgående meldinger
(setq user-full-name "Mats Bergstrøm")
(setq user-mail-address "mats_bergstrom@yahoo.no")
(setq mail-self-blind nil)

(setq send-mail-function 'smtpmail-send-it)
;(setq smtpmail-default-smtp-server "smtp.chello.no")
;(setq smtpmail-default-smtp-server "smtp.normail.no")
;(setq smtpmail-default-smtp-server "smtp.mail.yahoo.no")
(setq smtpmail-smtp-service "smtp")
(setq smtpmail-local-domain nil)
(setq smtpmail-debug-info t)
(load-library "smtpmail")
(setq smtpmail-code-conv-from nil)

(load-library "message")
(setq message-send-mail-function 'smtpmail-send-it)

;; redefinition
(defsubst gnus-simplify-subject-re (subject)
  "Remove \"Re:\" from subject lines. MB: also remove \"Ad:\"."
  (if (string-match "^\\([Rr][Ee]\\|[Aa][Dd]\\): *" subject)
;;  (if (string-match "^[Aa][Dd]: *" subject)
      (substring subject (match-end 0))
    subject))

(setq gnus-simplify-subject-functions
      (list #'gnus-simplify-subject-re))

(setq nnmail-split-methods 'nnmail-split-fancy
      nnmail-split-fancy
     '(| (any "@elkem\\.no\\|Steinar Rune Eriksen\\|Liv\\.Kvalsvik" 
"tx+elkem")
	 (any "edb\\.com"
	  (any "knutjer" "private+de-eldres-raad"))
         (any "anders\\.frovig@dnb\\.no"
	  (any "haakon\\.bors\\.lind@dnb\\.no"
	   "private+gg"))
	 (to "firmapost@transistix\\.com" "tx+firmapost")
	 (any "gresk:" "private+gresk")
	 (any "@geo-guide\\.com" "tx+geoguide")
	 (any "clisp-list@.*\\.sourceforge\\.net" "mailing-list+clisp-list")
	 (any "post@gethlp\\.no" "mbpf+gethlp")
	 (from "Yahoo!" "private+yahoo")
	 (any "Per Bjorge" "mbpf+fiw")
	 (any "@oslodomkor\\.no\\|Dagfinn Hovland\\|marianne.lund" 
"private+coro")
	 ;; persons
	 (from "Lars Brusletto Sveen" "private+lbs")
	 (any "@wilhelmsen\\.no" "mbpf+wilhelmsen")
	 (from "haakon\\.bors\\.lind@dnb\\.no" "private+haakon")
	 (from "anders\\.frovig@dnb\\.no" "private+anders")
	 (from "gherman@darwin\\.in-berlin\\.de" "private+dinu")
	 (from "Dahl Tore" "private+tore")
	 (from "ole-martin\\.halck@ffi\\.no" 
	  (from "omhalck@bredband\\.no" "private+ole-martin"))
	 (from "Per Kristian Nilsen" "private+per-kristian")
	 (any "Christian Skaug" "private+skaug")
	 (any "Christian Thorn" "private+skaug")
	 (any "warloe@kavli\\.no" "private+christine")
	 (any
	  
"@contango\\.no\\|nordpool\\|kjorsvik\\|fiskvik\\|@hydro\\|@elkem\\|tim rear" "contango")
	 (any "@nfi\\.no" "private+cinemateket")
         (any "Lars Petter Endresen" "private+lars-petter")
         (any "Snorre Farner" "private+snorre")
	 (from "egbert" "tx+staff")
	 "incoming"))

;; load mb redefinitions
(load "mb-gnus")
