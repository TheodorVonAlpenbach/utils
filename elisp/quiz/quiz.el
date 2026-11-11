;;; quiz.el --- Edit legacy quiz files -*- coding: utf-8; -*-

;;; TODO
;;; * this file should only contain settings and mappings, maybe also other
;;;   methods that do not belong to other files
;;; * this file should control including the other quiz lisp files (with
;;;   'require etc)

;;; o mail function (with mail list)
;;; o fix fill prefix thing (maybe the best thing is to provide a quiz mode)
;;; o coloring (set appropriate faces)

;;; * make a re-number-items method (sometimes a q-question must be deleted, and
;;;   then...)
;;; * base methods on "objects": 
;;;   · q-file/q-buffer (quiz buffer or summary quiz buffer)
;;;   · q-list (entire quiz-buffer or region of summary buffer)
;;;   · q-question (entity containing at least a q-item)
;;;   · q-item (either Q, A, S by now; later I might add C for comment, T: for
;;;     the durations of the validity)

;; see legenda.txt for categories

(require 'cl-lib)
(require 'subr-x)
(require 'quiz-format)
(require 'quiz-navigation)
(require 'quiz-mode)
(require 'quiz-config)
(require 'quiz-edit)
(require 'quiz-layout)
(require 'quiz-cleanup)
(require 'quiz-csv)
(require 'quiz-summary)

;;; Mail functionality
(defconst quiz-default-mail-group "Hovedstyret av 1914")
(defconst quiz-mail-groups
  (list (list "Hovedstyret av 1914"
	      (list "Tore van Dahl <toredah@student.matnat.uio.no>"
		    "Per Stovne <perstovne@hotmail.com>"
		    "Truls Flatberg <Truls.Flatberg@sintef.no>"
		    "Ole Martin Halck <omhalck@online.no>"
		    "Truls Flatberg <trulsf@ifi.uio.no>"
		    "Thomas Bergkirk <thomas.bergkirk@rubicontv.no>"))))

(cl-defun purify-address (address)
  (string-match* "<?\\([^ >]*@[^ >]*\\)>?" address 1))

(cl-defun quiz-mail-buffer (refresh)
  "Sends current quiz buffer as mail to recipients"
  (interactive "P")
  (if refresh (mb-refresh-addresses))
  (let* ((group quiz-default-mail-group)
	 (to (concat* (second (assoc group quiz-mail-groups))
	       :in ", "
	       :key #'purify-address))
	 (subject (buffer-name))
	 (body (buffer-string)))
    (gnus-msg-mail "" subject)
    (auto-fill-mode -1)
    (message-goto-to)
    (insert to)
    (message-goto-body)
    (insert body)))
;;(quiz-mail-buffer 1)
;;(concat* (second (assoc quiz-default-mail-group quiz-mail-groups)) :in ", ")

(require 'mb-utils-buffer)

(cl-defun quiz-convert-aftenposten-nyhetsquiz-question-to-qz-question
    (beg end n &optional s)
  "STRING is aftenposten question string. Returns a string question in
qz format. The question is numbered N."
  (let* ((string (buffer-substring beg end))
	 (q (string-match* "^[0-9]+\\.\\s-*\\(.*\\)$" string 1))
	 (a (string-match* "^Riktig svar:\\s-*\\(.*\\)$" string 1))
	 s)
    (kill-region beg end)
    (goto-char beg)
    (insert (quiz-item-q n q))
    (insert (quiz-item-a n a))
    (when s (insert (quiz-item-s n s)))))

(cl-defun quiz-convert-aftenposten-nyhetsquiz-to-qz-file ()
  (interactive)
  (let ((n 0)) 
    (buffer-do-regions beg end ("\\(.*\n.*\n\\)\\s-*\n" 1)
      ;;(message (buffer-substring beg (+ beg 20)))
      (quiz-convert-aftenposten-nyhetsquiz-question-to-qz-question
       beg end (cl-incf n)))))

(provide 'quiz)

;; For referering til IDLs hjemmesider:
;; http://www.mylder.no/idl/views/640&ref0=291

;; Kirkeinndelinger og rettslige inndelinger:
;; http://www.ssb.no/aarbok/tab/t-000010-001.html

;; http://www.kongehuset.no/ 
