;;; cram-common.el --- Cram record accessors -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ld-metadata)

;;; Users

(defun cram-user-id (user)
  "Return USER's identifier."
  (car user))

(defun cram-user-name (user)
  "Return USER's name."
  (cadr user))

(defun cram-user-rating (user)
  "Return USER's rating pair."
  (cl-third user))

(defun cram-user-rating-e (user)
  "Return USER's estimated rating."
  (car (cram-user-rating user)))

(defun cram-user-rating-d (user)
  "Return USER's rating deviation."
  (cadr (cram-user-rating user)))

(defun cram-user-metadata (user)
  "Return USER's metadata."
  (car (last user)))

(defun cram-user-last-updated (user)
  "Return USER's last-updated timestamp."
  (ld-get-metadatum :updated (cram-user-metadata user)))

;;; Problems

(defun cram-problem-id (problem)
  "Return PROBLEM's identifier."
  (car problem))

(defun cram-problem-source-id (problem)
  "Return PROBLEM's source identifier."
  (cadr problem))

(defun cram-problem-question (problem)
  "Return PROBLEM's question."
  (cl-third problem))

(defun cram-problem-answer (problem)
  "Return PROBLEM's answer."
  (cl-fourth problem))

(defun cram-problem-picture (problem)
  "Return PROBLEM's picture."
  (cl-fifth problem))

(defun cram-problem-alternatives (problem)
  "Return PROBLEM's alternative answers."
  (cl-sixth problem))

(defun cram-problem-hints (problem)
  "Return PROBLEM's hints."
  (cl-seventh problem))

(defun cram-problem-rating (problem)
  "Return PROBLEM's rating pair."
  (cl-eighth problem))

(defun cram-problem-rating-e (problem)
  "Return PROBLEM's estimated rating."
  (car (cram-problem-rating problem)))

(defun cram-problem-rating-d (problem)
  "Return PROBLEM's rating deviation."
  (cadr (cram-problem-rating problem)))

(defun cram-problem-metadata (problem)
  "Return PROBLEM's metadata property list."
  (cdr (cl-ninth problem)))

(defun cram-problem-updated (problem)
  "Return PROBLEM's last-updated timestamp."
  (plist-get (cram-problem-metadata problem) :updated))

;;; Matches

(defun cram-match-id (match)
  "Return MATCH's identifier."
  (car match))

(defun cram-match-timestamp (match)
  "Return MATCH's timestamp."
  (cadr match))

(defun cram-match-response (match)
  "Return MATCH's response."
  (cl-third match))

(defun cram-match-time (match)
  "Return MATCH's elapsed time."
  (cl-fourth match))

(defun cram-match-score (match)
  "Return MATCH's score."
  (cl-fifth match))

(defun cram-match-user-id (match)
  "Return MATCH's user identifier."
  (cl-sixth match))

(defun cram-match-problem-id (match)
  "Return MATCH's problem identifier."
  (cl-seventh match))

(defun cram-match-user-rating (match)
  "Return MATCH's user rating."
  (cl-eighth match))

(defun cram-match-problem-rating (match)
  "Return MATCH's problem rating."
  (cl-ninth match))

(provide 'cram-common)

;;; cram-common.el ends here
