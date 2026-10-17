;;; mb-org.el --- Personal Org mode extensions -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'org)

(declare-function key-chord-define "key-chord" (keymap keys command))
(defvar evil-normal-state-local-map)

(defun mb-org-kbd-maps ()
  "Build and optionally install the personal Org prefix map.
Always bind the graphical TAB event to `org-cycle'.  Install the
`gh' key chord only when key-chord and an Evil local map are
available.  Return the prefix map."
  (let ((map (make-sparse-keymap))
        (move-map (make-sparse-keymap))
        (insert-map (make-sparse-keymap)))
    (local-set-key [tab] #'org-cycle)
    (define-key map "t" #'org-todo)
    (define-key map "m" move-map)
    (define-key move-map "j" #'org-move-subtree-down)
    (define-key move-map "k" #'org-move-subtree-up)
    (define-key move-map "h" #'org-do-promote)
    (define-key move-map "l" #'org-do-demote)
    (define-key move-map "H" #'org-promote-subtree)
    (define-key move-map "L" #'org-demote-subtree)
    (define-key map "i" insert-map)
    (define-key insert-map "d" #'mb-org-insert-date)
    (define-key insert-map "t" #'mb-org-insert-time)
    (define-key insert-map "f" #'mb-org-insert-fact-of-today)
    (define-key insert-map "a" #'mb-org-insert-arrival)
    (define-key insert-map "h" #'mb-org-insert-departure)
    (when (and (fboundp 'key-chord-define)
               (boundp 'evil-normal-state-local-map)
               (keymapp evil-normal-state-local-map))
      (key-chord-define evil-normal-state-local-map "gh" map))
    map))

(defun mb-org-init ()
  "Apply personal Org buffer setup."
  (mb-org-kbd-maps))

(add-hook 'org-mode-hook #'mb-org-init)

(defun insert-line-prefix (string)
  "Insert STRING at the beginning of the current line."
  (save-excursion
    (beginning-of-line)
    (insert string)))

(defun mb-org--format-date (time)
  "Format TIME as an ISO date."
  (format-time-string "%Y-%m-%d" time))

(defun mb-org--format-time (time)
  "Format TIME as hours and minutes."
  (format-time-string "%H:%M" time))

(cl-defun mb-org-insert-date (&optional
                              (prefix "* ")
                              (suffix "")
                              (time (current-time)))
  "Insert an Org date heading for TIME at the current line.
PREFIX and SUFFIX surround the ISO date.  Leave point at the
start of the original current line."
  (interactive)
  (beginning-of-line)
  (insert prefix (mb-org--format-date time) suffix "\n"))

(cl-defun mb-org-insert-time (&optional
                              (prefix "** ")
                              (time (current-time))
                              (suffix " "))
  "Insert an Org time heading for TIME at the current line.
PREFIX and SUFFIX surround the hour-minute timestamp.  Leave
point immediately before the inserted newline."
  (interactive)
  (beginning-of-line)
  (insert prefix (mb-org--format-time time) suffix)
  (let ((end (point)))
    (insert "\n")
    (goto-char end)))

(cl-defun mb-org-insert-fact-of-today
    (&key
     (text "Dagens lærdom")
     (prefix "** ")
     (time (current-time))
     (suffix " "))
  "Insert a timestamped fact heading.
TEXT follows the timestamp.  PREFIX, TIME, and SUFFIX are passed
to `mb-org-insert-time'."
  (interactive)
  (mb-org-insert-time prefix time suffix)
  (insert text)
  (forward-char 1))

(defun mb-org--prepare-append ()
  "Move to buffer end with exactly one blank line after existing text."
  (goto-char (point-max))
  (unless (= (point-min) (point-max))
    (skip-chars-backward "\n")
    (delete-region (point) (point-max))
    (insert "\n\n")))

(cl-defun mb-org-insert-arrival
    (&optional (time (time-subtract (current-time) (seconds-to-time 300))))
  "Append an arrival entry for TIME.
When omitted, TIME defaults to five minutes ago."
  (interactive)
  (mb-org--prepare-append)
  (mb-org-insert-date "* " "" time)
  (mb-org-insert-time "** " time " Ankomst")
  (goto-char (point-max)))

(cl-defun mb-org-insert-departure
    (&optional (time (time-add (current-time) (seconds-to-time 300))))
  "Insert a departure entry for TIME at the current line.
When omitted, TIME defaults to five minutes from now."
  (interactive)
  (mb-org-insert-time "** " time " Hjem")
  (goto-char (point-max)))

(provide 'mb-org)

;;; mb-org.el ends here
