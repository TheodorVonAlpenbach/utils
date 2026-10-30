;;; global-map.el --- Personal global key bindings -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'info)

(declare-function inc-thing-at-point "mb-things")
(declare-function inferior-lisp nil)
(declare-function inferior-lisp-program-with-lispinit nil)
(declare-function mb-all-grep "mb-grep")
(declare-function mb-cpp-grep "mb-grep")
(declare-function mb-grep "mb-grep")
(declare-function mb-h-grep "mb-grep")
(declare-function print-notepad-buffer "mb-print")
(declare-function print-notepad-file "mb-print")
(declare-function print-notepad-region "mb-print")

(defvar inferior-lisp-program)

(defvaralias 'print-map 'mb-global-print-map)
(defvaralias 'insert-map 'mb-global-insert-map)
(defvaralias 'greek-map 'mb-global-greek-map)
(defvaralias 'find-map 'mb-global-find-map)

(defvar mb-global-print-map (make-sparse-keymap "Print")
  "Prefix map for personal printing commands.")

(defvar mb-global-insert-map (make-sparse-keymap "Insert")
  "Prefix map for personal insertion commands.")

(defvar mb-global-greek-map (make-sparse-keymap "Greek")
  "Prefix map for inserting Greek letters.")

(defvar mb-global-find-map (make-sparse-keymap "Find")
  "Prefix map for personal search commands.")

(defun mb-global-map-other-window-backward ()
  "Select the preceding window."
  (interactive)
  (other-window -1))

(defun mb-global-map-switch-to-other-buffer ()
  "Switch to the most recently used other buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun mb-global-map-run-inferior-lisp ()
  "Start the configured inferior Lisp."
  (interactive)
  (inferior-lisp inferior-lisp-program))

(defun mb-global-map-run-inferior-lisp-with-init ()
  "Start the configured inferior Lisp with its initialization file."
  (interactive)
  (inferior-lisp (inferior-lisp-program-with-lispinit)))

(defun mb-global-map-insert-command (string)
  "Return an interactive command that inserts STRING."
  (lambda ()
    (interactive)
    (insert string)))

(defun mb-global-map-increment-1 (count)
  "Increment the thing at point by COUNT units."
  (interactive "p")
  (inc-thing-at-point count 1))

(defun mb-global-map-increment-2 (count)
  "Increment the thing at point by COUNT larger units."
  (interactive "p")
  (inc-thing-at-point count 2))

(defun mb-global-map-increment-3 (count)
  "Increment the thing at point by COUNT largest units."
  (interactive "p")
  (inc-thing-at-point count 3))

(defun gfind-compile (pattern &optional file-extension level)
  "Compile a gfind command for PATTERN, FILE-EXTENSION, and LEVEL."
  (setq file-extension (or file-extension "")
        level (or level 0))
  (compile
   (mapconcat #'shell-quote-argument
              (list "gfind" "-u" (number-to-string level)
                    pattern file-extension)
              " ")))

(defun gfind (level)
  "Search for the symbol at point using gfind.
With prefix LEVEL, limit the search to that directory depth."
  (interactive "P")
  (let ((extension (and buffer-file-name
                        (file-name-extension buffer-file-name))))
    (gfind-compile
     (or (thing-at-point 'symbol t)
         (read-from-minibuffer "Find in files: "))
     (or extension "")
     (prefix-numeric-value (or level 0)))))

(defun mb-global-map--define-insertions ()
  "Populate personal prefix maps."
  (define-key mb-global-print-map "r" #'print-notepad-region)
  (define-key mb-global-print-map "f" #'print-notepad-file)
  (define-key mb-global-print-map "b" #'print-notepad-buffer)

  (dolist (binding
           '(("g" . insert-guillemets)
             ("q" . insert-quotes)
             ("'" . insert-single-quotes)
             ("\"" . insert-double-quotes)
             ("*" . insert-asterisks)
             ("[" . insert-square-brackets)
             ("{" . insert-curly-brackets)
             ("<" . insert-angle-brackets)
             ("x" . insert-xml-tag)
             ("p" . insert-post-scriptum)
             ("b" . insert-symbol-bullet)
             ("s" . insert-skip-quote)
             ("u" . insert-underline)
             ("d" . insert-date)
             ("t" . insert-time)
             ("D" . insert-date-and-time)
             ("i" . insert-todo-item)
             ("e" . insert-expression)))
    (define-key mb-global-insert-map (car binding) (cdr binding)))

  (dolist (binding
           '(("1" . "¹") ("2" . "²") ("3" . "³") ("0" . "°")
             ("o" . "º") ("a" . "ª") ("m" . "¯") ("B" . "ß")
             ("c" . "ç")))
    (define-key mb-global-insert-map (car binding)
                (mb-global-map-insert-command (cdr binding))))

  (cl-loop for latin across
           "ABCDEFGHIJKLMNOPRSTUVWXYZabcdefghijklmnoprstuvwxyz"
           for greek across
           "ΑΒΨΔΕΦΓΗΙΞΚΛΜΝΟΠΡΣΤΘΩςΧΥΖαβψδεφγηιξκλμνοπρστθωςχυζ"
           do (define-key mb-global-greek-map (string latin)
                          (mb-global-map-insert-command (string greek))))

  (define-key mb-global-find-map "g" #'mb-grep)
  (define-key mb-global-find-map "h" #'mb-h-grep)
  (define-key mb-global-find-map "c" #'mb-cpp-grep)
  (define-key mb-global-find-map "a" #'mb-all-grep))

(defun mb-global-map-setup (&optional map)
  "Install personal bindings in MAP, defaulting to `global-map'."
  (interactive)
  (let ((target (or map global-map)))
    (mb-global-map--define-insertions)
    (define-key lisp-mode-shared-map (kbd "C-c e") #'eval-buffer)
    (define-key lisp-mode-shared-map (kbd "C-c r") #'eval-region)
    (define-key lisp-mode-shared-map (kbd "C-c C-c") #'comment-region)
    (define-key Info-mode-map (kbd "M-n") #'forward-paragraph)
    (dolist (binding
             `(("C-c C-p" . print)
               ("M-n" . forward-paragraph)
               ("M-p" . backward-paragraph)
               ("C-M-S-u" . up-list)
               ("C-M-<backspace>" . backward-kill-sexp)
               ("M-U" . kill-paragraph)
               ("C-<backspace>" . backward-kill-paragraph)
               ("C-M-S-l" . lift-sexp)
               ("C-M-m" . back-to-indentation-and-indent)
               ("M-#" . mark-whole-word)
               ("M-T" . transpose-paragraphs)
               ("C-M-S-T" . transpose-sentences)
               ("M-S-u" . upcase-previous-word)
               ("M-S-l" . downcase-previous-word)
               ("M-S-c" . capitalize-previous-word)
               ("C-c C-q" . fill-region)
               ("C-c M-q" . fill-region-as-paragraph)
               ("C-c +" . c++-mode)
               ("C-c c" . compile)
               ("C-c M-n" . next-error)
               ("C-c M-p" . previous-error)
               ("C-c M-<" . first-error)
               ("<f11>" . mb-global-map-switch-to-other-buffer)
               ("S-<f11>" . bury-buffer)
               ("M-<f11>" . smart-swap)
               ("C-M-<f11>" . delete-window)
               ("C-<tab>" . other-window)
               ("C-S-<tab>" . mb-global-map-other-window-backward)
               ("S-<f8>" . goto-line)
               ("<f8>" . shell)
               ("<f7>" . mb-global-map-run-inferior-lisp)
               ("S-<f7>" . mb-global-map-run-inferior-lisp-with-init)
               ("<f6>" . lynx)
               ("S-<f6>" . ielm)
               ("<f5>" . load-file)
               ("<f3>" . shrink-window)
               ("<f4>" . enlarge-window)
               ("S-<f3>" . shrink-window-horizontally)
               ("S-<f4>" . enlarge-window-horizontally)
               ("<f1>" . dic-lookup-at-point)
               ("M-<f3>" . shrink-frame)
               ("M-<f4>" . enlarge-frame)
               ("M-S-<f3>" . shrink-frame-horizontally)
               ("M-S-<f4>" . enlarge-frame-horizontally)
               ("C-c p" . ,mb-global-print-map)
               ("C-c i" . ,mb-global-insert-map)
               ("C-x 8 g" . ,mb-global-greek-map)
               ("C-c f" . ,mb-global-find-map)
               ("C-+" . mb-global-map-increment-1)
               ("M-+" . mb-global-map-increment-2)
               ("C-M-+" . mb-global-map-increment-3)))
      (define-key target (kbd (car binding)) (cdr binding)))
    target))

(provide 'global-map)

;;; global-map.el ends here
