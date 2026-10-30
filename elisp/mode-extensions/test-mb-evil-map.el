;;; test-mb-evil-map.el --- Tests for mb-evil-map -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mb-evil-map)

(defvar evil-normal-state-map)
(defvar evil-insert-state-map)
(defvar evil-outer-text-objects-map)
(defvar evil-inner-text-objects-map)

(declare-function mb-evil-buffer "mb-evil")
(declare-function mb-evil-inner-defun "mb-evil")
(declare-function buffer-file-name-to-clipboard nil)
(declare-function mb-eval-defun nil)

(ert-deftest mb-evil-map-loads-without-evil ()
  (should (fboundp 'mb-evil-map-setup))
  (should (keymapp mb-evil-eval-map))
  (should (keymapp mb-evil-clipboard-map))
  (should (eq *eval-map* mb-evil-eval-map))
  (should (eq *clipboard-map* mb-evil-clipboard-map)))

(ert-deftest mb-evil-map-skips-activation-when-unavailable ()
  (let (required)
    (cl-letf (((symbol-function 'locate-library)
               (lambda (_library) nil))
              ((symbol-function 'require)
               (lambda (&rest args) (setq required args))))
      (should-not (mb-evil-map-activate-if-available))
      (should-not required))))

(ert-deftest mb-evil-map-setup-requires-integrations ()
  (cl-letf (((symbol-function 'featurep) (lambda (_feature) nil)))
    (should-error (mb-evil-map-setup) :type 'user-error)))

(ert-deftest mb-evil-map-installs-prefix-maps ()
  (let ((evil-normal-state-map (make-sparse-keymap))
        (evil-insert-state-map (make-sparse-keymap))
        (evil-outer-text-objects-map (make-sparse-keymap))
        (evil-inner-text-objects-map (make-sparse-keymap))
        (vc-prefix-map (make-sparse-keymap))
        chord-mode-argument
        chords)
    (cl-letf (((symbol-function 'key-chord-mode)
               (lambda (argument)
                 (setq chord-mode-argument argument)))
              ((symbol-function 'featurep)
               (lambda (feature)
                 (memq feature '(mb-evil key-chord))))
              ((symbol-function 'key-chord-define)
               (lambda (map key definition)
                 (push (list map key definition) chords)))
              ((symbol-function 'evil-key-chord-define)
               (lambda (states map key definition &rest _bindings)
                 (push (list states map key definition) chords)))
              ((symbol-function 'insert-n)
               (lambda (_string) #'ignore)))
      (mb-evil-map-setup))
    (should (= chord-mode-argument 1))
    (should
     (cl-find-if (lambda (entry)
                   (and (equal (nth 1 entry) "kj")
                        (eq (nth 2 entry) mb-evil-eval-map)))
                 chords))
    (should
     (cl-find-if (lambda (entry)
                   (and (equal (nth 1 entry) "cv")
                        (eq (nth 2 entry) mb-evil-clipboard-map)))
                 chords))
    (should (eq (lookup-key mb-evil-eval-map "d") #'mb-eval-defun))
    (should (eq (lookup-key mb-evil-clipboard-map "f")
                #'buffer-file-name-to-clipboard))
    (should (eq (lookup-key evil-outer-text-objects-map "g")
                #'mb-evil-buffer))
    (should (eq (lookup-key evil-inner-text-objects-map "D")
                #'mb-evil-inner-defun))))

(provide 'test-mb-evil-map)

;;; test-mb-evil-map.el ends here
