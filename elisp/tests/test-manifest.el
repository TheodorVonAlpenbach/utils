;;; test-manifest.el --- Maintained Emacs Lisp test manifest -*- lexical-binding: t; -*-

(defconst mb-tests-elisp-directory
  (file-name-directory
   (directory-file-name (file-name-directory load-file-name))))

(defconst mb-tests-load-directories
  '("" "download" "elisp" "external" "games/cram" "games/maths" "global"
    "list-database" "mode-extensions"
    "projects/ratings" "prototypes/geometry" "quiz" "utils"))

(defconst mb-tests-baseline-files
  '("prototypes/geometry/test-list-interval.el"
    "prototypes/geometry/test-list-envelope.el"
    "elisp/test-mb-paragraph.el"
  "elisp/test-mb-sexp.el"
  "global/test-global-map.el"
  "global/test-mb-buffer.el"
  "global/test-mb-frames.el"
  "global/test-LS.el"
  "global/test-mb-hexl.el"
  "global/test-mb-locale.el"
  "list-database/test-ld-identifier.el"
  "list-database/test-ld-model.el"
  "list-database/test-ld-global.el"
  "list-database/test-ld-operations.el"
  "list-database/test-ld-general.el"
  "list-database/test-ld-repository.el"
  "list-database/test-ld-consumers.el"
  "list-database/test-list-db.el"
  "games/cram/test-cram-backend.el"
  "games/cram/test-cram-config.el"
  "games/cram/test-cram-db-ui.el"
  "games/cram/test-cram-import.el"
  "games/cram/test-cram.el"
  "games/maths/test-maths-backend.el"
  "games/maths/test-maths-db-ui.el"
  "games/maths/test-maths.el"
    "global/test-mb-indent.el"
  "global/test-mb-insert.el"
  "global/test-mb-print.el"
  "global/test-mb-grep.el"
  "global/test-smart-swap.el"
    "mode-extensions/test-backus-naur-mode.el"
    "mode-extensions/test-mb-doc-view.el"
    "mode-extensions/test-mb-emacs-lisp.el"
    "mode-extensions/test-mb-ert.el"
    "mode-extensions/test-mb-evil-map.el"
    "mode-extensions/test-mb-java-mode.el"
    "mode-extensions/test-mb-js-mode.el"
    "mode-extensions/test-mb-json.el"
    "mode-extensions/test-mb-lilypond.el"
    "mode-extensions/test-mb-lisp.el"
    "mode-extensions/test-mb-metafont.el"
    "mode-extensions/test-mb-octave-eval.el"
    "mode-extensions/test-mb-octave-insert.el"
    "mode-extensions/test-mb-octave-matlab.el"
    "mode-extensions/test-mb-octave.el"
    "mode-extensions/test-octave-breakpoint.el"
    "mode-extensions/test-octave-debug-mode.el"
    "mode-extensions/test-mb-org.el"
    "mode-extensions/test-mb-python.el"
    "mode-extensions/test-mb-ruby.el"
    "mode-extensions/test-mb-sql.el"
    "mode-extensions/test-mb-texinfo.el"
    "projects/ratings/test-glicko.el"
    "projects/ratings/test-rating-plot.el"
    "quiz/test-quiz-edit.el"
    "quiz/test-quiz-format.el"
    "quiz/test-quiz-mode.el"
    "quiz/test-quiz-navigation.el"
    "quiz/test-quiz-csv.el"
    "utils/test-mb-anaphora.el"
    "utils/test-mb-combinatorics.el"
    "utils/test-mb-polynomials.el"
    "utils/test-mb-table.el"
    "utils/test-mb-math-functions.el"
    "utils/test-mb-utils-math.el"
    "utils/test-mb-lists.el"
    "utils/test-mb-sequences.el"
    "utils/test-mb-things.el"
    "utils/test-mb-utils-div.el"
    "utils/test-mb-utils-economy.el"
    "utils/test-mb-utils-buffer.el"
    "utils/test-mb-utils-file.el"
    "utils/test-mb-utils-geometry.el"
    "utils/test-mb-utils-io.el"
    "utils/test-mb-utils-iso-time.el"
    "utils/test-mb-utils-matrix.el"
    "utils/test-mb-utils-physics.el"
    "utils/test-mb-utils-regexp.el"
    "utils/test-mb-utils-search.el"
    "utils/test-mb-utils-sets.el"
    "utils/test-mb-utils-strings.el"
    "utils/test-mb-utils-time.el"
    "utils/test-mb-utils-tree.el"
    "utils/test-mb-utils-xml.el"))

(defun mb-tests-add-load-paths ()
  "Add maintained test directories to `load-path'."
  (dolist (directory mb-tests-load-directories)
    (add-to-list 'load-path
                 (expand-file-name directory mb-tests-elisp-directory))))

(defun mb-tests-load-baseline ()
  "Load every test file in the maintained baseline."
  (mb-tests-add-load-paths)
  (dolist (file mb-tests-baseline-files)
    (load (expand-file-name file mb-tests-elisp-directory)
          nil nil t)))

(provide 'test-manifest)

;;; test-manifest.el ends here
