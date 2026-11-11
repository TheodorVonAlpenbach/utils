;;; quiz.el --- Quiz editor entry point -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;; Load the maintained quiz editor, formatting, navigation, persistence,
;; import, summary, cleanup, and mail-composition features.

;;; Code:

(require 'quiz-format)
(require 'quiz-navigation)
(require 'quiz-mode)
(require 'quiz-config)
(require 'quiz-edit)
(require 'quiz-layout)
(require 'quiz-cleanup)
(require 'quiz-csv)
(require 'quiz-summary)
(require 'quiz-mail)
(require 'quiz-import)

(provide 'quiz)

;;; quiz.el ends here
