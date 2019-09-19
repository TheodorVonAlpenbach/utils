;;;; This module is not used by any other. It is meant just for
;;;; experimenting and scraching stuff

(require 'cram-config)
(require 'cram-backend)
(require 'cram-common)

(cram-draw-problem :method :worst)
(length (ld-select :problem))
(concat* (ld-select :problem :column :question) :in "\n")

(provide 'cram-scratch)
