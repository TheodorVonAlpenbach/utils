(require 'mb-utils-strings)

(defun sql-list (list)
  (concat* list :pre "(" :in "," :suf ")" :key #'sstring))
;;(sql-list '(1 2 3))

(defun sql-clean-jooq-log-1 (log)
  (string-replace "ada." "" (string-replace "`" "" log)))
;;(sql-clean-jooq-log-1 "insert into `ada`.`user_task_match` (`user_id`, `parameter_set_id`, `score`, `start_time`, `end_time`, `initial_user_rating`, `initial_user_ratings_deviation`, `new_user_rating`, `new_user_ratings_deviation`, `initial_parameter_set_rating`, `initial_parameter_set_ratings_deviation`, `new_parameter_set_rating`, `new_parameter_set_ratings_deviation`) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")

(defun sql-clean-jooq-log (log)
  (sql-clean-jooq-log-1 (string-match* "\\[\\(.*\\)\\];" log :num 1)))
;;(sql-clean-jooq-log "[insert into `ada`.`user_task_match` (`user_id`, `parameter_set_id`, `score`, `start_time`, `end_time`, `initial_user_rating`, `initial_user_ratings_deviation`, `new_user_rating`, `new_user_ratings_deviation`, `initial_parameter_set_rating`, `initial_parameter_set_ratings_deviation`, `new_parameter_set_rating`, `new_parameter_set_ratings_deviation`) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)];")

(provide 'sql-utils)
