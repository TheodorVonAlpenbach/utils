(require 'ada-mysql)

(defun user-selectable-languages (&rest columns)
  "Return a list of user selectable languages"
  (emacsql db
    (vector :select (column-selection columns)
	    :from 'language
	    :where '(= user-selectable 1)
	    :order-by 'priority)))
;;(user-selectable-languages :source-id :name :priority)

(defun language-from-name (name &rest columns)
  "Return a list of languages matching NAME"
  (emacsql db
    (vector :select (column-selection columns)
	    :from 'language
	    :where '(like name $r1))
    name))
;;(language-from-name "%laerer%99_5%" :id)


(provide 'ada-language)
