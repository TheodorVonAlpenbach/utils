(require 'ada-mysql)

(cl-defun parameter-set-group (parameter-set-group &rest columns)
  (emacsql db (vector :select (column-selection columns)
		      :from 'parameter-set-group
		      :where 'id :in '$v1)
	   (cl-coerce (ids parameter-set-group) 'vector)))
;;(ids (parameter-set-group '(118)))

(cl-defun parameter-set-group-from-source-id (source-id &rest columns)
  (emacsql db (vector :select (column-selection columns)
		      :from 'parameter-set-group
		      :where 'source-id :in '$v1)
	   (cl-coerce (ssymbol (listify source-id)) 'vector)))
;;(parameter-set-group-from-source-id '(6abbc81829325027307a3204))

(cl-defun parameter-set-group-rating (parameter-set-group &rest columns)
  "Return a list with elements (PARAMETER-SET-ID RATING)
for all parameter sets in PARAMETER-SET-GROUP."
  (cl-loop for ps in (parameter-set-from-group parameter-set-group :id)
	   if (parameter-set-rating ps)
	   collect (list ps it)))
;;(parameter-set-group-rating 36)

(cl-defun parameter-set (parameter-set &rest columns)
  (emacsql db (vector :select (column-selection columns)
		      :from 'parameter-set
		      :where 'id :in '$v1)
	   (cl-coerce (ids parameter-set) 'vector)))
;;(ids (parameter-set '(360 361) :id))

(cl-defun parameter-set-from-group (parameter-set-group &rest columns)
  (emacsql db (vector :select (column-selection columns)
		      :from 'parameter-set
		      :where 'parameter-set-group-id :in '$v1)
	   (cl-coerce (ids parameter-set-group) 'vector)))
;;(parameter-set-from-group 102 :id)

(cl-defun delete-parameter-set-for-group-source-id (source-id &rest columns)
  (delete-parameter-set
   (parameter-set-from-group
    (parameter-set-group-from-source-id source-id :id) :id)))
;;(delete-parameter-set-for-group-source-id "6abbc81829325027307a3204")

(cl-defun delete-parameter-set (parameter-set &rest columns)
  (awhen (ids parameter-set)
    (delete-opponent (opponent-from-parameter-set it))
    (emacsql db (vector :delete :from 'parameter-set
			:where 'id :in '$v1)
	     (cl-coerce it 'vector))))
;;(delete-parameter-set 2659)

(cl-defun parameter-set-rating (parameter-set &rest columns)
  (rating-from-opponent (opponent-from-parameter-set parameter-set)))
;;(parameter-set-rating 166)

(cl-defun opponent (opponent &rest columns)
  (emacsql db (vector :select (column-selection columns)
		      :from 'opponent
		      :where 'id :in '$v1)
	   (cl-coerce (ids opponent) 'vector)))
;;(opponent 11)

(cl-defun opponent-from-parameter-set (parameter-set &rest columns)
  (awhen (ids parameter-set)
    (emacsql db (vector :select (column-selection columns)
			:from 'opponent
			:where 'parameter-set-id :in '$v1)
	     (cl-coerce it 'vector))))
;;(opponent-from-parameter-set 620)

(cl-defun delete-opponent (opponent)
  "Delete OPPONENT.
OPPONENT is either a singleton opponent or a list of opponents"
  (when opponent
    (delete-opponent-rating opponent)
    (emacsql db (vector :delete :from 'opponent
			:where 'id :in '$v1)
	     (cl-coerce (ids opponent) 'vector))))

(cl-defun rating-from-opponent (opponent &rest columns)
  (awhen (ids opponent)
    (emacsql db (vector :select (column-selection columns)
			:from 'opponent-rating
			:where 'opponent-id :in '$v1)
	     (cl-coerce it 'vector))))
;;(rating-from-opponent 1)
;;(mapcar #'id atps)

(cl-defun delete-opponent-rating (opponent)
  "Delete the rating of OPPONENT
OPPONENT is either a singleton opponent or a list of opponents"
  (emacsql db (vector :delete :from 'opponent-rating
		      :where 'opponent-id :in '$v1)
	   (cl-coerce (ids opponent) 'vector)))
;;(delete-parameter-set-ratings atps)

(cl-defun rating-category (rating-category &rest columns)
  (aif (ids rating-category)
    (emacsql db (vector :select (column-selection columns)
			:from 'rating-category
			:where 'id :in '$v1)
	     (cl-coerce it 'vector))))
;;(rating-category 3)

(provide 'ada-parameter-set)
