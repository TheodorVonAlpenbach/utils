(require 'ada-user)
(require 'ada-group)

(defun get-all-teacher-pupils (teacher)
  (remove-duplicates (group-members (car (group-ids-from-user teacher)))))
;;(get-all-teacher-pupils (user-from-name "Claerer_no456326499_1%"))

(defun teacher-pupil-p (teacher pupil)
  "Return nil iff PUPIL is not in any of TEACHER's groups"
  (find (id pupil) (get-all-teacher-pupils teacher)))
;;(teacher-pupil-p (car (user "336dd2be-94e8-4f95-b184-adf18d58326f")) (car (user "46929e3b-5150-4a95-b012-17177c49a858")))
https://portal.skolen-test.cdu.no/api/local/file/stream/15940/4a62957a-787c-453b-9bee-9601043bf587/v2.json