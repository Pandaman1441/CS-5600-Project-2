



(defun gps (state goals ops &optional (plan '()))
    (if (subsetp goals state)
        (append plan (list 'goal-achieved state))
        (some #'(lambda (op)
                (when (preconditions-met? (op-preconds op) state)
                    (trace-step op state)
                    (let* ((new-state (apply-op op state))
                            (new-plan (append plan (list (op-action op)))))
                        (gps new-state goals ops))))
            ops)))

;;; check if all preconds are met
(defun preconditions-met? (preconds state)
    (format t "checking preconds ~a against state ~a~%" preconds state)
    (every #'(lambda (p) (member p state)) preconds))

;;; Apply an operator to change state
(defun apply-op (op state)
    (format t "applying op ~a to state ~a~%" (op-action op) state)
    (let ((new-state (set-difference state (op-del-list op))))
        (append new-state (op-add-list op))))

;;; tracking operator steps
(defun trace-step (op state)
    (format t "Applying ~a to ~a~%" (op-action op) state))

;;; write plan to a file
(defun write-to-file (plan &optional (filename "plan.txt"))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
        (dolist (step plan)
            (format stream "~a~%" step))))