



(defun gps (state goals operators)
    (if (subsetp goals state)
        (list 'goal-achieved state)
        (some #'(lambda (op)
                (when (preconditions-met? (operator-preconds op) state)
                    (let ((new-state(apply-operator op state)))
                        (gps new-state goals operators))))
            operators)))

(defun preconditions-met? (preconds state)
    (every #'(lambda (p) (member p state)) preconds))

(defun apply-operator (operator state)
    (let ((new-state (set-difference state (operator-del-list operator))))
        (append new-state (operator-add-list operator))))

(defun trace-step (operator state)
    (format t "Applying ~a to ~a~%" (operator-action operator) state))

(defun write-to-file (plan filename)
    (with-open-file (stream filename :direction :output :if-exists :supersede)
        (dolist (step plan)
            (format stream "~a~%" step))))