



(defvar *ops* nil "A list of available operators.")
(defvar *applied-ops* nil)


(defun gps (state goals &optional (ops *ops*))
  (let ((plan (remove-if #'atom (achieve-all state goals nil ops))))
    (write-to-file *applied-ops*)
    plan))

(defun achieve-all (state goals goal-stack ops)
  (let ((current-state state))
    (if (and (every #'(lambda (goal)
                        (setf current-state
                              (achieve current-state goal goal-stack ops)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state
        nil)))

(defun achieve (state goal goal-stack ops)
  (cond
    ((member-equal goal state)
     state)
    ((member-equal goal goal-stack)
     nil)
    (t (some #'(lambda (op)
                 (when (appropriate-p goal op)
                   (apply-op state goal op goal-stack ops)))
             ops))))

(defun member-equal (item list)
    (cl:member item list :test #'equal))
      

(defun apply-op (state goal op goal-stack ops)
  (let ((new-state (achieve-all state (op-preconds op) (cons goal goal-stack) ops)))
    (if new-state
        (progn
          (format t "Applying operator: ~a~%" (op-action op))
          (push (op-action op) *applied-ops*)
          (append (remove-if #'(lambda (x)
                                 (member-equal x (op-del-list op)))
                             new-state)
                  (op-add-list op)))
        nil)))

(defun appropriate-p (goal op)
  (member-equal goal (op-add-list op)))

(defun write-to-file (plan &optional (filename "plan.txt"))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
        (dolist (step (reverse plan))
            (format stream "~a~%" step))))
