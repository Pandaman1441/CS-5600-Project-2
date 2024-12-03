

(defstruct op
    (action nil)
    (preconds nil)
    (add-list nil)
    (del-list nil))

;;; this makes an operator to move from one cell to another
(defun make-move-op (from-x from-y to-x to-y direction)
    (make-op
      :action (intern (string-upcase (format nil "move-~a" direction)))
      :preconds (list (intern (string-upcase (format nil "roomba-at-~a-~a" from-x from-y))))
      :add-list (list (intern (string-upcase (format nil "roomba-at-~a-~a" to-x to-y))))
      :del-list (list (intern (string-upcase (format nil "roomba-at-~a-~a" from-x from-y))))))

;;; this iterates through the grid to move in any direction from any cell
(defun generate-move-ops (grid-size)
    (let ((ops '()))
        (dotimes (x grid-size)
            (dotimes (y grid-size)
                (let ((x+1 (+ x 1))
                      (x-1 (- x 1))
                      (y+1 (+ y 1))
                      (y-1 (- y 1)))
                    (when (< x+1 grid-size)
                        (push (make-move-op x y x+1 y 'right) ops))
                    (when (>= x-1 0)
                        (push (make-move-op x y x-1 y 'left) ops))
                    (when (>= y-1 0)
                        (push (make-move-op x y x y-1 'up) ops))
                    (when (< y+1 grid-size)
                        (push (make-move-op x y x y+1 'down) ops)))))
        ops))

;;; this makes an operator to clean a cell
(defun make-clean-op (x y)
    (make-op
        :action (intern (string-upcase (format nil "clean-~a-~a" x y)))
        :preconds (list (intern (string-upcase (format nil "roomba-at-~a-~a" x y)))
                (intern (string-upcase (format nil "dirty-at-~a-~a" x y))))
        :add-list (list (intern (string-upcase (format nil "cleaned-at-~a-~a" x y))))
        :del-list (list (intern (string-upcase (format nil "dirty-at-~a-~a" x y))))))

;;; this iterates through each cell and applies make-clean-op to make each operator
(defun generate-clean-ops (grid-size)
    (let ((ops '()))
        (dotimes (x grid-size)
            (dotimes (y grid-size)
                (push (make-clean-op x y) ops)))
        ops))


;;; generates all the possible ops for moving and cleaning each cell
(defun generate-ops (grid-size)
    (append (generate-move-ops grid-size)
            (generate-clean-ops grid-size)))