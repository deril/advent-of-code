(in-package :point)

(deftype point (&optional dimention)
  `(vector real ,(or dimention *)))

(defun make-point-array (length &optional initial-contents)
  (if initial-contents
      (make-array length :element-type 'real :initial-contents initial-contents)
      (make-array length :element-type 'real)))

(defun make-point (&rest coordinates)
  (declare (type (or null (cons real)) coordinates))
  (make-point-array (length coordinates) coordinates))

(declaim (inline x))
(defun x (point)
  "Alias for (elt point 0)"
  (declare (type point point))
  (elt point 0))

(declaim (inline y))
(defun y (point)
  "Alias for (elt point 1)"
  (declare (type point point))
  (elt point 1))
