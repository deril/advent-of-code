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

(defun read-delimiter (stream char)
  (declare (ignore stream))
  (error "Delimiter ~S shouldn't be read alone" char))

(set-dispatch-macro-character
 #\# #\<
 (lambda (stream subchar arg)
   (let ((*readtable* (copy-readtable)))
     (set-macro-character #\> 'read-delimiter)
     (let ((lst (read-delimited-list #\> stream t)))
       `(make-point ,@lst)))))

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

(defun = (point &rest other-points)
  "Returns true if all points have the same dimension and coordinates;
  false otherwise."
  (declare (type point point)
           (type (or null (cons point)) other-points))
  (reduce #'equalp (cons point other-points)))

(defun map-coordinates (function points)
  "Calls FUNCTION on each successive set of coordinates from the points.
  FUNCTION must take as many parameters as there are points.  POINTS may
  be any collection of point objects understood by FSet.  Returns a point
  made from the returns values of FUNCTION."
  (declare (type (or null (cons point) fset:collection) points))
  (let* ((arb-point (if (or (typep points 'fset:set)
                            (typep points 'fset:bag))
                        (fset:arb points)
                        (fset:first points)))
         (dimension-count (length arb-point))
         (result (make-point-array dimension-count)))
    (case (fset:size points)
      (0)
      (1 (dotimes (i (length arb-point))
           (setf (cl:aref result i)
                 (funcall function (cl:aref arb-point i)))))
      (t (let ((args (make-array dimension-count :initial-element nil)))
           ;; We're using `reduce` for side effects here.  A `do-` macro
           ;; would be clearer, but those aren't polymorphic, and we want
           ;; to be able to handle any single-arity FSet collection.
           (fset:reduce (lambda (_ point)
                          (declare (ignore _))
                          (dotimes (i dimension-count)
                            (push (cl:aref point i) (cl:aref args i))))
                        points
                        :initial-value nil)
           (dotimes (i dimension-count)
             (setf (cl:aref result i)
                   (apply function (reverse (cl:aref args i))))))))
    result))

(defun + (&rest points)
  "Adds the points together, coordinatewise"
  (declare (type (or null (cons point)) points))
  (cond
    ((endp points)
     (make-point))
    ((endp (cdr points))
     (car points))
    (t
     (map-coordinates #'cl:+ points))))

(defun - (point &rest more-points)
  "Subtracts MORE-POINTS from POINT, coordinatewise.  With only one
  parameter, negates POINT."
  (declare (type point point)
           (type (or null (cons point)) more-points))
  (if (endp more-points)
      (map-coordinates #'cl:- (list point))
      (map-coordinates #'cl:- (list point (apply #'+ more-points)))))

(defun * (point scalar)
  "Multiplies each coordinate of POINT by SCALAR."
  (declare (type point point)
           (type real scalar))
  (map-coordinates (lambda (x) (cl:* x scalar)) (list point)))

(defun mod (point divisor-point)
  "Returns a point where each coordinate in POINT is modulo the
  corresponding coordinate in DIVISOR-POINT."
  (map-coordinates #'cl:mod (list point divisor-point)))

(defun compare (op &rest points)
  "Applies OP to every coordinate of each member of points."
  (labels ((compare-r (point-a more-points)
             (or (endp more-points)
                 (destructuring-bind (point-b &rest remaining-points) more-points
                   (and (iter (for a in-vector point-a)
                          (for b in-vector point-b)
                          (always (funcall op a b)))
                        (compare-r point-b remaining-points))))))
    (if (endp (cdr points))      ; Implicitly handles the "no points" case
        t
        (compare-r (car points) (cdr points)))))

(defun < (&rest points)
  "Returns true if every coordinate of each member of POINTS is less than
  each corresponding coordinate of the next point, and so on."
  (apply #'compare #'cl:< points))

(defun turn (point direction)
  "Rotates a 2D point 90 degrees.  DIRECTION should be either the symbol
  LEFT or RIGHT."
  (declare (type (point 2) point)
           (type symbol direction))
  (ecase direction
    (:right
     (make-point (cl:aref point 1) (cl:- (cl:aref point 0))))
    (:left
     (make-point (cl:- (cl:aref point 1)) (cl:aref point 0)))))

(defun bbox (points)
  "Returns two points.  The first contains the smallest values for each
  set of coordinates; the second, the largest."
  (declare (type (or null (cons point) fset:collection) points))
  (values (map-coordinates #'min points)
          (map-coordinates #'max points)))

(defun print-2d-map (map element-char-fn &key (stream t) (unknown #\ ) highlight)
  "Prints a representation of MAP to STREAM.  The keys of MAP should be
  two-coordinate points, with the first coordinate giving the
  column (increasing to the right) and the second coordinate giving the
  row (increasing *down*).

  ELEMENT-CHAR-FN should be a function of one argument.  It will be called
  for each value of MAP.  It should return a character, though anything
  that can be passed to FORMAT will work.

  UNKNOWN will be used for positions that are not present in MAP.

  HIGHLIGHT is a position that is to be marked in some way to make it stand out."
  (multiple-value-bind (min-point max-point) (bbox (fset:convert 'list (fset:domain map)))
    (declare (type (point 2) min-point max-point))
    (iter (for row from (elt min-point 1) to (elt max-point 1))
      (iter (for col from (elt min-point 0) to (elt max-point 0))
        (for position = (make-point col row))
        (for (values element foundp) = (fset:lookup map position))
        (format stream "~A" (if foundp
                                (funcall element-char-fn element)
                                unknown))
        (when (and highlight (= highlight position))
          (princ #\U20DD stream)))
      (format stream "~%")))
  (values))
