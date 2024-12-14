(in-package :aoc-2024-14)

(aoc:define-day 231852216 8159)

;;; Parsing

(parseq:defrule position ()
    (and "p=" (aoc:comma-list aoc:integer-string))
  (:choose 1)
  (:lambda (x y)
    (point:make-point x y)))

(parseq:defrule velocity ()
    (and "v=" (aoc:comma-list aoc:integer-string))
  (:choose 1)
  (:lambda (x y)
    (point:make-point x y)))

(parseq:defrule robot ()
    (and position " " velocity)
  (:choose 0 2))

;;; Input

(defun parse-robots (robots)
  (reduce #'(lambda (result robot)
              (fset:with-last result (parseq:parseq 'robot robot)))
          robots
          :initial-value (fset:empty-seq)))

(defparameter *robots* (parse-robots (aoc:input)))
(defparameter *dimensions* #<101 103>)

(defparameter *example-robots*
  (parse-robots
   '("p=0,4 v=3,-3"
     "p=6,3 v=-1,-3"
     "p=10,3 v=-1,2"
     "p=2,0 v=2,-1"
     "p=0,0 v=1,3"
     "p=3,0 v=-2,-2"
     "p=7,6 v=-1,-3"
     "p=3,0 v=-1,-2"
     "p=9,3 v=2,3"
     "p=7,3 v=-1,2"
     "p=2,4 v=2,-3"
     "p=9,5 v=-3,-3")))
(defparameter *example-dimensions* #<11 7>)

;;; Part 1

(defun robot-position-after-time (position velocity seconds dimensions)
  (point:mod (point:+ position (point:* velocity seconds)) dimensions))

(defun predicted-robot-positions (robots seconds dimensions)
  (fset:reduce #'(lambda (result robot)
                   (destructuring-bind (position velocity) robot
                     (fset:with result (robot-position-after-time position velocity seconds dimensions))))
               robots
               :initial-value (fset:empty-bag)))

(defun grid-quadrants (dimensions)
  (let ((x0 (1- (point:x dimensions)))
        (y0 (1- (point:y dimensions))))
    (list
     (list 0 0 (1- (/ x0 2)) (1- (/ y0 2)))
     (list (1+ (/ x0 2)) 0 x0 (1- (/ y0 2)))
     (list 0 (1+ (/ y0 2)) (1- (/ x0 2)) y0)
     (list (1+ (/ x0 2)) (1+ (/ y0 2)) x0 y0))))

(defun robots-in-quadrant (positions dimensions)
  (iter outer
    (with (x0 y0 x1 y1) = dimensions)
    (for x from x0 to x1)
    (iter
      (for y from y0 to y1)
      (in outer
          (sum (fset:multiplicity positions #<x y>))))))

(defun count-robots (robots seconds dimensions)
  (iter
    (with positions = (predicted-robot-positions robots seconds dimensions))
    (with quadrants = (grid-quadrants dimensions))
    (for quadrant in quadrants)
    (terpri)
    (multiply (robots-in-quadrant positions quadrant))))

(defun get-answer-1 (&optional (robots *robots*) (dimensions *dimensions*))
  (let ((seconds 100))
    (count-robots robots seconds dimensions)))

(aoc:given 1
  (= 12 (get-answer-1 *example-robots* *example-dimensions*)))

;;; Part 2

(defun every-robot-has-place-p (robots seconds dimensions)
  (let ((new-robots (fset:empty-set)))
    (fset:do-seq (robot robots :value new-robots)
      (destructuring-bind (position velocity) robot
        (let ((new-position (robot-position-after-time position velocity seconds dimensions)))
          (if (fset:contains? new-robots new-position)
              (return nil)
              (fset:includef new-robots new-position)))))))

(defun get-answer-2 (&optional (robots *robots*) (dimensions *dimensions*))
  (iter
    (for seconds from 0)
    (when (every-robot-has-place-p robots seconds dimensions)
      (return seconds))))
