(in-package :aoc-2024-11)

(aoc:define-day 203953 242090118578155)

;;; Input

(defparameter *stones* (aoc:extract-ints (aoc:input)))
(defparameter *example-stones* (aoc:extract-ints "125 17"))

;;; Part 1

(defun number-of-digits (n)
  (floor (1+ (log n 10))))
(declaim (inline number-of-digits))

(defmemo:defmemo tally (depth stone)
  (if (zerop depth)
      1
      (cond ((zerop stone) (tally (1- depth) 1))
            ((evenp (number-of-digits stone))
             (multiple-value-bind (left right) (floor stone (expt 10 (/ (number-of-digits stone) 2)))
               (+ (tally (1- depth) left) (tally (1- depth) right))))
            (t (tally (1- depth) (* 2024 stone))))))

(defun count-stones (depth stones)
  (gmap:gmap (:result :sum)
             #'(lambda (stone) (tally depth stone))
             (:arg list stones)))

(defun get-answer-1 (&optional (stones *stones*))
  (count-stones 25 stones))

(aoc:given 1
  (= 55312 (get-answer-1 *example-stones*)))

;;; Part 2

(defun get-answer-2 (&optional (stones *stones*))
  (count-stones 75 stones))
