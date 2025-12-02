(in-package :aoc-2025-01)

(aoc:define-day 1036 6228)

;;; Input

(defparameter *rotations* (aoc:input))
(defparameter *example* '("L68"
                          "L30"
                          "R48"
                          "L5"
                          "R60"
                          "L55"
                          "L1"
                          "L99"
                          "R14"
                          "L82"))

;;; Part 1

(defparameter +dial-size+ 100)
(defparameter +start-position+ 50)

(defun parse-instruction (instruction)
  "Parse instruction string into a signed delta value.
   'R48' -> 48, 'L30' -> -30"
  (let ((magnitude (parse-integer instruction :start 1)))
    (ecase (char instruction 0)
      (#\R magnitude)
      (#\L (- magnitude)))))

(defun count-zero-arrivals (pos delta)
  (declare (ignore delta))
  (if (zerop pos) 1 0))

(defun count-ticks (instructions count-fn)
  (iter (for instruction in instructions)
    (for delta = (parse-instruction instruction))
    (for pos initially +start-position+
         then (mod (+ pos delta) +dial-size+))
    (sum (funcall count-fn pos delta))))

(defun get-answer-1 (&optional (instructions *rotations*))
  (count-ticks instructions #'count-zero-arrivals))


(aoc:given 1
  (= 3 (get-answer-1 *example*)))

;;; Part 2

(defun count-boundary-crossings (pos delta)
  (let ((ticks (abs delta))
        (direction (signum delta)))
    (floor (+ (mod (+ +dial-size+ (* direction pos)) +dial-size+)
              ticks)
           +dial-size+)))

(defun get-answer-2 (&optional (instructions *rotations*))
  (count-ticks instructions #'count-boundary-crossings))

(aoc:given 2
  (= 6 (get-answer-2 *example*)))
