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

(defun next-position (pos delta)
  (mod (+ pos delta) +dial-size+))

(defun count-zero-arrivals (pos next-pos delta)
  (declare (ignore pos delta))
  (if (zerop next-pos) 1 0))

(defun get-answer-1 (&optional (instructions *rotations*))
  (iter (for instruction in instructions)
    (for delta = (parse-instruction instruction))
    (for pos initially +start-position+
         then (next-position pos delta))
    (count (zerop pos))))


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
  (iter (for instruction in instructions)
    (for pos initially +start-position+
         then next-pos)
    (for delta = (parse-instruction instruction))
    (for next-pos = (next-position pos delta))
    (sum (count-boundary-crossings pos delta))))

(aoc:given 2
  (= 6 (get-answer-2 *example*)))
