(in-package :aoc-2025-07)

(aoc:define-day 1553 15811946526915)

;;; Input

(defparameter *diagram* (aoc:input))

(defparameter *example*
  '(".......S......."
    "..............."
    ".......^......."
    "..............."
    "......^.^......"
    "..............."
    ".....^.^.^....."
    "..............."
    "....^.^...^...."
    "..............."
    "...^.^...^.^..."
    "..............."
    "..^...^.....^.."
    "..............."
    ".^.^.^.^.^...^."
    "..............."))

;;; Part 1

(defun simulate-beams (diagram &key count-splits)
  (let ((beams (make-array (length (first diagram)) :initial-element 0))
        (split-count 0))
    (setf (svref beams (position #\S (first diagram))) 1)
    (iter (for line in diagram)
      (iter
        (for char in-string line with-index i)
        (when (and (char= char #\^)
                   (plusp (aref beams i)))
          (let ((current-beams (aref beams i)))
            (incf split-count)
            (incf (aref beams (1- i)) current-beams)
            (incf (aref beams (1+ i)) current-beams))
          (setf (aref beams i) 0))))
    (if count-splits
        split-count
        (reduce #'+ beams))))

(defun get-answer-1 (&optional (diagram *diagram*))
  (simulate-beams diagram :count-splits t))

(aoc:given 1
  (= 21 (get-answer-1 *example*)))

;;; Part 2

(defun get-answer-2 (&optional (diagram *diagram*))
  (simulate-beams diagram :count-splits nil))

(aoc:given 2
  (= 40 (get-answer-2 *example*)))
