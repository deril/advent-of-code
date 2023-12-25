(in-package :aoc-2015-03)

(aoc:define-day 2081 2341)

;; Input

(defparameter *instructions* (aoc:input))

;; Part 1

(defun move (acc direction)
  (destructuring-bind (curr-pos . visited-houses) acc
    (let ((new-pos (+ curr-pos (case direction
                                 (#\^ #C(0 1))
                                 (#\> #C(1 0))
                                 (#\v #C(0 -1))
                                 (#\< #C(-1 0))
                                 (otherwise (error "Unknown direction: ~a" direction))))))
      (cons new-pos (fset:adjoinf visited-houses new-pos)))))

(defun visited-houses (instructions)
  (cdr (fset:reduce #'move instructions :initial-value (cons 0 (fset:set 0)))))

(defun get-answer-1 (&optional (instructions *instructions*))
  (fset:size (visited-houses instructions)))

(aoc:given 1
  (= 2 (get-answer-1 ">"))
  (= 4 (get-answer-1 "^>v<"))
  (= 2 (get-answer-1 "^v^v^v^v^v")))


;; Part 2

(defun split-instructions (instructions)
  (loop for i from 1
        for instruction across instructions
        if (evenp i)
          collect instruction into robot-instructions
        else
          collect instruction into santa-instructions
        finally (return (values santa-instructions robot-instructions))))

(defun get-answer-2 (&optional (instructions *instructions*))
  (multiple-value-bind (santa-instructions robot-instructions)
      (split-instructions instructions)
    (let ((santa-houses (visited-houses santa-instructions))
          (robot-houses (visited-houses robot-instructions)))
      (fset:size (fset:union santa-houses robot-houses)))))

(aoc:given 2
  (= 3 (get-answer-2 "^v"))
  (= 3 (get-answer-2 "^>v<"))
  (= 11 (get-answer-2 "^v^v^v^v^v")))
