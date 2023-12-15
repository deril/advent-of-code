(in-package :aoc-2015-01)

(aoc:define-day 232 1783)

;; Input

(defvar *input* (aoc:input))

;; Part 1

(defun movement-direction (character)
  (case character
    (#\( 1)
    (#\) -1)
    (otherwise (error "Unknown direction"))))

(defun find-floor (paren-string)
  (loop for paren across paren-string
        sum (movement-direction paren)))

(defun get-answer-1 (&optional (instructions *input*))
  (find-floor instructions))

(aoc:given 1
  (= 0 (get-answer-1 "(())"))
  (= 0 (get-answer-1 "()()"))
  (= 3 (get-answer-1 "((("))
  (= 3 (get-answer-1 "(()(()("))
  (= 3 (get-answer-1 "))((((("))
  (= -1 (get-answer-1 "())"))
  (= -1 (get-answer-1 "))("))
  (= -3 (get-answer-1 ")))"))
  (= -3 (get-answer-1 ")())())")))

;; Part 2

(defun find-basement (paren-string)
  (loop for paren across paren-string
        for position from 1
        summing (movement-direction paren) into floor
        when (minusp floor)
          return position))

(defun get-answer-2 (&optional (instructions *input*))
  (find-basement instructions))

(aoc:given 2
  (= 1 (get-answer-2 ")"))
  (= 5 (get-answer-2 "()())")))
