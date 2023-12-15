(in-package :aoc-2015-01)

(aoc:define-day 232 nil)

;; Input

(defvar *input* (aoc:input))

;; Part 1

(defun count-floor (paren-string)
  (loop for paren across paren-string
        sum (if (char= paren #\() 1 -1)))

(defun get-answer-1 (&optional (instructions *input*))
  (count-floor instructions))

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
