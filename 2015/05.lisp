(in-package :aoc-2015-05)

(aoc:define-day 255 55)

;; Input

(defparameter *strings* (aoc:input))
(defvar *example-1* '("ugknbfddgicrmopn"
                      "aaa"
                      "jchzalrnumimnmhp"
                      "haegwjzuvuyypxyu"
                      "dvszwmarrgswjxmb"))
(defvar *example-2* '("qjhvhtzxzqqjkmpb"
                      "xxyxx"
                      "uurcxstgmygtbstg"
                      "ieodomkazucvgmuy"))

;; Part 1

(defun nice-string-p (string)
  (and (not (cl-ppcre:scan "ab|cd|pq|xy" string))
       (cl-ppcre:scan "([aeiou].*){3,}" string)
       (cl-ppcre:scan "(\\w)\\1" string)))

(defun get-answer-1 (&optional (strings *strings*))
  (count-if #'nice-string-p strings))

(aoc:given 1
  (= 2 (get-answer-1 *example-1*)))

;; Part 2

(defun nice-string-p-2 (string)
  (and (cl-ppcre:scan "(\\w\\w).*\\1" string)
       (cl-ppcre:scan "(\\w)\\w\\1" string)))

(defun get-answer-2 (&optional (strings *strings*))
  (count-if #'nice-string-p-2 strings))

(aoc:given 2
  (= 2 (get-answer-2 *example-2*)))
