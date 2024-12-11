(in-package :aoc-2024-01)

(aoc:define-day 1879048 21024792)

;;; Parsing

(parseq:defrule line ()
    (and aoc:integer-string (+ " ") aoc:integer-string)
  (:choose 0 2))

(defun transpose (lists)
  (apply #'mapcar #'list lists))

;;; Input

(defparameter *lists* (transpose (aoc:input :parse-line 'line)))
(defparameter *example*
  (transpose
   (mapcar (alexandria:curry #'parseq:parseq 'line)
           '("3   4"
             "4   3"
             "2   5"
             "1   3"
             "3   9"
             "3   3"))))

;;; Part 1

(defun get-answer-1 (&optional (lists *lists*))
  (let ((seq1 (sort (copy-list (first lists)) #'<))
        (seq2 (sort (copy-list (second lists)) #'<)))
    (gmap:gmap
     (:result :sum)
     #'(lambda (x y) (abs (- x y)))
     (:arg list seq1)
     (:arg list seq2))))

(aoc:given 1
  (= 11 (get-answer-1 *example*)))

;;; Part 2

(defun list-to-bag (list)
  (fset:convert 'fset:bag list))

(defun get-answer-2 (&optional (lists *lists*))
  (let ((seq1 (first lists))
        (seq2 (list-to-bag (second lists))))
    (gmap:gmap
     (:result :sum)
     #'(lambda (x) (* x (fset:multiplicity seq2 x)))
     (:arg list seq1))))

(aoc:given 2
  (= 31 (get-answer-2 *example*)))
