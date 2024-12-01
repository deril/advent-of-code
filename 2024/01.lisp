(in-package :aoc-2024-01)

(aoc:define-day nil nil)

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

(defun sum-over-two-seqs (seq1 seq2 func)
  (iter
    (for x in-sequence seq1)
    (for y in-sequence seq2)
    (sum (funcall func x y))))

(defun get-answer-1 (&optional (lists *lists*))
  (let ((seq1 (sort (copy-list (first lists)) #'<))
        (seq2 (sort (copy-list (second lists)) #'<)))
    (sum-over-two-seqs seq1 seq2 #'(lambda (x y) (abs (- x y))))))

(aoc:given 1
  (= 11 (get-answer-1 *example*)))

;;; Part 2

(defun list-to-bag (list)
  (fset:convert 'fset:bag list))

(defun get-answer-2 (&optional (lists *lists*))
  (let ((seq1 (first lists))
        (seq2 (list-to-bag (second lists))))
    (iter (for x in-sequence seq1)
      (sum (* x (fset:multiplicity seq2 x))))))

(aoc:given 2
  (= 31 (get-answer-2 *example*)))
