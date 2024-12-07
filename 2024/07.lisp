(in-package :aoc-2024-07)

(aoc:define-day 7710205485870 20928985450275)

;;; Parsing

(parseq:defrule right-hand-side ()
    (and aoc:integer-string (? " "))
  (:choose 0))

(parseq:defrule line ()
    (and aoc:integer-string ": " (* right-hand-side))
  (:choose 0 2)
  (:lambda (result numbers)
    (cons result numbers)))

;;; Input

(defparameter *equations* (aoc:input :parse-line 'line))
(defparameter *example*
  (mapcar (alexandria:curry #'parseq:parseq 'line)
          '("190: 10 19"
            "3267: 81 40 27"
            "83: 17 5"
            "156: 15 6"
            "7290: 6 8 6 15"
            "161011: 16 10 13"
            "192: 17 8 14"
            "21037: 9 7 18 13"
            "292: 11 6 16 20")))

;;; Part 1

(defun check-operators (result numbers operators)
  (labels ((evaluate (current remaining)
             (if (null remaining)
                 (= current result)
                 (let ((next (first remaining))
                       (rest (rest remaining)))
                   (some #'(lambda (operator)
                             (evaluate (funcall operator current next) rest))
                         operators)))))
    (evaluate (first numbers) (rest numbers))))

(defun process-equations (equations operators)
  (loop for equation in equations
        for (result . numbers) = equation
        when (check-operators result numbers operators)
          sum result))

(defun get-answer-1 (&optional (equations *equations*))
  (process-equations equations '(+ *)))

(aoc:given 1
  (= 3749 (get-answer-1 *example*)))

;;; Part 2

(defun concat (a b)
  (declare (type fixnum a b))
  (+ (* a (expt 10 (ceiling (log (+ b 1) 10)))) b))

(defun get-answer-2 (&optional (equations *equations*))
  (process-equations equations '(+ * concat)))

(aoc:given 2
  (= 11387 (get-answer-2 *example*)))
