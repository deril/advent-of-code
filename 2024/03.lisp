(in-package :aoc-2024-03)

(aoc:define-day 183788984 62098619)

;;; Parsing

(parseq:defrule instruction ()
    (and "mul(" aoc:integer-string "," aoc:integer-string ")")
  (:choose 1 3))

(defun extract-all-mul-patterns (input &key (ignore-dos t))
  (let ((enabled t)
        (instrs nil))
    (dolist (match (cl-ppcre:all-matches-as-strings "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)" input))
      (cond ((string= match "do()")
             (setf enabled t))
            ((string= match "don't()")
             (setf enabled nil))
            ((or enabled ignore-dos)
             (push (parseq:parseq 'instruction match) instrs))))
    instrs))

;;; Input

(defparameter *instructions*
  (reduce #'(lambda (str line) (concatenate 'string str line))
          (aoc:input)
          :initial-value ""))
(defparameter *example*
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

;;; Part 1

(defun execute-mul (mul)
  (destructuring-bind (a b) mul
    (* a b)))

(defun get-answer-1 (&optional (instructions *instructions*))
  (reduce #'+ (mapcar #'execute-mul
                      (extract-all-mul-patterns instructions :ignore-dos t))))

(aoc:given 1
  (= 161 (get-answer-1 *example*)))


;;; Part 2

(defun get-answer-2 (&optional (instructions *instructions*))
  (reduce #'+ (mapcar #'execute-mul
                      (extract-all-mul-patterns instructions :ignore-dos nil))))

(aoc:given 2
  (= 48 (get-answer-2 *example*)))
