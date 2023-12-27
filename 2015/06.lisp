(in-package :aoc-2015-06)

(aoc:define-day 543903 nil)

;; Parsing

(parseq:defrule instruction ()
  (and command
       " "
       (aoc:comma-list aoc:integer-string)
       " through "
       (aoc:comma-list aoc:integer-string))
  (:choose 0 2 4)
  (:lambda (command begin end)
    (fset:seq command (apply #'point:make-point begin) (apply #'point:make-point end))))


(parseq:defrule command ()
  (or "turn on" "turn off" "toggle")
  (:string)
  (:lambda (c) (substitute #\- #\Space c))
  (:function #'string-upcase)
  (:function #'intern))

;; Input

(defparameter *instructions* (aoc:input :parse-line 'instruction))
(defparameter *example*
  (mapcar (alexandria:curry #'parseq:parseq 'instruction)
          '("turn on 0,0 through 999,999"
            "toggle 0,0 through 999,0"
            "turn off 499,499 through 500,500")))

;; Part 1

(defparameter *part-1-grid*)

(defun follow-instruction (instruction)
  (let ((command (fset:@ instruction 0))
        (begin (fset:@ instruction 1))
        (end (fset:@ instruction 2)))
    (loop for x from (point:x begin) to (point:x end)
          do (loop for y from (point:y begin) to (point:y end)
                   do (ecase command
                        ((TURN-ON) (setf (aref *part-1-grid* x y) 1))
                        ((TURN-OFF) (setf (aref *part-1-grid* x y) 0))
                        ((TOGGLE) (setf (aref *part-1-grid* x y) (logxor (aref *part-1-grid* x y) 1))))))))

(defun follow-instructions (instructions)
  (dolist (instruction instructions)
    (follow-instruction instruction)))

(defun get-answer-1 (&optional (instructions *instructions*))
  (setf *part-1-grid* (make-array '(1000 1000) :initial-element 0))
  (follow-instructions instructions)
  (count 1 (loop for x from 0 to 999
                 append (loop for y from 0 to 999
                              collect (aref *part-1-grid* x y)))))

(aoc:given 1
  (= 500 (get-answer-1 *example*)))
