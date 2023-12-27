(in-package :aoc-2015-06)

(aoc:define-day 543903 14687245)

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

(defvar *light-grid*)

(defun part-1-command-interpretation (command x y)
  (ecase command
    ((TURN-ON) (setf (aref *light-grid* x y) 1))
    ((TURN-OFF) (setf (aref *light-grid* x y) 0))
    ((TOGGLE) (setf (aref *light-grid* x y) (logxor (aref *light-grid* x y) 1)))))

(defun follow-instruction (command begin end interpretation)
  (loop for x from (point:x begin) to (point:x end)
        do (loop for y from (point:y begin) to (point:y end)
                 do (funcall interpretation command x y))))

(defun follow-instructions (instructions interpretation)
  (dolist (instruction instructions)
    (let ((command (fset:@ instruction 0))
          (begin (fset:@ instruction 1))
          (end (fset:@ instruction 2)))
      (follow-instruction command begin end interpretation))))

(defun get-answer-1 (&optional (instructions *instructions*))
  (setf *light-grid* (make-array '(1000 1000) :element-type 'bit :initial-element 0))
  (follow-instructions instructions #'part-1-command-interpretation)
  (count 1 (loop for x from 0 to 999
                 append (loop for y from 0 to 999
                              collect (aref *light-grid* x y)))))

(aoc:given 1
  (= 998996 (get-answer-1 *example*)))

;; Part 2

(defun part-2-command-interpretation (command x y)
  (ecase command
    ((TURN-ON) (incf (aref *light-grid* x y)))
    ((TURN-OFF) (decf (aref *light-grid* x y) (min 1 (aref *light-grid* x y))))
    ((TOGGLE) (incf (aref *light-grid* x y) 2))))

(defun get-answer-2 (&optional (instructions *instructions*))
  (setf *light-grid* (make-array '(1000 1000) :element-type 'integer :initial-element 0))
  (follow-instructions instructions #'part-2-command-interpretation)
  (loop for x from 0 to 999
        summing (loop for y from 0 to 999
                      summing (aref *light-grid* x y))))

(aoc:given 2
  (= 1001996 (get-answer-2 *example*)))
