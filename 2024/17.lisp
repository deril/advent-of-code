(in-package :aoc-2024-17)

(aoc:define-day nil nil)

;;; Parsing

(parseq:defrule register ()
    (and "Register " char ": " (aoc:integer-string))
  (:choose 1 3)
  (:lambda (register value)
    (list (intern (string register) :keyword) value)))

(parseq:defrule program ()
    (and "Program: " (aoc:comma-list aoc:integer-string))
  (:choose 1))

;;; Input

(defun separate-input (input)
  (let ((split (position "" input :test #'string=)))
    (values (subseq input 0 split)
            (subseq input (1+ split)))))

(defun parse-input (input)
  (multiple-value-bind (registers program)
      (separate-input input)
    (list
     (mapcan #'nconc (mapcar (alexandria:curry #'parseq:parseq 'register) registers))
     (parseq:parseq 'program (first program)))))

(defparameter *input* (parse-input (aoc:input)))

(defparameter *example* (parse-input '("Register A: 729"
                                       "Register B: 0"
                                       "Register C: 0"
                                       ""
                                       "Program: 0,1,5,4,3,0")))

;;; Part 1

(defun operand-value (operand registers)
  (let ((register (case operand
                    ((0 1 2 3) operand)
                    (4 :A)
                    (5 :B)
                    (6 :C))))
    (if (keywordp register)
        (getf registers register)
        operand)))

(defun adv (operand registers pointer)
  (setf (getf registers :A)
        (truncate
         (getf registers :A)
         (expt 2 (operand-value operand registers))))
  (+ pointer 2))

(defun bxl (operand registers pointer)
  (setf (getf registers :B)
        (logxor (getf registers :B) operand))
  (+ pointer 2))

(defun bst (operand registers pointer)
  (setf (getf registers :B)
        (mod (operand-value operand registers) 8))
  (+ pointer 2))

(defun jnz (operand registers pointer)
  (if (zerop (getf registers :A))
      (+ pointer 2)
      operand))

(defun bxc (operand registers pointer)
  (declare (ignore operand))
  (setf (getf registers :B)
        (logxor (getf registers :B) (getf registers :C)))
  (+ pointer 2))

(defun out (operand registers pointer)
  (format t "~D"
          (mod (operand-value operand registers) 8))
  (+ pointer 2))

(defun bdv (operand registers pointer)
  (setf (getf registers :B)
        (truncate
         (getf registers :A)
         (expt 2 (operand-value operand registers))))
  (+ pointer 2))

(defun cdv (operand registers pointer)
  (setf (getf registers :C)
        (truncate
         (getf registers :A)
         (expt 2 (operand-value operand registers))))
  (+ pointer 2))

(defun ingest (pointer program)
  (when (and (>= pointer 0) (< pointer (length program)))
    (nth pointer program)))

(defun compute (op1 op2 registers pointer)
  (let ((command (case op1
                   (0 'adv)
                   (1 'bxl)
                   (2 'bst)
                   (3 'jnz)
                   (4 'bxc)
                   (5 'out)
                   (6 'bdv)
                   (7 'cdv))))
    (reveal `(,command ,op2 ',registers ,pointer))))

(defun reveal (command)
  (let* ((output-stream (make-string-output-stream))
         (*standard-output* output-stream))
    (list (eval command) (get-output-stream-string output-stream))))

(defun recur (program registers)
  (loop for (pointer output) = '(0 "") then (compute op1 op2 registers pointer)
        for op1 = (ingest pointer program)
        for op2 = (ingest (1+ pointer) program)
        collect output into outputs
        while op1
        finally (return (format nil "~{~A~^,~}" (remove "" outputs :test #'string=)))))

(defun get-answer-1 (&optional (input *input*))
  (recur (second input) (copy-seq (first input) )))

(aoc:given 1
  (string= "4,6,3,5,6,3,5,2,1,0" (get-answer-1 *example*)))
