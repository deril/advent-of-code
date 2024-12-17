(in-package :aoc-2024-17)

(aoc:define-day "4,1,7,6,4,1,0,2,7" 164279024971453) ;; TODO Part 2 solution is very tailored to the input

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
                    (6 :C)
                    (otherwise (error "Invalid operand")))))
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
  (let* ((output-stream (make-string-output-stream))
         (*standard-output* output-stream)
         (command (vector 'adv 'bxl 'bst 'jnz 'bxc 'out 'bdv 'cdv)))
    (list
     (funcall (aref command op1) op2 registers pointer)
     (get-output-stream-string output-stream))))

(defun recur (program registers)
  (loop for (pointer output) = '(0 "") then (compute op1 op2 registers pointer)
        for op1 = (ingest pointer program)
        for op2 = (ingest (1+ pointer) program)
        collect output into outputs
        while op1
        finally (return (format nil "~{~A~^,~}" (remove "" outputs :test #'string=)))))

(defun get-answer-1 (&optional (input *input*))
  (destructuring-bind (registers program) input
    (recur program (copy-seq registers))))

(aoc:given 1
  (string= "4,6,3,5,6,3,5,2,1,0" (get-answer-1 *example*)))

;;; Part 2

(defun run-with-register-a (registers program a)
  (let ((registers (copy-seq registers)))
    (setf (getf registers :A) a)
    (recur program registers)))

(defun find-self-copy (registers program)
  (let ((candidates (fset:seq 0))
        (min-candidate (expt 2 (* 3 (1- (length program))))))
    (loop while (and (fset:nonempty? candidates)
                     (< (fset:last candidates) min-candidate))
          for seed = (fset:pop-first candidates)
          do (loop for a from 0 below (expt 2 6)
                   do (let* ((a (+ (ash seed 6) a))
                             (result (aoc:extract-ints (run-with-register-a registers program a))))
                        (when (< a 8)
                          (setq result (cons 0 '(result))))
                        (when (equal result (subseq program (- (length program) (length result))))
                          (setq candidates (fset:with-last candidates a)))
                        (when (equal program result)
                          (return-from find-self-copy (fset:last candidates))))))))

(defun get-answer-2 (&optional (input *input*))
  (destructuring-bind (registers program) input
    (find-self-copy registers program)))
