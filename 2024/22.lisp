(in-package :aoc-2024-22)

(aoc:define-day 17724064040 1998)

;; Input

(defparameter *secrets* (mapcar #'parse-integer (aoc:input)))

(defparameter *example-secrets* (mapcar #'parse-integer '("1" "10" "100" "2024")))
(defparameter *example-secrets-2* (mapcar #'parse-integer '("1" "2" "3" "2024")))

;; Part 1

(declaim (inline mix-and-prune))
(defun mix-and-prune (a b)
  (declare (type (unsigned-byte 48) a b))
  (logand (logxor a b) #xFFFFFF))


(defun generate (secret n)
  (declare (type (unsigned-byte 24) secret)
           (type (integer 1) n))
  (let ((result (make-array (1+ n) :element-type '(unsigned-byte 24))))
    (setf (aref result 0) secret)
    (loop for i from 1 to n
          for value = (aref result (1- i))
          for value1 = (mix-and-prune (ash value 6) value)
          for value2 = (mix-and-prune (ash value1 -5) value1)
          for value3 = (mix-and-prune (ash value2 11) value2)
          do (setf (aref result i) value3))
    result))

(defun get-answer-1 (&optional (secrets *secrets*))
  (declare (type list secrets))
  (gmap:gmap
   (:result :sum)
   #'(lambda (secret) (aref (generate secret 2000) 2000))
   (:arg list secrets)))

(aoc:given 1
  (= 37327623 (get-answer-1 *example-secrets*)))

;; Part 2

(defun compute-diffs (secrets)
  (coerce
   (loop for i from 0 below (- (length secrets) 1)
         collect (- (mod (aref secrets (1+ i)) 10) (mod (aref secrets i) 10)))
   'vector))

(defun max-from-list (secrets bananas)
  (let ((deltas (compute-diffs secrets))
        (seen (make-hash-table)))
    (loop for i from 0 below (- (length secrets) 4)
          for key = (+ (* (+ (aref deltas (+ i 0)) 10) (expt 20 3))
                       (* (+ (aref deltas (+ i 1)) 10) (expt 20 2))
                       (* (+ (aref deltas (+ i 2)) 10) 20)
                       (+ (aref deltas (+ i 3)) 10))
          unless (gethash key seen)
            do (setf (gethash key seen) t)
               (incf (gethash key bananas 0) (mod (aref secrets (+ i 4)) 10)))))

(defun get-answer-2 (&optional (secrets *secrets*))
  (let ((bananas (make-hash-table)))
    (dolist (secret secrets)
      (max-from-list (generate secret 2000) bananas))
    (loop for value being the hash-values of bananas maximize value)))

(aoc:given 2
  (= 23 (get-answer-2 *example-secrets-2*)))
