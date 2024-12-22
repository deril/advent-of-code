(in-package :aoc-2024-22)

(aoc:define-day nil nil)

;; Input

(defparameter *secrets* (mapcar #'parse-integer (aoc:input)))

(defparameter *example-secrets* (mapcar #'parse-integer '("1" "10" "100" "2024")))

;; Part 1

(defun mix-and-prune (a b)
  (declare (type (integer 0 #x7FFFFFFFFFFFFFFF) a b))
  (logand (logxor a b) #xFFFFFF))


(defun generate (secret n)
  (declare (type (integer 0 #xFFFFFF) secret)
           (type (integer 1) n))
  (iter
    (for value initially secret then value3)
    (repeat n)
    (for value1 = (mix-and-prune (ash value 6) value))
    (for value2 = (mix-and-prune (ash value1 -5) value1))
    (for value3 = (mix-and-prune (ash value2 11) value2))
    (collect value3)))

(defun get-answer-1 (&optional (secrets *secrets*))
  (declare (type list secrets))
  (gmap:gmap
   (:result :sum)
   #'(lambda (secret) (alexandria:lastcar (generate secret 2000)))
   (:arg list secrets)))

(aoc:given 1
  (= 37327623 (get-answer-1 *example-secrets*)))

;; Part 2

(defun compute-diffs (secrets)
  (loop for (a b) on secrets while b
        collect (- (mod b 10) (mod a 10))))

(defun mex-from-list (secrets)
  (iter
    (with seen = (fset:empty-set))
    (with deltas = (compute-diffs secrets))
    (for i from 1 to (- (length deltas) 4))
    (let ((key (subseq deltas i (+ i 4))))
      (unless (fset:contains? seen key)
        (fset:includef seen key)
        (incf (gethash key bananas 0) (aref )))
      )
    )
  )

(defun get-answer-2 (&optional (secrets *example-secrets*))
  (gmap:gmap
   (:result :max)
   #'(lambda (secret) (max-from-list (generate secret 2000)))
   (:arg list secrets)))
