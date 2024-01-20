(in-package :aoc-2015-10)

(aoc:define-day 329356 4666278)

;; Input

(defparameter *input* (aoc:input))
(defparameter *example* "1")

;; Part 1

(defun string-to-digits-array (string)
  (let* ((n (length string))
         (array (make-array n :element-type 'integer)))
    (loop for i from 0 below n
          do (setf (aref array i) (digit-char-p (aref string i))))
    array))

(defun look-and-say (seed n)
  (let ((dp (make-array (1+ n) :element-type 'vector :initial-element (make-array 50 :element-type 'integer))))
    (setf (aref dp 1) (string-to-digits-array seed))
    (loop for i from 2 to n
          for prev = (aref dp (1- i))
          for prev-length = (length prev)
          do (setf (aref dp i)
                   (loop with curr = (make-array 50 :element-type 'integer :fill-pointer 0)
                         with count = 1
                         with j = 0
                         while (< j prev-length)
                         do (if (and (< (+ j 1) prev-length)
                                     (= (aref prev j) (aref prev (+ j 1))))
                                (incf count)
                                (progn
                                  (vector-push-extend count curr)
                                  (vector-push-extend (aref prev j) curr)
                                  (setf count 1)))
                            (incf j)
                         finally (return curr))))
    (aref dp n)))

(defun get-answer-1 (&optional (input *input*))
  (length (look-and-say input 41)))

(aoc:deftest look-and-say
  (5am:is (equal (look-and-say "1" 6) "312211")))

;; Part 2

(defun get-answer-2 (&optional (input *input*))
  (length (look-and-say input 51)))
