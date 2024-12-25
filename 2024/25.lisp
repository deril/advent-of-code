(in-package :aoc-2024-25)

(aoc:define-day 3065 nil)

;; Input

(defun split-input (input)
  (let ((split (position "" input :test #'string=)))
    (if (null split)
        (list input)
        (cons (subseq input 0 split)
              (split-input (subseq input (1+ split)))))))

(defparameter *example* (split-input (aoc:input :file "examples/25.txt")))
(defparameter *input* (split-input (aoc:input)))

;; Part 1

(defun key-locks (line-list)
  (loop for item in line-list
        with keys = nil
        with locks = nil
        do (let ((pins (make-array (length (first item)) :initial-element 0)))
             (loop for line in (butlast (cdr item))
                   do (loop for i from 0 below (length line)
                            for c across line
                            when (char= c #\#)
                              do (incf (aref pins i))))
             (if (char= (char (car item) 0) #\#)
                 (push pins locks)
                 (push pins keys)))
        finally (return (list keys locks))))

(defun compatible-pair-p (key-counts lock-counts)
  (every (lambda (k l) (<= (+ k l) 5)) key-counts lock-counts))

(defun count-compatible-pairs (keys locks)
  (loop for lock in locks
        sum (loop for key in keys
                  count (compatible-pair-p key lock))))

(defun get-answer-1 (&optional (input *input*))
  (destructuring-bind (keys locks) (keys-locks input)
    (count-compatible-pairs keys locks)))

(aoc:given 1
  (= 3 (get-answer-1 *example*)))
