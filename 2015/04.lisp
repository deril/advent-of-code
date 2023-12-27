(in-package :aoc-2015-04)

;; Part 2 takes about 5 seconds to run on my machine.
(aoc:define-day 282749 9962624)

;; Input

(defparameter *secret-key* (aoc:input))

;; Part 1

(defun mine-hash (algorithm secret-key)
  (flet ((hash-probe (n)
           (format nil "~A~A" secret-key n)))
    (do ((i 1 (1+ i))
         (hash (md5:md5sum-string (hash-probe 1)) (md5:md5sum-string (hash-probe i))))
        ((funcall algorithm hash) (1- i)))))

(defun get-answer-1 (&optional (secret-key *secret-key*))
  (let ((difficalty-function (lambda (hash)
                               (and (= (aref hash 0) 0)
                                    (= (aref hash 1) 0)
                                    (= (ash (aref hash 2) -4) 0)))))
    (mine-hash difficalty-function secret-key)))

(aoc:given 1
  (= 609043 (get-answer-1 "abcdef"))
  (= 1048970 (get-answer-1 "pqrstuv")))

;; Part 2

(defun get-answer-2 (&optional (secret-key *secret-key*))
  (let ((difficalty-function (lambda (hash)
                               (and (= (aref hash 0) 0)
                                    (= (aref hash 1) 0)
                                    (= (aref hash 2) 0)))))
    (mine-hash difficalty-function secret-key)))
