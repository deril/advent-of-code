(in-package :aoc-2015-04)

(aoc:define-day 282749 9962624)

;; Input

(defparameter *secret-key* (aoc:input))

;; Part 1

(defun md5 (string)
  (format nil "~(~{~2,'0X~}~)"
          (map 'list #'identity (md5:md5sum-string string))))

(defun mine-hash (algorithm secret-key)
  (flet ((hash-probe (n)
           (format nil "~A~A" secret-key n)))
    (do ((i 1 (1+ i))
         (hash (md5 (hash-probe 1)) (md5 (hash-probe i))))
        ((funcall algorithm hash) (1- i)))))

(defun get-answer-1 (&optional (secret-key *secret-key*))
  (let ((difficalty-function (lambda (hash) (string= (subseq hash 0 5) "00000"))))
    (mine-hash difficalty-function secret-key)))

(aoc:given 1
  (= 609043 (get-answer-1 "abcdef"))
  (= 1048970 (get-answer-1 "pqrstuv")))

;; Part 2

(defun get-answer-2 (&optional (secret-key *secret-key*))
  (let ((difficalty-function (lambda (hash) (string= (subseq hash 0 6) "000000"))))
    (mine-hash difficalty-function secret-key)))
