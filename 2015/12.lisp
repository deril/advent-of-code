(in-package :aoc-2015-12)

(aoc:define-day 111754 65402)

;; Input

(defparameter *document* (com.inuoe.jzon:parse (aoc:input)))

;; Part 1

(defvar *ignored-values*)

(defun sum-numbers (obj &optional (sum 0))
  (typecase obj
    (number     (+ sum obj))
    (list       (do ((temp obj (cdr temp))
                     (result sum))
                    ((null temp) result)
                  (setq result (sum-numbers (car temp) result))))
    (hash-table (do ((temp (alexandria:hash-table-values obj) (cdr temp))
                     (result sum))
                    ((null temp) result)
                  (if (member (car temp) *ignored-values* :test #'equal)
                      (return-from sum-numbers sum)
                      (setq result (sum-numbers (car temp) result)))))
    (vector     (do ((i 0 (+ i 1))
                     (result sum))
                    ((= i (length obj)) result)
                  (setq result (sum-numbers (aref obj i) result))))
    (otherwise  sum)))

(aoc:deftest given-1-sum
  (5am:is (= 0 (sum-numbers (com.inuoe.jzon:parse "[]"))))
  (5am:is (= 0 (sum-numbers (com.inuoe.jzon:parse "{}"))))
  (5am:is (= 0 (sum-numbers (com.inuoe.jzon:parse "{\"a\":[-1,1]}"))))
  (5am:is (= 0 (sum-numbers (com.inuoe.jzon:parse "[-1,{\"a\":1}]"))))
  (5am:is (= 3 (sum-numbers (com.inuoe.jzon:parse "[[[3]]]"))))
  (5am:is (= 3 (sum-numbers (com.inuoe.jzon:parse "{\"a\":{\"b\":4},\"c\":-1}"))))
  (5am:is (= 6 (sum-numbers (com.inuoe.jzon:parse "[1,2,3]"))))
  (5am:is (= 6 (sum-numbers (com.inuoe.jzon:parse "{\"a\":2,\"b\":4}")))))

(defun get-answer-1 (&optional (document *document*))
  (let ((*ignored-values* nil))
    (sum-numbers document)))

;; Part 2

(aoc:deftest given-2-sum
  (5am:is (= 4 (sum-numbers (com.inuoe.jzon:parse "[1,{\"c\":\"red\",\"b\":2},3]"))))
  (5am:is (= 0 (sum-numbers (com.inuoe.jzon:parse "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}"))))
  (5am:is (= 6 (sum-numbers (com.inuoe.jzon:parse "[1,\"red\",5]")))))

(defun get-answer-2 (&optional (document *document*))
  (let ((*ignored-values* '("red")))
    (sum-numbers document)))
