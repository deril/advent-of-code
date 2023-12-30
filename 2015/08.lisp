(in-package :aoc-2015-08)

(aoc:define-day 1371 2117)

;; Input

(defparameter *list* (aoc:input))
(defparameter *examle-list* '("\"\"" "\"abc\"" "\"aaa\\\"aaa\"" "\"\\x27\""))

;; Part 1

(defun evaluate-string (string)
  (read-from-string
   (cl-ppcre:regex-replace-all "\\\\x[0-9a-fA-F]{2}" string "?")))

(defun get-answer-1 (&optional (list *list*))
  (loop for string in list
        summing (- (length string)
                   (length (evaluate-string string)))))

(aoc:given 1
  (= 12 (get-answer-1 *examle-list*)))

;; Part 2

(defun get-answer-2 (&optional (list *list*))
  (loop for string in list
        summing (- (length (prin1-to-string string))
                   (length string))))
