(defpackage #:aoc2023-01
  (:use #:cl #:utils)
  (:import-from #:cl-ppcre #:scan-to-strings)
  (:export
   ;; #:parse-input
   #:part-a
   #:part-b))

(in-package #:aoc2023-01)


(defun parse-input (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defvar *input* (parse-input (input-file-path 1)))

(defvar *matching-regex* "(\d)")

(defun part-a (input)
  (let ((matches (loop for line in input
                       for match = (cl-ppcre:scan-to-strings *matching-regex* line)
                       when match
                         collect (parse-integer (first match)))))
    (reduce #'+ matches))
  )
