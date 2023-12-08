(defpackage #:aoc2023-01
  (:use #:cl #:utils)
  (:import-from #:cl-ppcre #:all-matches-as-strings)
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

(defparameter *document* (parse-input (input-file-path 1)))

(defun calibration-value (string)
  (let ((ds (mapcar #'parse-integer (all-matches-as-strings "\\d" string))))
    (+ (* 10 (first ds)) (first (last ds)))))

(defun part-a (&optional (document *document*))
    (reduce #'+ (mapcar #'calibration-value document)))
