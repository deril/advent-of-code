(defpackage #:aoc2023-01
  (:use #:cl #:utils)
  (:import-from #:cl-ppcre
                #:all-matches-as-strings
                #:scan-to-strings)
  (:import-from #:alexandria
                #:eswitch)
  (:export
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

;; Part 2

(defun parse-integer-full (string)
  (if (= (length string) 1)
      (parse-integer string)
      (eswitch (string :test #'string=)
        ("one" 1)
        ("two" 2)
        ("three" 3)
        ("four" 4)
        ("five" 5)
        ("six" 6)
        ("seven" 7)
        ("eight" 8)
        ("nine" 9))))

(defparameter +text-digit-pattern+ "one|two|three|four|five|six|seven|eight|nine")

(defun calibration-value-full (string)
  (let ((first (scan-to-strings (format nil "\\d|~A" +text-digit-pattern+) string))
        (last (reverse (scan-to-strings (format nil "\\d|~A" (reverse +text-digit-pattern+)) (reverse string)))))
    (+ (* 10 (parse-integer-full first)) (parse-integer-full last))))


(defun part-b (&optional (document *document*))
  (reduce #'+ (mapcar #'calibration-value-full document)))
