(in-package :aoc-2023-01)

(aoc:define-day 55386 54824)

(defparameter *document* (aoc:input))
(defparameter *example-1*
  '("1abc2"
    "pqr3stu8vwx"
    "a1b2c3d4e5f"
    "treb7uchet"))
(defparameter *example-2*
  '("two1nine"
    "eightwothree"
    "abcone2threexyz"
    "xtwone3four"
    "4nineeightseven2"
    "zoneight234"
    "7pqrstsixteen"))

;; Part 1

(defun calibration-value (string)
  (let ((ds (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d" string))))
    (+ (* 10 (first ds)) (first (last ds)))))

(defun get-answer-1 (&optional (document *document*))
  (loop for line in document
        summing (calibration-value line)))

(aoc:given 1
  (= 142 (get-answer-1 *example-1*)))

;; Part 2

(defun parse-integer-full (string)
  (if (= (length string) 1)
      (parse-integer string)
      (alexandria:eswitch (string :test #'string=)
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
  (let ((first (ppcre:scan-to-strings (format nil "\\d|~A" +text-digit-pattern+) string))
        (last (reverse (ppcre:scan-to-strings (format nil "\\d|~A" (reverse +text-digit-pattern+)) (reverse string)))))
    (+ (* 10 (parse-integer-full first)) (parse-integer-full last))))

(defun get-answer-2 (&optional (document *document*))
  (loop for line in document
        summing (calibration-value-full line)))

(aoc:given 2
  (= 281 (get-answer-2 *example-2*)))
