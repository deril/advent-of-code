(in-package #:aoc2023-test)

(defun test-input (day)
  (asdf:system-relative-pathname :aoc2023-test (format nil "test/data/day-~a.txt" day)))
