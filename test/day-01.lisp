(in-package #:aoc2023-test)

(def-suite* day-01)

(defun test-quasi ()
  (run! 'day-01))

(test parse-input
  (is (equal '("1abc2"
               "pqr3stu8vwx"
               "a1b2c3d4e5f"
               "treb7uchet")
             (aoc2023-01::parse-input (test-input 1)))))

(test part-1
  (is (= 142 (aoc2023-01::part-a (aoc2023-01::parse-input (test-input 1))))))

(test calibration-value-full
  (is (= 42 (aoc2023-01::calibration-value-full "four7seventeen3eightwo"))))

(test part-2
    (is (= 281 (aoc2023-01::part-b (aoc2023-01::parse-input (test-input 1.2))))))
