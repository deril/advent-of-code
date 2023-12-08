(in-package #:aoc2023-test)

(def-suite* day-01-all)

(defun test-quasi ()
  (run! 'day-01-all))

(test parse-input
  (is (equal '("1abc2"
               "pqr3stu8vwx"
               "a1b2c3d4e5f"
               "treb7uchet")
             (aoc2023-01::parse-input (test-input 1)))))

(test part-1
  (is (= 142 (aoc2023-01::part-a (aoc2023-01::parse-input (test-input 1))))))
;; (test part-2
;;       (is (= 241861950 (day-1:part-b (day-01:parse-input (test-input "day-01"))))))
