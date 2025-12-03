(in-package :aoc-2025-03)

(aoc:define-day 17281 171388730430281)

;;; Parsing

(defun string-to-digits (number-string)
  (map 'simple-vector #'digit-char-p number-string))

;;; Input

(defparameter *banks*
  (mapcar #'string-to-digits
          (aoc:input)))
(defparameter *example-banks*
  (mapcar #'string-to-digits
          '("987654321111111"
            "811111111111119"
            "234234234234278"
            "818181911112111")))

;;; Part 1

(defun max-jultage (bank n)
  (let ((len (length bank))
        (dp (make-array n :initial-element 0)))
    (dotimes (i len)
      (let ((digit (svref bank i)))
        (loop for pos downfrom (1- (min (1+ i) n)) to 0 do
          (let ((candidate (if (zerop pos)
                               digit
                               (+ (* (aref dp (1- pos)) 10) digit))))
            (when (> candidate (aref dp pos))
              (setf (aref dp pos) candidate))))))
    (aref dp (1- n))))


(defun get-answer-1 (&optional (banks *banks*))
  (loop for bank in banks
        summing (max-jultage bank 2)))

(aoc:given 1
  (= 357 (get-answer-1 *example-banks*)))

;;; Part 2

(defun get-answer-2 (&optional (banks *banks*))
  (loop for bank in banks
        summing (max-jultage bank 12)))

(aoc:given 2
  (= 3121910778619 (get-answer-2 *example-banks*)))
