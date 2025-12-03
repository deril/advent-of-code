(in-package :aoc-2025-03)

(aoc:define-day 17281 171388730430281)

;;; Input

(defparameter *banks* (aoc:input))
(defparameter *example-banks*
  '("987654321111111"
    "811111111111119"
    "234234234234278"
    "818181911112111"))

;;; Part 1

(defun max-jultage (bank n)
  (let ((batteries-to-drop (- (length bank) n))
        (stack nil))
    (loop for battery across bank do
      ;; remove smaller batteries from stack while we can
      (loop while (and stack
                       (char< (car stack) battery)
                       (plusp batteries-to-drop))
            do
               (pop stack)
               (decf batteries-to-drop))
      (push battery stack))
    ;; if we still need to remove baterries, remove from end
    (loop while (plusp batteries-to-drop) do
      (pop stack)
      (decf batteries-to-drop))
    (let ((result-list (nreverse stack)))
      (parse-integer (coerce result-list 'string)))))

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
