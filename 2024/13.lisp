(in-package :aoc-2024-13)

(aoc:define-day 31897 87596249540359)

;;; Input

(defun separate-input (input)
  (let ((split (position "" input :test #'string=)))
    (if split
        (cons (subseq input 0 split)
              (separate-input (subseq input (1+ split))))
        (cons input nil))))

(defparameter *machines*
  (mapcar #'(lambda (machine)
              (mapcar #'aoc:extract-ints machine))
          (separate-input (aoc:input))))
(defparameter *example-machines*
  (mapcar #'(lambda (machine)
              (mapcar #'aoc:extract-ints machine))
          (separate-input
           '("Button A: X+94, Y+34"
             "Button B: X+22, Y+67"
             "Prize: X=8400, Y=5400"
             ""
             "Button A: X+26, Y+66"
             "Button B: X+67, Y+21"
             "Prize: X=12748, Y=12176"
             ""
             "Button A: X+17, Y+86"
             "Button B: X+84, Y+37"
             "Prize: X=7870, Y=6450"
             ""
             "Button A: X+69, Y+23"
             "Button B: X+27, Y+71"
             "Prize: X=18641, Y=10279"))))

;;; Part 1

(defun find-coins-amount (a1 b1 a2 b2 c1 c2)
  (let* ((a (/ (- (* c1 b2) (* c2 a2))
               (- (* a1 b2) (* a2 b1))))
         (b (/ (- c1 (* a1 a)) a2)))
    (if (and (zerop (mod a 1)) (zerop (mod b 1)))
        (values a b)
        (values nil nil))))

(defun count-coins (machines &optional (conversion-error 0))
  (iter
    (for machine in machines)
    (for ((a1 b1) (a2 b2) (c1 c2)) = machine)
    (for (values a b) = (find-coins-amount a1 b1 a2 b2 (+ conversion-error c1) (+ conversion-error c2)))
    (when (and a b)
      (summing (+ (* 3 a) b)))))

(defun get-answer-1 (&optional (machines *machines*))
  (count-coins machines))

(aoc:given 1
  (= 480 (get-answer-1 *example-machines*)))

;;; Part 2

(defun get-answer-2 (&optional (machines *machines*))
  (count-coins machines 10000000000000))
