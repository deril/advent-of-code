(in-package :aoc-2024-04)

(aoc:define-day 2358 1737)

;;; Input

(defparameter *example* (aoc:parse-grid-to-array
                         '("MMMSXXMASM"
                           "MSAMXMSMSA"
                           "AMXSXMAAMM"
                           "MSAMASMSMX"
                           "XMASAMXAMM"
                           "XXAMMXXAMA"
                           "SMSMSASXSS"
                           "SAXAMASAAA"
                           "MAMMMXMMMM"
                           "MXMXAXMASX")))

(defparameter *input* (aoc:parse-grid-to-array (aoc:input)))

;;; Part 1

(defun fetch-xmas (array idx dir)
  (let ((rows (array-dimension array 0))
        (cols (array-dimension array 1)))
    (loop for k from 0 below 4
          for i = (+ (realpart idx) (* (realpart dir) k))
          for j = (+ (imagpart idx) (* (imagpart dir) k))
          if (or (< i 0) (>= i rows) (< j 0) (>= j cols))
            return ""
          else
            collect (aref array i j) into chars
          finally (return (coerce chars 'string)))))


(defun get-answer-1 (&optional (input *input*))
  (let* ((directions '(#C(0 1) #C(0 -1) #C(1 0) #C(-1 0) #C(1 1) #C(1 -1) #C(-1 -1) #C(-1 1))))
    (iter outer
      (for i below (array-dimension input 0))
      (iter
        (for j below (array-dimension input 1))
        (for char = (aref input i j))
        (if (char= char #\X)
            (iter (for dir in directions)
              (in outer (counting (string= (fetch-xmas input (complex i j) dir) "XMAS")))))))))

(aoc:given 1
  (= 18 (get-answer-1 *example*)))

;;; Part 2

(defun fetch-x-mas (array idx)
  (let ((checks '(((#C(-1 -1) #\M) (#C(1 -1) #\M) (#C(-1 1) #\S) (#C(1 1) #\S))
                  ((#C(-1 -1) #\M) (#C(-1 1) #\M) (#C(1 -1) #\S) (#C(1 1) #\S))
                  ((#C(-1 1) #\M) (#C(1 1) #\M) (#C(-1 -1) #\S) (#C(1 -1) #\S))
                  ((#C(1 1) #\M) (#C(1 -1) #\M) (#C(-1 -1) #\S) (#C(-1 1) #\S)))))
    (some (lambda (pattern)
            (every (lambda (check)
                     (destructuring-bind (offset char) check
                       (char= (aoc:cref array (+ idx offset)) char)))
                   pattern))
          checks)))

(defun get-answer-2 (&optional (input *input*))
  (iter outer
    (for i from 1 below (1- (array-dimension input 0)))
    (iter
      (for j from 1 below (1- (array-dimension input 1)))
      (for char = (aref input i j))
      (when (char= char #\A)
        (in outer (counting (fetch-x-mas input (complex i j))))))))

(aoc:given 2
  (= 9 (get-answer-2 *example*)))
