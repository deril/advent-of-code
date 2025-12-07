(in-package :aoc-2025-06)

(aoc:define-day 4387670995909 9625320374409)

;;; Parsing

(defun split-input (input)
  (values (butlast input)
          (car (last input))))

;;; Input
(defparameter *input* (aoc:input))

(defparameter *example*
  '("123 328  51 64 "
    " 45 64  387 23 "
    "  6 98  215 314"
    "*   +   *   +"))

;;; Part 1

(defun transpose (lists)
  (apply #'mapcar #'list lists))

(defun get-answer-1 (&optional (raw-input *input*))
  (multiple-value-bind (rows ops)
      (split-input raw-input)
    (let ((rows (transpose (mapcar #'aoc:extract-ints rows)))
          (ops (cl-ppcre:split "\\s+" ops)))
      (reduce #'+
              (mapcar #'(lambda (col op) (reduce (find-symbol op) col))
                      rows
                      ops)))))

(aoc:given 1
  (= 4277556 (get-answer-1 *example*)))

;;; Part 2

(defun gutter-positions (rows)
  (iter (for pos from 0 below (length (first rows)))
    (when (every (lambda (row) (char= (char row pos) #\Space)) rows))
    (collecting pos)))

(defun split-columns (rows gutter-positions)
  (let ((gutters (append gutter-positions (list nil))))
    (iter outer (for row in rows)
      (collect
          (iter (for gutter in gutters)
            (for prev previous gutter)
            (let ((start (if (null prev)
                             0
                             (1+ prev)))
                  (end gutter))
              (collect (subseq row start end))))))))

(defun build-numbers (col)
  (iter (for i from 0 below (length (first col)))
    (collecting
      (parse-integer
       (concatenate 'string
                    (mapcar #'(lambda (num) (char num i)) col))))))

(defun get-answer-2 (&optional (raw-input *input*))
  (multiple-value-bind (rows ops)
      (split-input raw-input)
    (let* ((gutters (gutter-positions rows))
           (cols (transpose (split-columns rows gutters)))
           (operators (cl-ppcre:split "\\s+" ops)))
      (reduce #'+
              (mapcar #'(lambda (col op-str)
                          (reduce (find-symbol op-str)
                                  (build-numbers col)))
                      cols
                      operators)))))

(aoc:given 2
  (= 3263827 (get-answer-2 *example*)))
