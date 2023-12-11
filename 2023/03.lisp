(in-package :aoc-2023-03)

(aoc:define-day 556057 82824352)

;; Parsing

(defun parse-number-row (row)
  (labels ((parse-number (result start-pos pos)
             (cond
               ((<= (length row) pos)
                (fset:with result
                           (list (parse-integer row :start start-pos)
                                 start-pos
                                 (1- pos))))
               ((digit-char-p (schar row pos))
                (parse-number result start-pos (1+ pos)))
               (t
                (parse-non-number (fset:with result
                                             (list (parse-integer row
                                                                  :start start-pos
                                                                  :end pos)
                                                   start-pos
                                                   (1- pos)))
                                  (1+ pos)))))
           (parse-non-number (result pos)
             (cond
               ((<= (length row) pos)
                result)
               ((digit-char-p (schar row pos))
                (parse-number result pos (1+ pos)))
               (t
                (parse-non-number result (1+ pos))))))
    (parse-non-number (fset:empty-set) 0)))

(defun parse-numbers (rows)
  (labels ((parse-row (result remaining-rows index)
             (if (endp remaining-rows)
                 result
                 (parse-row (fset:with result index (parse-number-row (car remaining-rows)))
                            (cdr remaining-rows)
                            (1+ index)))))
    (parse-row (fset:empty-map) rows 0)))

(defun parse-symbol-row (row index)
  (labels ((parse-symbol (result pos)
             (cond
               ((<= (length row) pos)
                result)
               ((or (digit-char-p (schar row pos))
                    (char= #\. (schar row pos)))
                (parse-symbol result (1+ pos)))
               (t
                (parse-symbol (fset:with result
                                         (point:make-point pos index)
                                         (schar row pos))
                              (1+ pos))))))
    (parse-symbol (fset:empty-map) 0)))

(defun parse-symbols (rows)
  (labels ((parse-row (result remaining-rows index)
             (if (endp remaining-rows)
                 result
                 (parse-row (fset:map-union (parse-symbol-row (car remaining-rows) index)
                                            result)
                            (cdr remaining-rows)
                            (1+ index)))))
    (parse-row (fset:empty-map) rows 0)))

;; Input

(defparameter *numbers* (parse-numbers (aoc:input)))
(defparameter *symbols* (parse-symbols (aoc:input)))
(defparameter *example*
  '("467..114.."
    "...*......"
    "..35..633."
    "......#..."
    "617*......"
    ".....+.58."
    "..592....."
    "......755."
    "...$.*...."
    ".664.598.."))
(defparameter *example-numbers* (parse-numbers *example*))
(defparameter *example-symbols* (parse-symbols *example*))

;; Part 1

(defun group-row-numbers-for-symbol (row symbol-coords)
  (let ((symbol-x (point:x symbol-coords)))
    (fset:reduce (lambda (result number-tuple)
                   (destructuring-bind (number left right) number-tuple
                     (if (and (<= left (1+ symbol-x))
                              (<= (1- symbol-x) right))
                         (fset:with result number)
                         result)))
                 row
                 :initial-value (fset:empty-bag))))

(defun group-numbers-for-symbol (numbers symbol-coords)
  (let ((row-y (point:y symbol-coords)))
    (fset:reduce (lambda (result dy)
                   (fset:bag-sum (group-row-numbers-for-symbol (fset:lookup numbers (+ dy row-y))
                                                               symbol-coords)
                                 result))
                 '(-1 0 1)
                 :initial-value (fset:empty-bag))))

(defun group-numbers-by-symbol (numbers symbols)
  (fset:reduce (lambda (result coords symbol)
                 (cons (list symbol (group-numbers-for-symbol numbers coords))
                       result))
               symbols
               :initial-value nil))

(defun get-answer-1 (&optional (numbers *numbers*) (symbols *symbols*))
  (fset:reduce #'+
               (fset:reduce #'fset:bag-sum
                            (mapcar #'second (group-numbers-by-symbol numbers symbols)))))

(aoc:given 1
  (= 4361 (get-answer-1 *example-numbers* *example-symbols*)))

;; Part 2

(defun tuple-gear-p (tuple)
  (destructuring-bind (symbol numbers) tuple
    (and (char= symbol #\*)
         (= 2 (fset:size numbers)))))

(defun get-answer-2 (&optional (numbers *numbers*) (symbols *symbols*))
  (fset:reduce #'+
               (fset:image (lambda (tuple)
                             (fset:reduce #'* (second tuple)))
                           (fset:filter #'tuple-gear-p (group-numbers-by-symbol numbers symbols)))))

(aoc:given 2
  (= 467835 (get-answer-2 *example-numbers* *example-symbols*)))
