(in-package :aoc-2025-04)

(aoc:define-day 1460 9243)

;;; Parsing

(defun parse-symbol-row (row index)
  (labels ((parse-symbol (result pos)
             (cond
               ((<= (length row) pos)
                result)
               ((char= #\. (schar row pos))
                (parse-symbol result (1+ pos)))
               (t
                (parse-symbol (fset:with result
                                         (point:make-point pos index)
                                         0)
                              (1+ pos))))))
    (parse-symbol (fset:empty-map) 0)))

(defun parse-rows (rows)
  (labels ((parse-row (result remaining-rows index)
             (if (endp remaining-rows)
                 result
                 (parse-row (fset:map-union (parse-symbol-row (car remaining-rows) index)
                                            result)
                            (cdr remaining-rows)
                            (1+ index)))))
    (parse-row (fset:empty-map) rows 0)))

;;; Input

(defparameter *diagram*
  (calculate-neighbours
   (parse-rows (aoc:input))))

(defparameter *example*
  (calculate-neighbours
   (parse-rows
    '("..@@.@@@@."
      "@@@.@.@.@@"
      "@@@@@.@.@@"
      "@.@@@@..@."
      "@@.@@@@.@@"
      ".@@@@@@@.@"
      ".@.@.@.@@@"
      "@.@@@.@@@@"
      ".@@@@@@@@."
      "@.@.@@@.@."))))

;;; Part 1

(defparameter *directions* (list #<1 0> #<-1 0> #<0 1> #<0 -1>
                                 #<1 1> #<1 -1> #<-1 1> #<-1 -1>))

(defun count-neigbours (posititon diagram)
  (count-if #'(lambda (neighbor)
                (multiple-value-bind (_ foundp) (fset:@ diagram neighbor)
                  (declare (ignore _))
                  foundp))
            (mapcar (lambda (dir) (point:+ posititon dir))
                    *directions*)))

(defun calculate-neighbours (diagram)
  (fset:do-set (roll (fset:domain diagram))
    (setf (fset:@ diagram roll) (count-neigbours roll diagram)))
  diagram)

(defun optimize-worklifts (diagram)
  (let ((removed-rolls (fset:empty-set)))
    (fset:do-map (pos neighbours diagram)
      (when (< neighbours 4)
        (fset:adjoinf removed-rolls pos)
        (fset:removef diagram pos)
        (dolist (dir *directions*)
          (let ((neighbour (point:+ pos dir)))
            (multiple-value-bind (count foundp) (fset:@ diagram neighbour)
              (when foundp
                (setf (fset:@ diagram neighbour) (1- count))))))))
    (values (fset:size removed-rolls) diagram)))

(defun get-answer-1 (&optional (diagram *diagram*))
  (nth-value 0 (optimize-worklifts diagram)))

(aoc:given 1
  (= 13 (get-answer-1 *example*)))

;;; Part 2

(defun get-answer-2 (&optional (diagram *diagram*))
  (loop with current-diagram = diagram
        for (removed-count current-diagram) = (multiple-value-list (optimize-worklifts current-diagram))
        sum removed-count into total-removed
        while (> removed-count 0)
        finally (return total-removed)))

(aoc:given 2
  (= 43 (get-answer-2 *example*)))
