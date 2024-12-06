(in-package :aoc-2024-06)

(aoc:define-day 5095 1933)

;;; Parsing

(defun parse-guard (rows)
  (iter outer
    (for row in rows)
    (for index below (length rows))
    (iter
      (for pos below (length row))
      (in outer (finding (point:make-point pos index) such-that (char= #\^ (schar row pos)))))))

(defun parse-symbol-row (row index)
  (labels ((parse-symbol (result pos)
             (cond
               ((<= (length row) pos)
                result)
               ((or (char= #\^ (schar row pos))
                    (char= #\. (schar row pos)))
                (parse-symbol result (1+ pos)))
               (t
                (parse-symbol (fset:with result
                                         (point:make-point pos index))
                              (1+ pos))))))
    (parse-symbol (fset:empty-set) 0)))

(defun parse-rows (rows)
  (labels ((parse-row (result remaining-rows index)
             (if (endp remaining-rows)
                 result
                 (parse-row (fset:union (parse-symbol-row (car remaining-rows) index)
                                        result)
                            (cdr remaining-rows)
                            (1+ index)))))
    (parse-row (fset:empty-set) rows 0)))

;;; Input

(defparameter *input* (aoc:input))
(defparameter *obstructions* (parse-rows *input*))
(defparameter *guard-args*
  (list :position (parse-guard *input*)
        :direction (point:make-point 0 -1)
        :limit (point:make-point (length (first *input*)) (length *input*))))

(defparameter *example*
  '("....#....."
    ".........#"
    ".........."
    "..#......."
    ".......#.."
    ".........."
    ".#..^....."
    "........#."
    "#........."
    "......#..."))
(defparameter *example-obstructions* (parse-rows *example*))
(defparameter *example-guard-args*
  (list :position (parse-guard *example*)
        :direction (point:make-point 0 -1)
        :limit (point:make-point (length (first *example*)) (length *example*))))

(defstruct guard position direction limit)

;;; Part 1

(defun next-point (guard)
  (point:+ (guard-position guard) (guard-direction guard)))

(defun next-position (guard obstructions)
  (let ((candidate-position (next-point guard)))
    (cond
      ((fset:contains? obstructions candidate-position) :obstruction)
      ((not (point:< #<-1 -1> candidate-position (guard-limit guard))) :out-of-bounds)
      (t candidate-position))))

(defun turn (guard)
  (setf (guard-direction guard) (point:turn (guard-direction guard) :left)))

(defun move (guard)
  (setf (guard-position guard) (point:+ (guard-position guard) (guard-direction guard))))

(defun all-paths (obstructions guard)
  (let ((visited-paths (fset:set (guard-position guard))))
    (loop
      for position = (next-position guard obstructions)
      do (case position
           (:obstruction
            (turn guard))
           (:out-of-bounds
            (return visited-paths))
           (otherwise
            (move guard)
            (setf visited-paths (fset:with visited-paths position)))))
    visited-paths))

(defun get-answer-1 (&optional (guard-args *guard-args*) (obstructions *obstructions*))
  (let ((guard (apply #'make-guard guard-args)))
    (fset:size (all-paths obstructions guard))))

(aoc:given 1
  (= 41 (get-answer-1 *example-guard-args* *example-obstructions*)))

;;; Part 2

(defun stuck-in-loop-p (obstructions guard)
  (let ((visited-obstructions (fset:empty-set)))
    (loop
      for position = (next-position guard obstructions)
      do (case position
           (:obstruction
            (let* ((next-point (next-point guard))
                   (log-key (cons next-point (guard-direction guard))))
              (if (fset:contains? visited-obstructions log-key)
                  (return t)
                  (progn
                    (setf visited-obstructions (fset:with visited-obstructions log-key))
                    (turn guard)))))
           (:out-of-bounds
            (return nil))
           (otherwise
            (move guard))))))

(defun additional-obstructions (obstructions guard-args)
  (let* ((guard (build-guard guard-args))
         (all-paths (fset:convert 'list (all-paths obstructions guard))))
    (loop
      for candidate-obstruction in all-paths
      unless (fset:contains? obstructions candidate-obstruction)
        count (stuck-in-loop-p (fset:with obstructions candidate-obstruction) (build-guard guard-args)))))

(defun build-guard (guard-args)
  (apply #'make-guard guard-args))

(defun get-answer-2 (&optional (guard-args *guard-args*) (obstructions *obstructions*))
  (additional-obstructions obstructions guard-args))

(aoc:given 2
  (= 6 (get-answer-2 *example-guard-args* *example-obstructions*)))
