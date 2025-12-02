(in-package :aoc-2025-02)

(aoc:define-day 19574776074 25912654282)

;;; Parsing

(parseq:defrule range ()
    (and aoc:integer-string "-" aoc:integer-string)
  (:choose 0 2))

(parseq:defrule ranges ()
    (and (aoc:comma-list range))
  (:choose 0))

;;; Input

(defparameter *product-ids*
  (aoc:input :parse-line 'ranges))

(defparameter *example*
  (parseq:parseq 'ranges
                 "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"))

;;; Part 1

(defparameter +min-repetitions+ 2)

(declaim (inline number-of-digits))
(defun number-of-digits (n)
  (declare (type (integer 1) n))
  (floor (1+ (log n 10))))

(defun fset-range (start end)
  (loop for i from start to end
        with result = (fset:empty-seq)
        do (setf result (fset:with-last result i))
        finally (return result)))

(defun broken-ids-fset (start end factor min-block max-block)
  (fset:reduce
   #'(lambda (acc block-value)
       (let ((candidate (* block-value factor)))
         (if (and (>= candidate start) (<= candidate end))
             (fset:with acc candidate)
             acc)))
   (fset-range min-block max-block)
   :initial-value (fset:empty-set)))


(defun sum-broken-ids-in-range (start end &optional (repetitions nil))
  (let* ((digit-length (number-of-digits end))
         (max-reps (or repetitions digit-length)))
    (fset:reduce
     #'+
     (fset:reduce
      (lambda (acc rep)
        (let* ((block-length (floor digit-length rep))
               (factor (/ (1- (expt 10 (* block-length rep)))
                          (1- (expt 10 block-length))))
               (min-block (max (expt 10 (1- block-length))
                               (ceiling (/ start factor))))
               (max-block (min (1- (expt 10 block-length))
                               (floor (/ end factor)))))
          (fset:union acc (broken-ids-fset start end factor min-block max-block))))
      (fset-range +min-repetitions+ max-reps)
      :initial-value (fset:empty-set))
     :initial-value 0)))

(defun get-answer-1 (&optional (product-id-ranges *product-ids*))
  (gmap:gmap
   (:result :sum)
   (lambda (range)
     (sum-broken-ids-in-range (first range) (second range) +min-repetitions+))
   (:arg list product-id-ranges)))

(aoc:given 1
  (= 1227775554 (get-answer-1 *example*)))

;;; Part 2

(defun get-answer-2 (&optional (product-id-ranges *product-ids*))
  (gmap:gmap
   (:result :sum)
   (lambda (range)
     (sum-broken-ids-in-range (first range) (second range)))
   (:arg list product-id-ranges)))

(aoc:given 2
  (= 4174379265 (get-answer-2 *example*)))
