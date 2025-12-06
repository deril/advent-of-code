(in-package :aoc-2025-05)

(aoc:define-day 698 352807801032167)

;; Parsing

(parseq:defrule range ()
    (and aoc:integer-string "-" aoc:integer-string)
  (:choose 0 2)
  (:lambda (start end) (cons start end)))

(defun split-input (input)
  (let ((split (position "" input :test #'string=)))
    (values (subseq input 0 split)
            (subseq input (1+ split)))))

;; Input
(defparameter *input*
  (multiple-value-bind (ranges ids)
      (split-input (aoc:input))
    (defparameter *ranges* (mapcar (alexandria:curry #'parseq:parseq 'range) ranges))
    (defparameter *ids* (mapcar (alexandria:curry #'parseq:parseq 'aoc:integer-string) ids))))
(defparameter *example*
  (multiple-value-bind (ranges ids)
      (split-input
       '("3-5"
         "10-14"
         "16-20"
         "12-18"
         ""
         "1"
         "5"
         "8"
         "11"
         "17"))
    (defparameter *example-ranges* (mapcar (alexandria:curry #'parseq:parseq 'range) ranges))
    (defparameter *example-ids* (mapcar (alexandria:curry #'parseq:parseq 'aoc:integer-string) ids))))

;;; Part 1

(defun id-in-range-p (id range)
  (destructuring-bind (start . end) range
    (and (>= id start) (<= id end))))

(defun count-ids-in-ranges (ids ranges)
  (loop for id in ids
        count (some (lambda (range) (id-in-range-p id range)) ranges)))

(defun get-answer-1 (&optional (ids *ids*) (ranges *ranges*))
  (count-ids-in-ranges ids ranges))

(aoc:given 1
  (= 3 (get-answer-1 *example-ids* *example-ranges*)))

;;; Part 2

(defun merge-ranges (ranges)
  (let ((sorted (sort (copy-list ranges) #'< :key #'car)))
    (loop with merged = (list (car sorted))
          for range in (cdr sorted)
          for (start . end) = range
          for (last-start . last-end) = (car merged)
          if (<= start (1+ last-end))
            do (setf merged (cons (cons last-start (max last-end end)) (cdr merged)))
          else
            do (setf merged (cons range merged))
          finally (return merged))))

(defun get-answer-2 (&optional (ranges *ranges*))
  (let ((merged (merge-ranges ranges)))
    (reduce #'+ (mapcar (lambda (range)
                          (1+ (- (cdr range) (car range))))
                        merged))))

(aoc:given 2
  (= 14 (get-answer-2 *example-ranges*)))
