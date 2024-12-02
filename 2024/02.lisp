(in-package :aoc-2024-02)

(aoc:define-day 591 621)

;;; Input

(defun ints-to-seq (ints)
  (mapcar #'aoc:extract-ints ints))

(defparameter *reports* (ints-to-seq (aoc:input)))
(defparameter *example*
  (ints-to-seq
   '("7 6 4 2 1"
     "1 2 7 8 9"
     "9 7 6 2 1"
     "1 3 2 4 5"
     "8 6 4 4 1"
     "1 3 6 7 9")))

;;; Part 1

(defun diff-within-limits (level next-level)
  (let ((diff (abs (- next-level level))))
    (and (>= diff 1) (<= diff 3))))

(defun find-direction (level next-level)
  (signum (- next-level level)))

(defun report-safe-p (report)
  (let ((initial-direction (find-direction (first report) (second report))))
    (iter
      (for (level n-level) on report)
      (while n-level)
      (for i upfrom 0)
      (if (not (and (= initial-direction (find-direction level n-level))
                    (diff-within-limits level n-level)))
          (return (values nil i)))
      (finally (return (values t nil))))))

(defun get-answer-1 (&optional (reports *reports*))
  (iter
    (for report in reports)
    (counting (report-safe-p report))))

(aoc:given 1
  (= 2 (get-answer-1 *example*)))

;;; Part 2

(defun remove-nth-element (list index)
  (nconc (subseq list 0 index) (nthcdr (1+ index) list)))

(defun without-each (list)
  (iter
    (repeat (length list))
    (for i upfrom 0)
    (collect (remove-nth-element list i))))

(defun get-answer-2 (&optional (reports *reports*))
  (iter
    (for report in reports)
    (counting
     (some #'report-safe-p (without-each report)))))

(aoc:given 2
  (= 4 (get-answer-2 *example*)))
