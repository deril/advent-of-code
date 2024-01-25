(in-package :aoc-2015-15)

(aoc:define-day 18965440 nil)

;; Parsing

(parseq:defrule ingredient ()
    (and (and (+ alpha)) ": capacity " (aoc:integer-string) ", durability " (aoc:integer-string) ", flavor " (aoc:integer-string) ", texture " (aoc:integer-string) ", calories " (aoc:integer-string))
  (:choose 2 4 6 8 10))

;; Input

(defun ingredients-to-matrix (ingredients)
  (make-array (list (length ingredients) 5) :initial-contents ingredients))

(defparameter *ingredients*
  (ingredients-to-matrix
   (aoc:input :parse-line 'ingredient)))

(defparameter *example-ingredients*
  (ingredients-to-matrix
   (mapcar (alexandria:curry #'parseq:parseq 'ingredient)
           '("Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"
             "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"))))

;; Part 1

(defvar *teaspoons* 100)

(defun score (amounts ingredients)
  (destructuring-bind (rows cols) (array-dimensions ingredients)
    (destructuring-bind (capacity durability flavor texture calories)
        (loop for j below cols
              collect (max 0
                           (loop for i below rows
                                 sum (* (aref amounts i) (aref ingredients i j)))))
      (* capacity durability flavor texture))))

(defun proportions-generator (n)
  (let ((result (make-array n :initial-contents (append (make-list (1- n) :initial-element 0) (list 0)))))
    (labels ((overflow ()
               (let ((index (position-if (lambda (x) (> x 100)) result)))
                 (when index
                   (setf (aref result index) 0)
                   (decf index)
                   (incf (aref result index))
                   (overflow))))
             (next-sequence ()
               (let ((index (- n 2)))
                 (incf (aref result index))
                 (setf (aref result (1+ index))
                       (- *teaspoons* (reduce #'+ (subseq result 0 (1+ index)))))
                 (when (> (aref result index) 100)
                   (progn
                     (overflow)
                     (next-sequence))
                   )))
             (update-result ()
               (next-sequence)
               (cond ((= (aref result 0) 100) nil) ;; no more combinations
                     ((= (reduce #'+ result) 100) (copy-seq result))
                     (t (update-result)))))
      (lambda ()
        (update-result)))))

(defun best-score (ingredients)
  (let ((generator (proportions-generator (array-dimension ingredients 0))))
    (loop for amounts = (funcall generator)
          while amounts
          for best = (score amounts ingredients)
          maximize best)))

(defun get-answer-1 (&optional (ingredients *ingredients*))
  (best-score ingredients))

(aoc:given 1
  (= 62842880 (get-answer-1 *example-ingredients*)))
