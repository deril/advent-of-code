(in-package :aoc-2015-16)

(aoc:define-day 103 405)

;; Parsing

(parseq:defrule thing ()
    (and " "
         (or "children" "cats" "samoyeds" "pomeranians" "akitas"
             "vizslas" "goldfish" "trees" "cars" "perfumes")
         ": "
         (aoc:integer-string)
         (? ","))
  (:choose 1 3)
  (:lambda (thing value)
    (cons (intern (string-upcase thing)) value)))

(parseq:defrule sue ()
    (and "Sue "
         (aoc:integer-string)
         ":"
         (+ thing))
  (:choose 3))

;; Input

;;; actual aunt number is +1 as the input is 1-indexed
(defparameter *aunts* (aoc:input :parse-line 'sue))

(defparameter *target*
  '((children . 3)
    (cats . 7)
    (samoyeds . 2)
    (pomeranians . 3)
    (akitas . 0)
    (vizslas . 0)
    (goldfish . 5)
    (trees . 3)
    (cars . 2)
    (perfumes . 1)))

;; Part 1

(defun get-answer-1 ()
  (iter
    (for aunt in-sequence *aunts* with-index i)
    (for matches = (intersection *target* aunt :test #'equal))
    (finding (1+ i) maximizing (length matches))))

;; Part 2

(defun get-ansnwe-2 ()
  (iter
    (for aunt in-sequence *aunts* with-index i)
    (for matches = (loop for (thing . value) in aunt
                         collect (cond ((member thing '(cats trees))
                                        (> value (cdr (assoc thing *target*))))
                                       ((member thing '(pomeranians goldfish))
                                        (< value (cdr (assoc thing *target*))))
                                       (t
                                        (= value (cdr (assoc thing *target*)))))))
    (finding (1+ i) maximizing (length (remove nil matches)))))
