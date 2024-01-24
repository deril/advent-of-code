(in-package :aoc-2015-13)

(aoc:define-day 733 nil)

;; Parsing

(parseq:defrule name ()
    (and (+ alpha))
  (:string)
  (:function #'string-upcase)
  (:lambda (name)
    (intern name :keyword)))

(parseq:defrule happiness-sign ()
    (or "gain" "lose"))

(parseq:defrule happiness ()
    (and name " would " happiness-sign " " (aoc:integer-string) " happiness units by sitting next to " name ".")
  (:choose 0 2 4 6)
  (:lambda (name1 sign value name2)
    (let ((reatl-value (if (string= sign "gain") value (- value))))
      (cons (list name1 name2) reatl-value))))

;; Input

(defparameter *happiness-list*
  (aoc:input :parse-line 'happiness))

(defparameter *example-happiness*
  (mapcar (alexandria:curry #'parseq:parseq 'happiness)
          '("Alice would gain 54 happiness units by sitting next to Bob."
            "Alice would lose 79 happiness units by sitting next to Carol."
            "Alice would lose 2 happiness units by sitting next to David."
            "Bob would gain 83 happiness units by sitting next to Alice."
            "Bob would lose 7 happiness units by sitting next to Carol."
            "Bob would lose 63 happiness units by sitting next to David."
            "Carol would lose 62 happiness units by sitting next to Alice."
            "Carol would gain 60 happiness units by sitting next to Bob."
            "Carol would gain 55 happiness units by sitting next to David."
            "David would gain 46 happiness units by sitting next to Alice."
            "David would lose 7 happiness units by sitting next to Bob."
            "David would gain 41 happiness units by sitting next to Carol.")))

;; Part 1

(defun happiness-between (names happiness-list)
  (let ((happiness1 (assoc names happiness-list :test #'equal))
        (happiness2 (assoc (reverse names) happiness-list :test #'equal)))
    (if (and happiness1 happiness2)
        (+ (cdr happiness1) (cdr happiness2))
        (error "No happiness between people ~A" names))))

(defun all-names (happiness-list)
  (remove-duplicates
   (alexandria:flatten
    (mapcar #'car happiness-list))))

(defun happiness-for-sitting (names happiness-list)
  (loop for name1 in names
        for name2 in (cdr names)
        sum (happiness-between (list name1 name2) happiness-list) into happiness
        finally (return (+ happiness (happiness-between (list name2 (first names)) happiness-list)))))

(defun calculate-sitting (happiness-list)
  (let ((happiness-values nil))
    (permutation:visit-permutations
     (lambda (sitting)
       (push (happiness-for-sitting sitting happiness-list) happiness-values))
     (all-names happiness-list))
    happiness-values))

(defun get-answer-1 (&optional (happiness-list *happiness-list*))
  (apply #'max (calculate-sitting happiness-list)))

(aoc:given 1
  (= 330 (get-answer-1 *example-happiness*)))
