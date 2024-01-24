(in-package :aoc-2015-13)

(aoc:define-day 733 725)

;; Parsing

(parseq:defrule name ()
    (and (+ alpha))
  (:string)
  (:function #'string-upcase)
  (:lambda (name)
    (intern name :keyword)))

(parseq:defrule happiness ()
    (and name " would " (or "gain" "lose") " " (aoc:integer-string) " happiness units by sitting next to " name ".")
  (:choose 0 2 4 6)
  (:lambda (name1 sign value name2)
    (let ((reatl-value (if (string= sign "gain") value (- value))))
      (list name1 name2 reatl-value))))

;; Input

(defun build-matrix (tokenized-input)
  (let* ((nodes (sort (remove-duplicates (mapcar #'first tokenized-input))
                      #'string<))
         (matrix (make-array (list (length nodes) (length nodes))
                             :element-type 'integer
                             :initial-element 0)))
    (iter (for (source target adjustment) in tokenized-input)
      (setf (aref matrix (position source nodes) (position target nodes))
            adjustment))
    matrix))

(defparameter *happiness-list*
  (aoc:input :parse-line 'happiness))

(defparameter *example-happiness-list*
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

(defun happiness-change (happiness-matrix sitting)
  (iter (for source in-vector sitting with-index i)
    (for left = (svref sitting (mod (1- i) (length sitting))))
    (for right = (svref sitting (mod (1+ i) (length sitting))))
    (summing (aref happiness-matrix source left))
    (summing (aref happiness-matrix source right))))

(defun maximum-happiness (happiness-matrix)
  (let* ((number-of-people (array-dimension happiness-matrix 0))
         (max-sitting (make-array number-of-people
                                  :element-type 'integer
                                  :initial-contents (iter (for i below number-of-people) (collect i))))
         (max-happiness (happiness-change happiness-matrix max-sitting)))
    (permutation:visit-permutations
     (lambda (sitting)
       (let ((happiness (happiness-change happiness-matrix sitting)))
         (when (> happiness max-happiness)
           (setf max-happiness happiness))))
     max-sitting
     :copy nil)
    max-happiness))

(defun get-answer-1 (&optional (happiness-list *happiness-list*))
  (let ((happiness-matrix (build-matrix happiness-list)))
    (maximum-happiness happiness-matrix)))

(aoc:given 1
  (= 330 (get-answer-1 *example-happiness-list*)))

;; Part 2

(defun add-me (happiness-list)
  (build-matrix (cons '(:me :me 0) happiness-list)))

(defun get-answer-2 (&optional (happiness-list *happiness-list*))
  (let ((happiness-matrix (add-me happiness-list)))
    (maximum-happiness happiness-matrix)))
