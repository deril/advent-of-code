(in-package :aoc-2024-05)

(aoc:define-day 5509 4407)

;;; Parsing

(parseq:defrule page-rule ()
    (and aoc:integer-string "|" aoc:integer-string)
  (:choose 0 2))

(parseq:defrule update ()
    (aoc:comma-list aoc:integer-string))

;;; Input

(defun separate-input (input)
  (let ((split (position "" input :test #'string=)))
    (values (subseq input 0 split)
            (subseq input (1+ split)))))

(defparameter *input*
  (multiple-value-bind (rules updates)
      (separate-input (aoc:input))
    (defparameter *rules* (mapcar (alexandria:curry #'parseq:parseq 'page-rule) rules))
    (defparameter *updates* (mapcar (alexandria:curry #'parseq:parseq 'update) updates))))

(defparameter *example*
  (multiple-value-bind (rules updates)
      (separate-input '("47|53"
                        "97|13"
                        "97|61"
                        "97|47"
                        "75|29"
                        "61|13"
                        "75|53"
                        "29|13"
                        "97|29"
                        "53|29"
                        "61|53"
                        "97|53"
                        "61|29"
                        "47|13"
                        "75|47"
                        "97|75"
                        "47|61"
                        "75|61"
                        "47|29"
                        "75|13"
                        "53|13"
                        ""
                        "75,47,61,53,29"
                        "97,61,53,29,13"
                        "75,29,13"
                        "75,97,47,61,53"
                        "61,13,29"
                        "97,13,75,29,47"))
    (defparameter *example-rules* (mapcar (alexandria:curry #'parseq:parseq 'page-rule) rules))
    (defparameter *example-updates* (mapcar (alexandria:curry #'parseq:parseq 'update) updates))))

;;; Part 1

(defun add-edges (graph rules)
  (loop for rule in rules
        do (destructuring-bind (a b) rule
             (if (and (find a (digraph:vertices graph))
                      (find b (digraph:vertices graph)))
                 (digraph:insert-edge graph a b)))))

(defun index-map (ls)
  (let ((index-map (make-hash-table)))
    (loop for el in ls
          for i from (length ls) downto 1
          do (setf (gethash el index-map) i))
    index-map))

(defun valid-update-p (index-map update)
  (let ((indices (mapcar (lambda (x) (gethash x index-map)) update)))
    (every #'identity (mapcar #'< indices (rest indices)))))

(defun middle-element (list)
  (nth (floor (length list) 2) list))

(defun produce-graph (update rules)
  (let ((graph (digraph:make-digraph
                :initial-vertices
                update)))
    (add-edges graph rules)
    graph))

(defun valid-updates (rules updates)
  (loop for update in updates
        for graph = (produce-graph update rules)
        for index-map = (index-map (digraph:topological-sort graph))
        when (valid-update-p index-map update)
          collect update))

(defun get-answer-1 (&optional (rules *rules*) (updates *updates*))
  (reduce #'+ (mapcar #'middle-element (valid-updates rules updates))))

(aoc:given 1
  (= 143 (get-answer-1 *example-rules* *example-updates*)))

;;; Part 2

(defun sort-update (update index-map)
  (sort (copy-seq update) #'< :key (lambda (el) (gethash el index-map))))

(defun invalid-updates (rules updates)
  (loop for update in updates
        for graph = (produce-graph update rules)
        for index-map = (index-map (digraph:topological-sort graph))
        unless (valid-update-p index-map update)
          collect (sort-update update index-map)))

(defun get-answer-2 (&optional (rules *rules*) (updates *updates*))
  (reduce #'+ (mapcar #'middle-element (invalid-updates rules updates))))

(aoc:given 2
  (= 123 (get-answer-2 *example-rules* *example-updates*)))
