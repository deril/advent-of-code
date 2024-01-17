(in-package :aoc-2015-09)

(aoc:define-day 207 804)

;; Parsing

(parseq:defrule city ()
  (and (+ alpha))
  (:string)
  (:function #'string-upcase)
  (:lambda (name)
    (intern name :keyword)))

(parseq:defrule distance ()
  (and city " to " city " = " (aoc:integer-string))
  (:choose 0 2 4)
  (:lambda (from to distance)
    (cons (list from to) distance)))

(defun add-reversed-distances (distances)
  (let ((reversed-distances nil))
    (dolist (distance distances)
      (let ((reversed-distance (cons (reverse (car distance)) (cdr distance))))
        (push reversed-distance reversed-distances)))
    (append distances reversed-distances)))

;; Input

(defparameter *distances*
  (add-reversed-distances
   (aoc:input :parse-line 'distance)))
(defparameter *example-distances*
  (add-reversed-distances
   (mapcar (alexandria:curry #'parseq:parseq 'distance)
           '("London to Dublin = 464"
             "London to Belfast = 518"
             "Dublin to Belfast = 141"))))

;; Part 1

(defun distance-between (cities distances)
  (let ((distance (assoc cities distances :test #'equal)))
    (if distance
        (cdr distance)
        (error "No distance between cities"))))

(defun route-distance (route distances)
  (loop for city1 in route
        for city2 in (cdr route)
        sum (distance-between (list city1 city2) distances)))

(defun cities (distances)
  (remove-duplicates
   (alexandria:flatten
    (mapcar #'car distances))))

(defun calculate-distance-values (distances)
  (let ((distance-values nil))
    (permutation:visit-permutations
     (lambda (route)
       (push (route-distance route distances) distance-values))
     (cities distances))
    distance-values))

(defun get-answer-1 (&optional (distances *distances*))
  (apply #'min (calculate-distance-values distances)))

(aoc:given 1
  (= 605 (get-answer-1 *example-distances*)))

;; Part 2

(defun get-answer-2 (&optional (distances *distances*))
  (apply #'max (calculate-distance-values distances)))

(aoc:given 2
  (= 982 (get-answer-2 *example-distances*)))
