(in-package :aoc-2023-02)

(aoc:define-day 2204 71036)

;; Parsing

(parseq:defrule game ()
    (and "Game " aoc:integer-string ": " game-rounds)
  (:choose 1 3)
  (:lambda (id rounds) (fset:map (id rounds))))

(parseq:defrule game-rounds ()
    (and game-round (* (and "; " game-round)))
  (:lambda (first rest) (cons first (mapcar #'second rest))))

(parseq:defrule game-round ()
    (aoc:comma-list color-quantity)
  (:lambda (&rest bags) (reduce #'fset:bag-sum bags)))

(parseq:defrule color-quantity ()
    (and aoc:integer-string " " color)
  (:choose 0 2)
  (:lambda (quantity color) (fset:with (fset:empty-bag) color quantity)))

(parseq:defrule color ()
    (or "red" "green" "blue")
  (:string)
  (:function #'string-upcase)
  (:function #'intern))

(defun combine-games (games)
  (reduce (alexandria:rcurry #'fset:map-union (lambda (a b) (assert (not (and a b)) (a b) "Map key collision")))
          games))

;; Input

(defparameter *games* (combine-games (aoc:input :parse-line 'game)))
(defparameter *example*
  (combine-games
   (mapcar (alexandria:curry #'parseq:parseq 'game)
           '("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
             "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
             "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
             "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
             "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))))

;; Part 1

(defparameter +part-1-bag-conjecture+ (fset:bag (fset:% 'red 12) (fset:% 'green 13) (fset:% 'blue 14)))

(defun game-possible-p (contents game)
  (dolist (game-round game)
    (unless (fset:subbag? game-round contents)
      (return-from game-possible-p nil)))
  t)

(defun possible-games (contents games)
  (fset:filter (lambda (id rounds)
                 (declare (ignore id))
                 (game-possible-p contents rounds))
               games))

(defun get-answer-1 (&optional (games *games*))
  (fset:reduce #'+ (fset:domain (possible-games +part-1-bag-conjecture+ games))))

(aoc:given 1
  (= 8 (get-answer-1 *example*)))

;; Part 2

(defun game-minimum-contents (rounds)
  (reduce #'fset:union rounds))

(defun minimum-contents (games)
  (fset:image (lambda (id rounds) (values id (game-minimum-contents rounds)))
              games))

(defun game-power (contents)
  (assert (fset:subbag? (fset:bag 'red 'green 'blue) contents))
  (fset:reduce #'*
               (fset:image (alexandria:curry #'fset:multiplicity contents)
                           (fset:convert 'fset:bag (fset:convert 'fset:set contents)))))

(defun calculate-game-powers (games)
  (fset:image (lambda (id contents) (values id (game-power contents)))
              (minimum-contents games)))

(defun get-answer-2 (&optional (games *games*))
  (fset:reduce (lambda (sum id power)
                 (declare (ignore id))
                 (+ sum power))
               (calculate-game-powers games)
               :initial-value 0))

(aoc:given 2
  (= 2286 (get-answer-2 *example*)))
