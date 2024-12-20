(in-package :aoc-2024-20)

(aoc:define-day 1530 1033983)

;; Input

(defparameter *racetrack* (aoc:parse-grid-to-array (aoc:input)))

;; Part 1

(defparameter +cardinals+ (list #C(0 1) #C(1 0) #C(0 -1) #C(-1 0)))

(defun find-position (grid char)
  (iter outer
    (for i from 0 below (array-dimension grid 0))
    (iter
      (for j from 0 below (array-dimension grid 1))
      (in outer (finding (complex j i)
                         such-that (char= (aref grid i j) char))))))

(defun possible-moves (state grid)
  (iter
    (for direction in +cardinals+)
    (for new-state = (+ state direction))
    (when (member (aoc:cref grid new-state) '(#\E #\.))
      (collect (list 1 new-state)))))

(defun path (racetrack)
  (let ((start (find-position racetrack #\S))
        (end (find-position racetrack #\E)))
    (aoc:shortest-path start #'(lambda (state) (possible-moves state racetrack))
                       :end end)))

(defun distance-with-cheat (path cheat-time)
  (loop for i from 0 below (length path)
        summing (loop for j from (+ i 102) below (length path)
                      for dist = (aoc:manhattan-distance (svref path i)
                                                         (svref path j))
                      count (and (>= (- j i dist) 100) (<= dist cheat-time)))))

(defun get-answer-1 (&optional (racetrack *racetrack*))
  (let* ((path (path racetrack))
         (path-array (make-array (length path)
                                 :initial-contents path
                                 :element-type 'complex)))
    (distance-with-cheat path-array 2)))

;; Part 2

(defun get-answer-2 (&optional (racetrack *racetrack*))
  (let* ((path (path racetrack))
         (path-array (make-array (length path)
                                 :initial-contents path
                                 :element-type 'complex)))
    (distance-with-cheat path-array 20)))
