(in-package :aoc-2024-16)

(aoc:define-day 72428 nil)

;;; Input

(defparameter *input*
  (aoc:parse-grid-to-array (aoc:input)))

(defparameter *example1*
  (aoc:parse-grid-to-array
   '("###############"
     "#.......#....E#"
     "#.#.###.#.###.#"
     "#.....#.#...#.#"
     "#.###.#####.#.#"
     "#.#.#.......#.#"
     "#.#.#####.###.#"
     "#...........#.#"
     "###.#.#####.#.#"
     "#...#.....#.#.#"
     "#.#.#.###.#.#.#"
     "#.....#...#.#.#"
     "#.###.#.#.#.#.#"
     "#S..#.....#...#"
     "###############")))

(defparameter *example2*
  (aoc:parse-grid-to-array
   '("#################"
     "#...#...#...#..E#"
     "#.#.#.#.#.#.#.#.#"
     "#.#.#.#...#...#.#"
     "#.#.#.#.###.#.#.#"
     "#...#.#.#.....#.#"
     "#.#.#.#.#.#####.#"
     "#.#...#.#.#.....#"
     "#.#.#####.#.###.#"
     "#.#.#.......#...#"
     "#.#.###.#####.###"
     "#.#.#...#.....#.#"
     "#.#.#.#####.###.#"
     "#.#.#.........#.#"
     "#.#.#.#########.#"
     "#S#.............#"
     "#################")))

;;; Part 1

(defun start-position (grid)
  (iter outer
    (for i from 0 below (array-dimension grid 0))
    (iter
      (for j from 0 below (array-dimension grid 1))
      (in outer (finding (complex j i) such-that (char= (aref grid i j) #\S))))))

(defun finish-maze-p (state grid)
  (destructuring-bind (pos dir) state
    (declare (ignore dir))
    (char= (aoc:cref grid pos) #\E)))

(defun possible-turns (direction)
  (cons direction
        (if (zerop (realpart direction))
            (list #C(1 0) #C(-1 0))
            (list #C(0 1) #C(0 -1)))))

(defun possible-moves (state grid)
  (destructuring-bind (pos dir) state
    (iter
      (for dv in (possible-turns dir))
      (for new-pos = (+ pos dv))
      (for points = (if (= dir dv) 1 1001))
      (when (some #'(lambda (c) (char= (aoc:cref grid new-pos) c)) '(#\. #\E))
        (collecting (list points (list new-pos dv)))))))

(defun path (start maze)
  (aoc:shortest-path (list start #C(1 0))
                     (lambda (state) (possible-moves state maze))
                     :finishedp #'(lambda (state) (finish-maze-p state maze))))

(defun get-answer-1 (&optional (maze *input*))
  (multiple-value-bind (path points) (path (start-position maze) maze)
    (declare (ignore path))
    points))

(aoc:given 1
  (= 7036 (get-answer-1 *example1*))
  (= 11048 (get-answer-1 *example2*)))

;;; Part 2

