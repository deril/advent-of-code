(in-package :aoc-2024-08)

(aoc:define-day 259 927)

;;; Parsing

(defun parse-symbol-row (row index)
  (loop for pos below (length row)
        unless (char= #\. (schar row pos))
          collect (cons (schar row pos) #<pos index>)))

(defun parse-rows (rows)
  (loop for row in rows
        for index from 0
        nconcing (parse-symbol-row row index)))

(defun parse-antenas (rows)
  (let ((antenas '()))
    (loop for (antena-id . antena-position) in (parse-rows rows)
          for antena = (assoc antena-id antenas)
          if antena
            do (push antena-position (cdr antena))
          else
            do (push (cons antena-id (list antena-position)) antenas))
    antenas))

;;; Input

(defparameter *antenas* (parse-antenas (aoc:input)))
(defparameter *dimensions* (point:make-point (length (car (aoc:input))) (length (aoc:input))))

(defparameter *example*
  '("............"
    "........0..."
    ".....0......"
    ".......0...."
    "....0......."
    "......A....."
    "............"
    "............"
    "........A..."
    ".........A.."
    "............"
    "............"))
(defparameter *example-antenas* (parse-antenas *example*))
(defparameter *example-dimensions* (point:make-point (length (car *example*)) (length *example*)))

;;; Part 1

(defun point-in-bounds-p (point dimentions)
  (point:< #<-1 -1> point dimentions))

(defun find-antinodes (points dimensions &key (mode :delta))
  (let ((antinodes '())
        (max-range (if (eql mode :line)
                       (max (point:x dimensions) (point:y dimensions))
                       1)))
    (dolist (i points)
      (dolist (j points)
        (unless (point:= i j)
          (let ((delta (if (eql mode :delta)
                           (point:- i j)
                           (point:- j i))))
            (loop for k from 1 to max-range
                  for scaled-delta = (point:* delta k)
                  for antinode1 = (point:+ i scaled-delta)
                  for antinode2 = (point:- j scaled-delta)
                  when (point-in-bounds-p antinode1 dimensions)
                    do (push antinode1 antinodes)
                  when (point-in-bounds-p antinode2 dimensions)
                    do (push antinode2 antinodes))))))
    antinodes))

(defun count-antinodes (antenas dimensions &key (mode :delta))
  (length
   (remove-duplicates
    (mapcan #'(lambda (antena) (find-antinodes (cdr antena) dimensions :mode mode)) antenas)
    :test #'point:=)))

(defun get-answer-1 (&optional (input *antenas*) (dimensions *dimensions*))
  (count-antinodes input dimensions :mode :delta))

(aoc:given 1
  (= 14 (get-answer-1 *example-antenas* *example-dimensions*)))

;;; Part 2

(defun get-answer-2 (&optional (input *antenas*) (dimensions *dimensions*))
  (count-antinodes input dimensions :mode :line))

(aoc:given 2
  (= 34 (get-answer-2 *example-antenas* *example-dimensions*)))
