(in-package :aoc-2024-10)

(aoc:define-day 737 nil)

;;; Input

(defparameter *map*
  (aoc:parse-grid-to-map (aoc:input) :key #'digit-char-p))
(defparameter *example*
  (aoc:parse-grid-to-map
   '("89010123"
     "78121874"
     "87430965"
     "96549874"
     "45678903"
     "32019012"
     "01329801"
     "10456732")
   :key #'digit-char-p))

(defun find-path (map start-pos &optional (check-visited t))
  (let ((seen (fset:empty-set)))
    (labels ((search-path (position height)
               (if (eq (fset:@ map position) height)
                   (if (or (< height 9)
                           (and check-visited
                                (fset:contains? seen position)))
                       (gmap:gmap (:result :sum)
                                  #'(lambda (dir)
                                      (search-path (point:+ position dir)
                                                   (1+ height)))
                                  (:arg list (list #<1 0> #<-1 0> #<0 1> #<0 -1>)))
                       (progn
                         (setf seen (fset:with seen position))
                         1))
                   0)))
      (search-path start-pos 0))))

(defun sum-scores (map &optional (check-visited t))
  (gmap:gmap (:result :sum)
             #'(lambda (position height)
                 (if (zerop height)
                     (find-path map position check-visited)
                     0))
             (:arg fset:map map)))

(defun get-answer-1 (&optional (map *map*))
  (sum-scores map))

(aoc:given 1
  (= 36 (get-answer-1 *example*)))

;;; Part 2

(defun get-answer-2 (&optional (map *map*))
  (sum-scores map nil))

(aoc:given 2
  (= 81 (get-answer-2 *example*)))
