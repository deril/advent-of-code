(in-package :aoc-2024-12)

(aoc:define-day 1465112 893790)

;;; Input

(defparameter *farm* (aoc:parse-grid-to-array (aoc:input)))
(defparameter *example-farm*
  (aoc:parse-grid-to-array
   '("AAAA"
     "BBCD"
     "BBCC"
     "EEEC")))
(defparameter *example-farm2*
  (aoc:parse-grid-to-array
   '("OOOOO"
     "OXOXO"
     "OOOOO"
     "OXOXO"
     "OOOOO")))

;;; Part 1

;; Order is important
(defparameter *directions* '(#C(-1 0) #C(1 0) #C(0 1) #C(0 -1)))
;; Order is important
(defparameter *diagnonals* '(#C(-1 -1) #C(-1 1) #C(1 -1) #C(1 1)))

(defun make-region (farm plant-location)
  (let ((visited (fset:empty-set))
        (region (fset:empty-set))
        (queue (list plant-location)))
    (labels ((enqueue-neighbors (pos)
               (dolist (dir *directions*)
                 (let ((neighbor (+ pos dir)))
                   (when (and (not (fset:contains? visited neighbor))
                              (aoc:carray-in-bounds-p farm neighbor)
                              (char= (aoc:cref farm neighbor)
                                     (aoc:cref farm plant-location)))
                     (push neighbor queue))))))
      (loop
        (when (null queue) (return region))
        (let ((current (pop queue)))
          (setq visited (fset:with visited current))
          (setq region (fset:with region current))
          (enqueue-neighbors current))))))

(defun make-regions (farm)
  (let ((regions '())
        (visited (fset:empty-set)))
    (iter
      (for x below (array-dimension farm 0))
      (iter
        (for y below (array-dimension farm 1))
        (let ((pos (complex x y)))
          (unless (fset:contains? visited pos)
            (let* ((region (make-region farm pos))
                   (plant (aoc:cref farm pos)))
              (setq visited (fset:union visited region))
              (let ((existing-region (assoc plant regions)))
                (if (null existing-region)
                    (push (cons plant (list region)) regions)
                    (push region (cdr existing-region)))))))))
    (mapcar #'cdr regions)))

(defun region-perimeter (region)
  (let ((perimeter (* 4 (fset:size region))))
    (fset:do-set (pos region perimeter)
      (iter
        (for dir in *directions*)
        (for adjacent-pos = (+ pos dir))
        (when (fset:contains? region adjacent-pos)
          (decf perimeter))))))

(defun fence-price (farm)
  (iter
    (for regions in (make-regions farm))
    (summing
     (iter
       (for region in regions)
       (summing
        (* (fset:size region) (region-perimeter region)))))))

(defun get-answer-1 (&optional (farm *farm*))
  (fence-price farm))

(aoc:given 1
  (= 140 (get-answer-1 *example-farm*))
  (= 772 (get-answer-1 *example-farm2*)))

;;; Part 2

(defun region-corners (region)
  (let ((corners 0))
    (fset:do-set (pos region)
      (let ((neighbors (mapcar (lambda (offset)
                                 (fset:contains? region (+ pos offset)))
                               *directions*))
            (diagonals (mapcar (lambda (offset)
                                 (fset:contains? region (+ pos offset)))
                               *diagnonals*)))

        ;; Check for edges (lacking neighbors)
        (unless (or (nth 0 neighbors) (nth 2 neighbors)) (incf corners)) ;; North-East
        (unless (or (nth 0 neighbors) (nth 3 neighbors)) (incf corners)) ;; North-West
        (unless (or (nth 1 neighbors) (nth 2 neighbors)) (incf corners)) ;; South-East
        (unless (or (nth 1 neighbors) (nth 3 neighbors)) (incf corners)) ;; South-West

        ;; Check for missing diagonal connections
        (when (and (nth 0 neighbors) (nth 2 neighbors) (not (nth 1 diagonals))) (incf corners)) ;; North-East diagonal
        (when (and (nth 0 neighbors) (nth 3 neighbors) (not (nth 0 diagonals))) (incf corners)) ;; North-West diagonal
        (when (and (nth 1 neighbors) (nth 2 neighbors) (not (nth 3 diagonals))) (incf corners)) ;; South-East diagonal
        (when (and (nth 1 neighbors) (nth 3 neighbors) (not (nth 2 diagonals))) (incf corners)))) ;; South-West diagonal

    corners))


(defun fence-discounted-price (farm)
  (iter
    (for regions in (make-regions farm))
    (summing
     (iter
       (for region in regions)
       (summing
        (* (fset:size region) (region-corners region)))))))

(defun get-answer-2 (&optional (farm *farm*))
  (fence-discounted-price farm))

(aoc:given 2
  (= 80 (get-answer-2 *example-farm*))
  (= 436 (get-answer-2 *example-farm2*)))
