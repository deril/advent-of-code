(in-package :aoc-2024-09)

(aoc:define-day 6386640365805 6423258376982) ;; Part 2 is too slow, need to optimize

;;; Input

(defparameter *example* "2333133121414131402")
(defparameter *input* (aoc:input))

;;; Part 1

(defun calc-sum (file-id files-left unpacked-position)
  (iter
    (repeat files-left)
    (for position from unpacked-position)
    (summing (* file-id position))))

(defun disk (input)
  (labels ((collect-data (result position)
             (if (<= (length input) position)
                 result
                 (let ((status (if (evenp position) 'taken 'empty)))
                   (collect-data
                    (fset:with-last result
                      (list :status status
                            :size (digit-char-p (schar input position))
                            :id (floor position 2)))
                    (1+ position))))))
    (collect-data (fset:empty-seq) 0)))

(defun move-files (disk)
  (labels ((compact-memory (i j)
             (when (>= j i)
               (let ((head (fset:lookup disk i))
                     (tail (fset:lookup disk j)))
                 (if (and (eq (getf head :status) 'taken))
                     (compact-memory (1+ i) j) ;; head is full
                     (case (getf tail :status) ;; head is empty
                       (taken
                        (case (signum (- (getf tail :size) (getf head :size)))
                          (1
                           (setf (getf tail :size) (- (getf tail :size) (getf head :size))
                                 (getf head :status) 'taken
                                 (getf head :id) (getf tail :id))
                           (compact-memory (1+ i) j))
                          (0
                           (setf (getf head :status) 'taken
                                 (getf head :id) (getf tail :id)
                                 (getf tail :status) 'empty)
                           (compact-memory (1+ i) (1- j)))
                          (-1
                           (setf disk (fset:insert
                                       disk i
                                       (list :status 'taken
                                             :size (getf tail :size)
                                             :id (getf tail :id)))
                                 (getf head :size) (- (getf head :size) (getf tail :size))
                                 (getf tail :status) 'empty)
                           (compact-memory i (1- j)))))
                       (empty
                        (compact-memory i (1- j)))))))))
    (compact-memory 0 (1- (fset:size disk)))
    disk))

(defun calculate-checksum (disk)
  (let ((position 0))
    (fset:reduce
     #'(lambda (acc block)
         (if (eq (getf block :status) 'taken)
             (let ((sum (calc-sum (getf block :id) (getf block :size) position)))
               (setf position (+ position (getf block :size)))
               (+ acc sum))
             (progn
               (setf position (+ position (getf block :size)))
               acc)))
     disk
     :initial-value 0)))

(defun get-answer-1 (&optional (input *input*))
  (calculate-checksum (move-files (disk input))))

(aoc:given 1
  (= 1928 (get-answer-1 *example*)))

;;; Part 2

(defun move-blocks (disk)
  (labels ((compact-memory (i j)
             (when (plusp j)
               (if (> j i)
                   (let ((head (fset:lookup disk i))
                         (tail (fset:lookup disk j)))
                     (if (and (eq (getf tail :status) 'empty))
                         (compact-memory i (1- j)) ;; tail is empty
                         (case (getf head :status) ;; tail is taken
                           (empty
                            (case (signum (- (getf tail :size) (getf head :size)))
                              (1 ;; tail is bigger
                               (compact-memory (1+ i) j))
                              (0 ;; same size
                               (setf (getf head :status) 'taken
                                     (getf head :id) (getf tail :id)
                                     (getf tail :status) 'empty)
                               (compact-memory 0 (1- j)))
                              (-1 ;; head is bigger
                               (setf disk (fset:insert
                                           disk i
                                           (list :status 'taken
                                                 :size (getf tail :size)
                                                 :id (getf tail :id)))
                                     (getf head :size) (- (getf head :size) (getf tail :size))
                                     (getf tail :status) 'empty)
                               (compact-memory 0 (1- j)))))
                           (taken
                            (compact-memory (1+ i) j)))))
                   (compact-memory 0 (1- j))))))
    (compact-memory 0 (1- (fset:size disk)))
    disk))

(defun get-answer-2 (&optional (input *input*))
  (calculate-checksum (move-blocks (disk input))))

(aoc:given 2
  (= 2858 (get-answer-2 *example*)))
