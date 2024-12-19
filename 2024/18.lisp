(in-package :aoc-2024-18)

(aoc:define-day 316 "45,18")

;;; Input

(defun parse-input (input)
  (mapcar #'(lambda (line)
              (let ((coords (aoc:extract-ints line)))
                (complex (first coords) (second coords))))
          input))

(defparameter *bytes* (parse-input (aoc:input)))

(defparameter *example-bytes*
  (parse-input '("5,4"
                 "4,2"
                 "4,5"
                 "3,0"
                 "2,1"
                 "6,3"
                 "2,4"
                 "1,5"
                 "0,6"
                 "3,3"
                 "2,6"
                 "5,1"
                 "1,2"
                 "5,5"
                 "2,5"
                 "6,5"
                 "1,4"
                 "0,4"
                 "6,4"
                 "1,1"
                 "6,1"
                 "1,0"
                 "0,5"
                 "1,6"
                 "2,0")))

;;; Part 1

(defparameter +cardinals+ (list #C(0 1) #C(1 0) #C(0 -1) #C(-1 0)))

(defun possible-steps (state corrupted-memory)
  (loop for direction in +cardinals+
        for new-state = (+ state direction)
        unless (or (fset:contains? corrupted-memory new-state)
                   (minusp (realpart new-state))
                   (minusp (imagpart new-state))
                   (> (realpart new-state) 70)
                   (> (imagpart new-state) 70))
          collect (list 1 new-state)))

(defun path (corrupted-memory end)
  (aoc:shortest-path (complex 0 0)
                     #'(lambda (state)
                         (possible-steps state corrupted-memory))
                     :end end
                     :heuristic #'(lambda (state)
                                    (aoc:manhattan-distance state end))))

(defun get-answer-1 (&optional (bytes *bytes*))
  (let ((corrupted-memory (fset:convert 'fset:set (subseq bytes 0))))
    (1- (length (path corrupted-memory #C(70 70))))))

;;; Part 2

(defun first-fatal-byte (bytes)
  (let ((left 1024)
        (right (length bytes)))
    (do ((mid (floor (+ left right) 2) (floor (+ left right) 2)))
        ((= left mid) left)
      (if (path (fset:convert 'fset:set (subseq bytes 0 mid)) #C(70 70))
          (setf left mid)
          (setf right mid)))))

(defun get-answer-2 (&optional (bytes *bytes*))
  (let ((fatal-byte (nth (first-fatal-byte bytes) bytes)))
    (format nil "~D,~D" (realpart fatal-byte) (imagpart fatal-byte))))
