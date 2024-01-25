(in-package :aoc-2015-14)

(aoc:define-day 2655 1059)

;; Parsing

(parseq:defrule reindeer-name ()
    (and (+ alpha))
  (:string))

(parseq:defrule regndeer ()
    (and (reindeer-name)
         " can fly "
         (aoc:integer-string)
         " km/s for "
         (aoc:integer-string)
         " seconds, but then must rest for "
         (aoc:integer-string)
         " seconds.")
  (:choose 0 2 4 6)
  (:lambda (name speed duration rest)
    (make-reindeer :name name
                   :speed speed
                   :duration duration
                   :rest rest)))

(defstruct reindeer
  name
  speed
  duration
  rest)

;; Input

(defparameter *regndeers* (aoc:input :parse-line 'regndeer))

(defparameter *example-regndeers*
  (mapcar (alexandria:curry #'parseq:parseq 'regndeer)
          '("Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
            "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.")))

;; Part 1

(defun distance (regndeer time)
  (let ((cycle-time (+ (reindeer-duration regndeer)
                       (reindeer-rest regndeer))))
    (multiple-value-bind (full-cycles non-full-cycle)
        (truncate time cycle-time)
      (with-slots (speed duration) regndeer
        (+ (* full-cycles speed duration)
           (* (min duration non-full-cycle)
              speed))))))

(defun get-answer-1 (&optional (regndeers *regndeers*) (measure-time 2503))
  (iter (for regndeer in regndeers)
    (for distance = (distance regndeer measure-time))
    (finding distance maximizing distance)))

(aoc:given 1
  (= 1120 (get-answer-1 *example-regndeers* 1000)))

;; Part 2

(defun winner-points (regndeer time)
  (iter (with points = (make-array (length regndeer) :initial-element 0))
    (for second from 1 to time)
    (for distances = (mapcar (alexandria:rcurry #'distance second) regndeer))
    (for max-distance = (reduce #'max distances))
    (iter (for distance in-sequence distances with-index i)
      (when (= distance max-distance)
        (incf (aref points i))))
    (finally (return (reduce #'max points)))))

(defun get-answer-2 (&optional (regndeers *regndeers*) (measure-time 2503))
  (winner-points regndeers measure-time))

(aoc:given 2
  (= 689 (get-answer-2 *example-regndeers* 1000)))
