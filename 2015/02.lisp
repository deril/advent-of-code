(in-package :aoc-2015-02)

(aoc:define-day 1586300 3737498)

;; Parsing

(parseq:defrule dimentions ()
  (and aoc:integer-string "x" aoc:integer-string "x" aoc:integer-string)
  (:choose 0 2 4))

;; Input

(defparameter *presents* (aoc:input :parse-line 'dimentions))
(defparameter *example*
  (mapcar (alexandria:curry #'parseq:parseq 'dimentions)
          '("2x3x4"
            "1x1x10")))

;; Part 1

(defun smallest-side (box)
  (subseq (sort box #'<) 0 2))

(defun wrapping-paper-area (box)
  (destructuring-bind (w h l) box
    (+ (* 2 l w)
       (* 2 w h)
       (* 2 h l)
       (reduce #'* (smallest-side box)))))

(defun get-answer-1 (&optional (presents *presents*))
  (reduce #'+ (mapcar #'wrapping-paper-area presents)))

(aoc:given 1
  (= 101 (get-answer-1 *example*)))

;; Part 2

(defun ribbon-length (box)
  (destructuring-bind (w h l) box
    (+ (* 2 (reduce #'+ (smallest-side box)))
       (* w h l))))

(defun get-answer-2 (&optional (presents *presents*))
  (reduce #'+ (mapcar #'ribbon-length presents)))

(aoc:given 2
  (= 48 (get-answer-2 *example*)))
