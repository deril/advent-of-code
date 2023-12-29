(in-package :aoc-2015-07)

(aoc:define-day 46065 14134)

;; Parsing

(parseq:defrule instruction ()
  (and operation " -> " wire)
  (:choose 0 2)
  (:lambda (signal wire) (fset:map (wire signal))))

(parseq:defrule wire ()
  (+ alphanumeric)
  (:string)
  (:lambda (value) (if (parse-integer value :junk-allowed t) (parse-integer value) value)))

(parseq:defrule binary-gate ()
  (and wire " " (or "AND" "OR" "LSHIFT" "RSHIFT") " " wire)
  (:choose 0 2 4)
  (:lambda (a gate b) (cond ((string= gate "AND") `(logand ,a ,b))
                            ((string= gate "OR") `(logior ,a ,b))
                            ((string= gate "LSHIFT") `(ash ,a ,b))
                            ((string= gate "RSHIFT") `(ash ,a ,(- b)))
                            (t (assert nil (gate) "Unknown gate: ~a" gate)))))

(parseq:defrule unary-gate ()
  (and "NOT " wire)
  (:choose 1)
  (:lambda (wire) `(logxor #xffff ,wire)))

(parseq:defrule signal ()
  (or (aoc:integer-string) wire))

(parseq:defrule gate ()
  (or unary-gate binary-gate))

(parseq:defrule operation ()
  (or gate signal))

(defun combine-instructions (instructions)
  (reduce (alexandria:rcurry #'fset:map-union (lambda (a b) (assert (not (and a b)) (a b) "Duplicate wire")))
          instructions))

;; Input

(defparameter *instructions* (combine-instructions (aoc:input :parse-line 'instruction)))
(defparameter *example*
  (combine-instructions
   (mapcar (alexandria:curry #'parseq:parseq 'instruction)
           '("123 -> x"
             "456 -> y"
             "h -> b"
             "1 AND x -> j"
             "d OR e -> a"
             "x AND y -> d"
             "x OR y -> e"
             "x LSHIFT 2 -> f"
             "y RSHIFT 2 -> g"
             "NOT x -> h"
             "NOT y -> i"))))

;; Part 1

(defun make-cache ()
  (let ((cache (fset:empty-map 0)))
    (lambda (&optional command wire value)
      (case command
        (:get (fset:@ cache wire))
        (:set (setf (fset:@ cache wire) value))
        (:reset (setf cache (fset:empty-map 0)))))))

(defparameter compute-cache (make-cache))

(defun reset-cache ()
  (funcall compute-cache :reset))

(defun compute-value (wire instructions)
  (let ((value (fset:@ instructions wire)))
    (etypecase value
      (integer value)
      (string (get-value value instructions))
      (cons (apply (car value)
                   (mapcar (lambda (wire) (get-value wire instructions))
                           (cdr value)))))))

(defun get-value (wire instructions)
  (etypecase wire
    (integer wire)
    (t (multiple-value-bind (cached-value foundp) (funcall compute-cache :get wire nil)
         (if foundp
             cached-value
             (let ((uncached-value (compute-value wire instructions)))
               (funcall compute-cache :set wire uncached-value)
               uncached-value))))))

(defun get-answer-1 (&optional (instructions *instructions*))
  (reset-cache)
  (get-value "a" instructions))

(aoc:deftest get-value
  (reset-cache)
  (5am:is (= 72 (get-value "d" *example*)))
  (5am:is (= 507 (get-value "e" *example*)))
  (5am:is (= 492 (get-value "f" *example*)))
  (5am:is (= 114 (get-value "g" *example*)))
  (5am:is (= 65412 (get-value "h" *example*)))
  (5am:is (= 65079 (get-value "i" *example*)))
  (5am:is (= 123 (get-value "x" *example*)))
  (5am:is (= 456 (get-value "y" *example*)))
  (5am:is (= 65412 (get-value "b" *example*)))
  (5am:is (= 1 (get-value "j" *example*))))

;; Part 2

(defun get-answer-2 (&optional (instructions *instructions*))
  (reset-cache)
  (setf (fset:@ instructions "b") (get-answer-1 instructions))
  (reset-cache) ;; reset cache again to avoid caching the answer
  (get-value "a" instructions))
