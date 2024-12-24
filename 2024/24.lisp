(in-package :aoc-2024-24)

(aoc:define-day 49430469426918 nil)

;; Parsing

(parseq:defrule wire ()
    (rep 3 alphanumeric)
  (:string)
  (:lambda (wire) (read-from-string wire)))

(parseq:defrule resolved-wire ()
    (and wire ": " (aoc:integer-string))
  (:choose 0 2))

(parseq:defrule gate ()
    (or "AND" "OR" "XOR")
  (:lambda (gate) (read-from-string gate)))

(parseq:defrule binary-gate ()
    (and wire " " gate " " wire " -> " wire)
  (:choose 0 2 4 6)
  (:lambda (a b gate out) (list a gate b out)))

;; Input

(defun split-input (input)
  (let ((split (position "" input :test #'string=)))
    (values (subseq input 0 split)
            (subseq input (1+ split)))))

(defun parse-input (input)
  (multiple-value-bind (resolved unresolved) (split-input input)
    (list
     (mapcar (alexandria:curry #'parseq:parseq 'resolved-wire) resolved)
     (mapcar (alexandria:curry #'parseq:parseq 'binary-gate) unresolved))))

(defparameter *input* (parse-input (aoc:input)))

(defparameter *example*
  (parse-input
   '("x00: 1"
     "x01: 1"
     "x02: 1"
     "y00: 0"
     "y01: 1"
     "y02: 0"
     ""
     "x00 AND y00 -> z00"
     "x01 XOR y01 -> z01"
     "x02 OR y02 -> z02")))

(defparameter *example2*
  (parse-input
   '("x00: 1"
     "x01: 0"
     "x02: 1"
     "x03: 1"
     "x04: 0"
     "y00: 1"
     "y01: 1"
     "y02: 1"
     "y03: 1"
     "y04: 1"
     ""
     "ntg XOR fgs -> mjb"
     "y02 OR x01 -> tnw"
     "kwq OR kpj -> z05"
     "x00 OR x03 -> fst"
     "tgd XOR rvg -> z01"
     "vdt OR tnw -> bfw"
     "bfw AND frj -> z10"
     "ffh OR nrd -> bqk"
     "y00 AND y03 -> djm"
     "y03 OR y00 -> psh"
     "bqk OR frj -> z08"
     "tnw OR fst -> frj"
     "gnj AND tgd -> z11"
     "bfw XOR mjb -> z00"
     "x03 OR x00 -> vdt"
     "gnj AND wpb -> z02"
     "x04 AND y00 -> kjc"
     "djm OR pbm -> qhw"
     "nrd AND vdt -> hwm"
     "kjc AND fst -> rvg"
     "y04 OR y02 -> fgs"
     "y01 AND x02 -> pbm"
     "ntg OR kjc -> kwq"
     "psh XOR fgs -> tgd"
     "qhw XOR tgd -> z09"
     "pbm OR djm -> kpj"
     "x03 XOR y03 -> ffh"
     "x00 XOR y04 -> ntg"
     "bfw OR bqk -> z06"
     "nrd XOR fgs -> wpb"
     "frj XOR qhw -> z04"
     "bqk OR frj -> z07"
     "y03 OR x01 -> nrd"
     "hwm AND bqk -> z03"
     "tgd XOR rvg -> z12"
     "tnw OR pbm -> gnj")))

;; Part 1

(defstruct gate a b op out value)

(defun compute (gate cache)
  (with-slots (a b op out value) gate
    (unless value
      (let ((a-val (gethash a cache))
            (b-val (gethash b cache)))
        (when (and a-val b-val)
          (setf value (ecase op
                        (and (logand a-val b-val))
                        (or (logior a-val b-val))
                        (xor (logxor a-val b-val))))
          (when value
            (setf (gethash out cache) value)))))
    value))

(defun calculate-result (resolved unresolved wire-prefix)
  (let ((cache (make-hash-table)))
    (dolist (wire resolved)
      (destructuring-bind (name value) wire
        (setf (gethash name cache) value)))
    (let ((gates (mapcar #'(lambda (gate-data)
                             (destructuring-bind (a b op out) gate-data
                               (make-gate :a a :op op :b b :out out)))
                         unresolved)))
      (loop
        (when (null gates) (return))
        (setf gates (remove-if #'(lambda (gate) (compute gate cache)) gates :from-end t))))
    (loop with result = 0
          for name being the hash-keys of cache
          for value being the hash-values of cache
          when (alexandria:starts-with wire-prefix (symbol-name name))
            do (let ((index (parse-integer (subseq (symbol-name name) 1))))
                 (setf result (dpb value (byte 1 index) result)))
          finally (return result))))

(defun get-answer-1 (&optional (input *input*))
  (reset-cache)
  (destructuring-bind (resolved unresolved) input
    (calculate-result resolved unresolved #\Z)))

(aoc:given 1
  (= 4 (get-answer-1 *example*))
  (= 2024 (get-answer-1 *example2*)))

;; Part 2

