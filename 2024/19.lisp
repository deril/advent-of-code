(in-package :aoc-2024-19)

(aoc:define-day 283 615388132411142)

;; Parsing

(parseq:defrule towel ()
    (+ alpha)
  (:string))

;; Input

(defun separate-input (input)
  (let ((split (position "" input :test #'string=)))
    (list (subseq input 0 split)
          (subseq input (1+ split)))))

(defun parse-input (input)
  (destructuring-bind (towels patterns) (separate-input input)
    (list (parseq:parseq '(aoc:comma-list towel) (first towels))
          patterns)))

(defparameter *example-towels*
  (parse-input
   '("r, wr, b, g, bwu, rb, gb, br"
     ""
     "brwrr"
     "bggr"
     "gbbr"
     "rrbgbr"
     "ubwu"
     "bwurrg"
     "brgr"
     "bbrgwb")))

(defparameter *towels* (parse-input (aoc:input)))

;; Part 1

(defun count-possible-patterns (towels pattern)
  (let ((pattern-len (length pattern))
        (dp (make-array (1+ (length pattern))
                        :initial-element 0
                        :element-type '(integer 0))))
    (setf (svref dp 0) 1)
    (dotimes (i pattern-len)
      (when (> (svref dp i) 0)
        (dolist (towel towels)
          (let ((len (length towel)))
            (when (and (<= (+ i len) pattern-len)
                       (string= towel (subseq pattern i (+ i len))))
              (incf (svref dp (+ i len)) (svref dp i)))))))
    (aref dp pattern-len)))

(defun get-answer-1 (&optional (towels *towels*))
  (destructuring-bind (towels patterns) towels
    (count-if #'(lambda (pattern) (> (count-possible-patterns towels pattern) 0))
              patterns)))

(aoc:given 1
  (= 6 (get-answer-1 *example-towels*)))

;; Part 2

(defun get-answer-2 (&optional (towels *towels*))
  (destructuring-bind (towels patterns) towels
    (gmap:gmap
     (:result :sum)
     (alexandria:curry #'count-possible-patterns towels)
     (:arg list patterns))))

(aoc:given 2
  (= 16 (get-answer-2 *example-towels*)))
