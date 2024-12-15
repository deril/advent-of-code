(in-package :advent-of-code)

(defun cref (array complex-number)
  "Use a complex number to index a two-dimensional array."
  (aref array (imagpart complex-number) (realpart complex-number)))

(defun (setf cref) (new-value array complex-number)
  (setf (aref array (imagpart complex-number) (realpart complex-number))
        new-value))

(defun carray-in-bounds-p (array complex-number)
  "Check if a complex number is within the bounds of a two-dimensional array."
  (let ((x (realpart complex-number))
        (y (imagpart complex-number)))
    (array-in-bounds-p array x y)))

(defun loop-n-window (list n function)
  "Loop over a list with a window of size N, applying FUNCTION to each window."
  (loop for el on list
        for i from 1
        while (nthcdr (1- n) el)
        until (apply function (subseq el 0 n))
        finally (return (+ i (1- n)))))

(defmacro-driver (FOR var DIGITS-OF number
                      &optional BASE (base 10) FROM (direction 'right))
  "Each digit of a number.  DIRECTION is either LEFT or RIGHT and indicates
   the direction in which the number's digits will be processed.  RIGHT is
   slightly more efficient."
  (when (and (string/= "RIGHT" (symbol-name direction))
             (string/= "LEFT" (symbol-name direction)))
    (error "DIRECTION must be either LEFT or RIGHT, not ~A." direction))
  (let ((b (if (integerp base)
               base
               (gensym "BASE")))
        (n (gensym "N"))
        (divisor (if (and (integerp base)
                          (string= "RIGHT" (symbol-name direction)))
                     base
                     (gensym "DIVISOR")))
        (init-n (if (integerp number)
                    number
                    (gensym "INIT-N")))
        (quotient (gensym "QUOTIENT"))
        (remainder (gensym "REMAINDER"))
        (kwd (if generate 'generate 'for))
        (from-right (string= "RIGHT" (symbol-name direction))))
    `(progn
       ,(when (symbolp b)
          `(with ,b = ,base))
       (with ,n = nil)
       ,(when (symbolp init-n)
          `(with ,init-n = ,number))
       ,(when (symbolp divisor)
          `(with ,divisor = ,(if from-right
                                 b
                                 `(expt ,b (floor (log ,init-n ,b))))))
       (,kwd ,var next (progn
                         (when (and ,n (zerop ,(if from-right
                                                   n
                                                   divisor)))
                           (terminate))
                         (multiple-value-bind (,quotient ,remainder)
                             (truncate (or ,n ,init-n)
                                       ,divisor)
                           ,(when (not from-right)
                              `(setf ,divisor (truncate ,divisor ,b)))
                           (setf ,n ,(if from-right
                                         quotient
                                         remainder))
                           ,(if from-right
                                remainder
                                quotient)))))))

(defun digit-list (num &optional (base 10))
  (declare (type (integer 0) num)
           (type (integer 2 36) base))
  (iter (for d digits-of num base base)
    (declare (type (integer 0 35) d))
    (collecting d at beginning)))

(defun digit-vector (num &optional (base 10))
  (declare (type (integer 0) num)
           (type (integer 2 36) base))
  (coerce (digit-list num base) 'simple-vector))

(defun map-set-with (map key new-value)
  "If MAP contains set, updates the set at KEY to also contain NEW-VALUE."
  (fset:with map key
             (fset:with (fset:lookup map key)
                        new-value)))
