(in-package :advent-of-code)

(defun cref (array complex-number)
  "Use a complex number to index a two-dimensional array."
  (aref array (realpart complex-number) (imagpart complex-number)))

(defun loop-n-window (list n function)
  "Loop over a list with a window of size N, applying FUNCTION to each window."
  (loop for el on list
        for i from 1
        while (nthcdr (1- n) el)
        until (apply function (subseq el 0 n))
        finally (return (+ i (1- n)))))
