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

(defun manhattan-distance (complex-number &optional (reference 0))
  (+ (abs (- (realpart complex-number) (realpart reference)))
     (abs (- (imagpart complex-number) (imagpart reference)))))

(defun shortest-path (start option-fn &key end finishedp test heuristic)
  "Finds the shortest path from START to END.  OPTION-FN should be a
  function that accepts a state and returns a list of `(cost state)` pairs
  signifying next moves from the given state.  Returns two values: a list
  of states from START to END, and the total cost of the path.  TEST
  determines how the states are compared, in case `fset:compare' is
  insufficient.  It should be a function that receives two states as
  parameters and returns a true value if the states are equal.  The
  optional HEURISTIC is a function that is called with a state and returns
  an estimate of the minimum cost from that state to END.

  In place of END, you can give FINISHEDP, which is a function that will
  be called on each state.  It should return true if the state is the end
  of the path and false otherwise.

  The states (START and the values returned from OPTION-FN) must be usable
  in FSet collections.  If they're not natively supported by FSet, you'll
  need to make sure you've defined `fset:compare' for them."
  (when (and (not end)
             (not finishedp))
    (error "Must give either END or FINISHEDP."))
  (when (and end finishedp)
    (error "Cannot give both END and FINISHEDP."))
  (let ((finished-fn (or finishedp
                         (if test
                             (alexandria:curry test end)
                             (lambda (state)
                               (eq :equal (fset:compare end state)))))))
    (if (funcall finished-fn start)
        (values (list start) 0)
        (let ((visited (fset:map (start (list start))))
              (options (fpq:empty-queue :pred #'<=
                                        :key (if heuristic
                                                 (lambda (edge)
                                                   (destructuring-bind (cost from-node to-node) edge
                                                     (declare (ignore from-node))
                                                     (+ cost (funcall heuristic to-node))))
                                                 #'first))))
          (find-shortest-path
           finished-fn option-fn visited
           (shortest-path-add-edges 0 options start (funcall option-fn start)))))))

(defun find-shortest-path (finished-fn option-fn visited options)
  (if (fpq:empty? options)
      (values nil nil)
      (multiple-value-bind (new-options edge) (fpq:delete-root options)
        (destructuring-bind (cost from-node to-node) edge
          (cond
            ((fset:lookup visited to-node)
             (find-shortest-path finished-fn option-fn visited new-options))
            ((funcall finished-fn to-node)
             (values (reverse (cons to-node (fset:lookup visited from-node)))
                     cost))
            (t
             (let ((next-visited (fset:with visited to-node
                                            (cons to-node (fset:lookup visited from-node))))
                   ;; Even though the algorithm skips visited edges
                   ;; elsewhere, removing them here cuts down on the work
                   ;; needed down the road.  But!  Removing them here
                   ;; doesn't remove the need to check for them later, too;
                   ;; a node may be visited between being added to the
                   ;; options and being extracted as the next candidate.
                   (next-edges (remove-if (lambda (edge) (fset:lookup visited (second edge)))
                                          (funcall option-fn to-node))))
               (find-shortest-path
                finished-fn option-fn next-visited
                (shortest-path-add-edges cost new-options to-node next-edges)))))))))

(defun shortest-path-add-edges (base-cost options start-node edges)
  "Adds all of EDGES to OPTIONS."
  (reduce (lambda (result edge)
            (destructuring-bind (cost node) edge
              (fpq:insert result
                          (list (+ base-cost cost)
                                start-node
                                node))))
          edges
          :initial-value options))
