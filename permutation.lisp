(in-package :permutation)

;;; Based on the plain-change permutation algorithm from TAOCP Volume 4A
;;; §7.2.1.2.
;;;
;;; The essence of the algorithm is that you can work your way through all
;;; n! permutations of a set of n elements by making n!-1 swaps of
;;; adjacent elements, if you do the swaps in the right order.  Obviously,
;;; this is very efficient, since you're not moving much data around in
;;; memory and the amount of work between each pair of permutations is
;;; minimized.
;;;
;;; The algorithm operates mostly recursively.  It considers _inversions_
;;; of the elements to be the number of times each element has been
;;; shifted to the left relative to its original position.  With this
;;; definition, each element has a maximum inversion level equal to its
;;; zero-based index within the original sequence.
;;;
;;; Each inversion change can only happen once the element's successor has
;;; gone through a complete sequence of inversion changes, resulting in it
;;; either being fully inverted or fully uninverted.  Once an element has
;;; gone through a complete cycle, it passes its successor through one
;;; last complete cycle.  Thus, the recursive algorithm for inverting
;;; element j is, roughly:
;;;
;;;  * Fully invert element j+1
;;;  * Move element j one step closer toward full inversion.
;;;  * Fully uninvert element j+1
;;;  * If element j is not fully inverted:
;;;    * Move element j one step closer to full inversion
;;;    * Fully invert element j+1
;;;    * If element j is not fully inverted:
;;;      * Move element j one step closer to full inversion
;;;      * Fully uninvert element j+1
;;;      * etc.
;;;
;;; Uninverting the element follows roughly the same process.  If the last
;;; step of inversion was to invert element j+1, then the uninverson of
;;; element j must start with the _uninversion_ of element j+1.
;;;
;;; One wrinkle while shifting the position of an element is that its
;;; expected position in the vector might be offset by elements normally
;;; to its right that have been fully inverted and are now on its left.
;;; Fortunately a few properties make this easy to deal with:
;;;
;;;  * The last step undertaken by the uninversion of element j is to
;;;    uninvert element j+1.  This means that after an uninversion the
;;;    offset is always 0.
;;;  * Elements in odd-numbered positions (zero-based) always end their
;;;    inversion processes by uninverting their successors, so they only
;;;    apply an offset of 1 to their predecessors when they're fully
;;;    inverted.
;;;  * Elements in even-numbered positions (zero-based) always end their
;;;    inversion process by inverting their successors.  That means they
;;;    add one to the offset given by their successors.  Since their
;;;    successors are odd-numbered, their predecessors will always receive
;;;    an offset of 2.
;;;  * An exception is the penultimate element in the list.  Its offset
;;;    can only ever be 1, because there's only one element that might be
;;;    inverted past it.
;;;
;;; All told, that leads to the following rules:
;;;
;;;  * After uninverting its successor, an element's offset is always 0.
;;;  * The final element's offset is always 0.
;;;  * After inverting its successor, an element's offset is:
;;;    * 2 if the element's position is even _and_ it's not the penultimate
;;;      element, and
;;;    * 1 otherwise.
;;;
;;; In the code below inversion is referred to as "descent" and
;;; uninversion is referred to as "ascent".  Mostly because "uninversion"
;;; is a weird-sounding word.
;;;
(defun visit-permutations (visit-fn sequence &key (copy t))
  "VISIT-FN will be called once for each permutation of SEQUENCE. Order of
permutations is not guaranteed. SEQUENCE should be a set of elements. If
COPY is true, the sequence will be copied before being permuted. If COPY
is false, the sequence will be permuted in place."
  (let* ((n (length sequence))
         (result (make-array n :initial-contents sequence)))
    (labels ((visit ()
               (funcall visit-fn
                        (if copy
                            (coerce (copy-seq result) (type-of sequence))
                            result)))
             (inner-descent ()
               (do ((j (1- n) (1- j)))
                   ((zerop j))
                 (visit)
                 (rotatef (svref result j) (svref result (1- j)))))
             (inner-ascent ()
               (do ((j 1 (1+ j)))
                   ((<= n j))
                 (visit)
                 (rotatef (svref result j) (svref result (1- j)))))
             (make-middle (direction position next-descent next-ascent)
               (let ((offset (if (and (oddp position) (< position (- n 2))) 2 1)))
                 (if (eq direction :descent)
                     (lambda ()
                       (do ((j position (1- j))
                            (count 0 (1+ count)))
                           ((zerop j))
                         (if (evenp count)
                             (progn
                               (funcall next-descent)
                               (visit)
                               (rotatef (svref result (+ offset j))
                                        (svref result (+ offset j -1))))
                             (progn
                               (funcall next-ascent)
                               (visit)
                               (rotatef (svref result j)
                                        (svref result (1- j))))))
                       (if (evenp position)
                           (funcall next-descent)
                           (funcall next-ascent)))
                     (lambda ()
                       (do ((j 1 (1+ j))
                            (count (if (evenp position) 1 0) (1+ count)))
                           ((< position j))
                         (if (evenp count)
                             (progn
                               (funcall next-descent)
                               (visit)
                               (rotatef (svref result (+ offset j))
                                        (svref result (+ offset j -1))))
                             (progn
                               (funcall next-ascent)
                               (visit)
                               (rotatef (svref result j)
                                        (svref result (1- j))))))
                       (funcall next-ascent)))))
             (make-outer (next-descent next-ascent)
               (let ((j (if (= n 3) 2 3)))
                 (lambda ()
                   (funcall next-descent)
                   (visit)
                   (rotatef (svref result j) (svref result (1- j)))
                   (funcall next-ascent)
                   (visit))))
             (build-func-tree (position next-descent next-ascent)
               (cond
                 ((null next-ascent)
                  (build-func-tree (1- position) #'inner-descent #'inner-ascent))
                 ((= position 1)
                  (make-outer next-descent next-ascent))
                 (t
                  (let ((new-descent (make-middle :descent position next-descent next-ascent))
                        (new-ascent (make-middle :ascent position next-descent next-ascent)))
                    (build-func-tree (1- position) new-descent new-ascent))))))
      (declare (inline visit))
      (case n
        ((0 1)
         (visit))
        (2
         (visit)
         (rotatef (svref result 1) (svref result 0))
         (visit))
        (otherwise
         (funcall (build-func-tree (1- n) nil nil))))
      (values))))

(defgeneric visit-subsets (visit-fn collection subset-size)
  (:documentation
   "VISIT-FN will be called once for each subset.  It will be passed an
   FSet bag."))

(defmethod visit-subsets (visit-fn (collection fset:seq) subset-size)
  (declare (type function visit-fn)
           (type (integer 0) subset-size))
  (labels ((visit-r (remaining-items remaining-size result)
             (if (zerop remaining-size)
                 (funcall visit-fn result)
                 (fset:do-seq (element remaining-items :index i :end (1+ (- (fset:size remaining-items) remaining-size)))
                   (visit-r (fset:subseq remaining-items (1+ i))
                            (1- remaining-size)
                            (fset:with result element))))))
    (visit-r collection subset-size (fset:empty-bag))
    (values)))
