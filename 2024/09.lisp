(in-package :aoc-2024-09)

(aoc:define-day 6386640365805 nil)

;;; Input

(defparameter *example* (parse-integer "2333133121414131402"))
(defparameter *input* (parse-integer (aoc:input)))

;;; Part 1
;;; (+ 0 2 4 3 4 5 12 14 16)


(defun calc-sum (file-id files-left unpacked-position)
  (iter
    (repeat files-left)
    (for position from unpacked-position)
    (summing (* file-id position))))

(defun calculate-checksum (input max-file-id)
  (labels ((iteration (lfile-id rfile-id lindex rindex position checksum op)
             (case op
               (file
                (format t "operation: file, unpacking ~A files, unpacked position ~A, current file-id ~A" (aref input lindex) position lfile-id))
               (empty
                (format t "operation: empty, try to fill ~A spaces, ~A files available, unpacked position ~A, current file-id ~A" (aref input lindex) (aref input rindex) position rfile-id)))
             ;; lfile-id - file-id of the left file. It should increase by 1 when the left file is unpacked.
             ;; rfile-id - file-id of the right file. It should decrease by 1 when the right file is moved.
             ;; lindex - cursor of the left file.
             ;; rindex - cursor of the right file.
             ;; position - current position in the unpacked file.
             ;; checksum - current checksum we accumulate.
             ;; op - operation to perform. It can be 'file or 'empty.
             ;; if (aref input lindex) is -1, then we have processed empty state.
             ;; if (aref input rindex) is -1, then we have moved the right file.
             (if (> lindex rindex)
                 checksum
                 (case op
                   (file
                    (iteration
                     (1+ lfile-id)
                     rfile-id
                     (1+ lindex)
                     rindex
                     (+ position (aref input lindex))
                     (+ checksum (calc-sum lfile-id (aref input lindex) position))
                     'empty))
                   (empty
                    (let ((could-fill-empty-space-p (signum (- (aref input lindex)
                                                               (aref input rindex))))
                          (tmp-checksum (calc-sum rfile-id (min (aref input rindex) (aref input lindex)) position))
                          (file-count-we-can-fill (min (aref input rindex) (aref input lindex)))
                          )
                      (case could-fill-empty-space-p
                        (1
                         (setf (aref input lindex) (- (aref input lindex) file-count-we-can-fill)
                               (aref input rindex) -1))
                        (-1
                         (setf (aref input rindex) (- (aref input rindex) file-count-we-can-fill)
                               (aref input lindex) -1))
                        (0
                         (setf (aref input lindex) -1
                               (aref input rindex) -1)))
                      (iteration
                       lfile-id
                       (if (minusp (aref input rindex)) (1- rfile-id) rfile-id) ;; if we have moved the right file, then we should decrease the file-id.
                       (if (minusp (aref input lindex)) (1+ lindex) lindex) ;; if we have processed empty state, then we should increase the cursor.
                       (if (minusp (aref input rindex)) (- rindex 2) rindex) ;; if we have moved the right file, then we should decrease the cursor.
                       (+ position file-count-we-can-fill)
                       (+ checksum tmp-checksum)
                       (if (minusp (aref input lindex)) 'file 'empty) ;; if we have processed empty state, then we should move to the next file.
                       )))))))
    (iteration 0 max-file-id 0 (- (length input) 1) 0 0 'file)))

(defun get-answer-1 (&optional (input *input*))
  (let ((array (aoc:digit-vector input)))
    (calculate-checksum array (floor (length array) 2))))

(aoc:given 1
  (= 1928 (get-answer-1 *example*)))

;; 5 4 3 2 1
;; 0 0 0 0 0 . . . . 1 1 1 . . 2
;; 0 0 0 0 0 2 . . . 1 1 1 . . .
;; 0 0 0 0 0 2 1 1 1 . . . . . .
;; (+ (* 0 0) (* 0 1) (* 0 2) (* 0 3) (* 0 4) (* 2 5) (* 1 6) (* 1 7) (* 1 8)) = 31
;;
;; 1 2 3 4 5
;; 0 . . 1 1 1 . . . . 2 2 2 2 2
;; 0 2 2 1 1 1 . . . . 2 2 2 . .
;; 0 2 2 1 1 1 2 2 2 . . . . . .
;; (+ (* 0 0) (* 2 1) (* 2 2) (* 1 3) (* 1 4) (* 1 5) (* 2 6) (* 2 7) (* 2 8)) = 60
;;
;; 1 2 0 3 4 5 1
;; 0 . . . . . 2 2 2 2 . . . . . 3
;; 0 3 . . . . 2 2 2 2 . . . . . .
;; 0 3 2 2 2 2 . . . . . . . . . .
;; (+ (* 0 0) (* 3 1) (* 2 2) (* 2 3) (* 2 4) (* 2 5)) = 31

;;; Part 2
