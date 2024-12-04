(defpackage #:advent-of-code
  (:nicknames #:aoc)
  (:use #:cl
        #:iterate)
  (:export #:cref
           #:comma-list
           #:define-day
           #:deftest
           #:extract-ints
           #:given
           #:input
           #:integer-string
           #:submit
           #:test))

(defpackage #:permutation
  (:use #:cl)
  (:export #:visit-permutations))

(defpackage #:point
  (:use #:cl
        #:iterate)
  (:export #:point
           #:make-point
           #:x
           #:y))

(eval
 `(progn
    ,@(loop for year in '(2015 2023 2024) ; YEAR-MARKER - do not edit this line.
            append (loop for number from 1 to 25
                         for package-name = (intern (format nil "ADVENT-OF-CODE-~D-~2,'0D" year number)
                                                    (find-package "KEYWORD"))
                         for nickname = (intern (format nil "AOC-~D-~2,'0D" year number)
                                                (find-package "KEYWORD"))
                         collect `(defpackage ,package-name
                                    (:nicknames ,nickname)
                                    (:use :cl :iterate))))))
