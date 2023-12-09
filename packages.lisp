(defpackage #:advent-of-code
  (:nicknames #:aoc)
  (:use #:cl
        #:iterate)
  (:export #:comma-list
           #:define-day
           #:deftest
           #:given
           #:input
           #:integer-string
           #:submit
           #:test))

(eval
 `(progn
    ,@(loop for year in '(2023)
            append (loop for number from 1 to 25
                         for package-name = (intern (format nil "ADVENT-OF-CODE-~D-~2,'0D" year number)
                                                    (find-package "KEYWORD"))
                         for nickname = (intern (format nil "AOC-~D-~2,'0D" year number)
                                                (find-package "KEYWORD"))
                         collect `(defpackage ,package-name
                                    (:nicknames ,nickname)
                                    (:use :cl))))))
