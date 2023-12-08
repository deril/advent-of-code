(defsystem "advent-of-code"
  :author "Dmytro Bihniak"
  :description "Solutions for the Advent of Code 2023 event."
  :license "ISC"

  :depends-on (:cl-ppcre
               :cl-utilities
               :drakma
               :fiveam
               :iterate
               :parseq
               :py-configparser)

  :serial t
  :components #.(append '((:file "packages"))
                        (loop for year in '(2023)
                              append (loop for n from 1 to 25
                                           collect (list :file (format nil "~D/~2,'0D" year n)))))

  :in-order-to ((test-op (test-op "aoc2023/test"))))

(defsystem "aoc2023/test"
  :author "Dmytro Bihniak"
  :description "Tests for the Advent of Code 2023 event."
  :license "ISC"

  :depends-on ("aoc2023"
               "fiveam")

  :serial t
  :components ((:module "test"
                :serial t
                :components
                ((:file "packages")
                 (:file "helper")
                 (:file "day-01"))))
  )
;; :perform (test-op (op c)
;;                   (symbol-call :fiveam :run! 'aoc2023-test:day-01)))
