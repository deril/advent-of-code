(defsystem "aoc2023"
  :author "Dmytro Bihniak"
  :description "Solutions for the Advent of Code 2023 event."
  :license "ISC"

  :depends-on ("cl-ppcre"
               "cl-utilities"
               "parseq")

  :serial t
  :components ((:module "src"
                :serial t
                :components
                ((:file "utils")
                 (:file "day-01"))))
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
  :perform (test-op (op c)
                    (symbol-call :fiveam :run! 'aoc2023-test:day-01)))
