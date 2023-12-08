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
  :components #.(append '((:file "packages")
                          (:file "advent-of-code"))
                        (loop for year in '(2023)
                              append (loop for n from 1 to 25
                                           collect (list :file (format nil "~D/~2,'0D" year n))))))
