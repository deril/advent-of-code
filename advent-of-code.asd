(defsystem "advent-of-code"
  :author "Dmytro Bihniak"
  :description "Solutions for the Advent of Code 2023 event."
  :license "GNU GPLv3"

  :depends-on (:com.inuoe.jzon
               :cl-ppcre
               :cl-utilities
               :drakma
               :fiveam
               :fset
               :iterate
               :lquery
               :md5
               :parseq
               :py-configparser)

  :serial t
  :components #.(append '((:file "packages")
                          (:file "permutation")
                          (:file "point")
                          (:file "advent-of-code"))
                        (loop for year in '(2015 2023) ; YEAR-MARKER - do not edit this line.
                              append (loop for n from 1 to 25
                                           collect (list :file (format nil "~D/~2,'0D" year n))))))
