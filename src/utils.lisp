(defpackage #:utils
  (:use :cl)
  (:export
   #:*year*
   #:input-file-path
   #:input-file-data
   #:input-file-lines
   #:format-solution-code
   #:solution-file-path
   #:generate-solution-file
   #:open-problem-page))

(in-package #:utils)

(defparameter *year* 2023
  "The year of the Advent of Code event.")

(defun input-file-path (day)
  "Return the absolute path of an input file in the repository."
  (let ((system (format nil "aoc~D" *year*))
        (subpath (make-pathname :directory '(:relative "data")
                                :name (format nil "day-~2,'0D" day)
                                :type "txt")))
    (asdf:system-relative-pathname system subpath)))

;; (defun input-file-data (day)
;;   "Return the content of an input file as a string"
;;   (let ((path (input-file-path day)))
;;     (system:read-file path :external-format text:*default-encoding*)))

;; (defun input-file-lines (day)
;;   "Return the content of an input file as a list of lines"
;;   (do* ((data (input-file-data day))
;;         (start 0)
;;         (end (length data))
;;         (lines nil))
;;        ((>= start end)
;;         (nreverse lines))
;;     (let ((eol (or (position #\Newline data :start start :end end) end)))
;;       (push (subseq data start eol) lines)
;;       (setf start (1+ eol)))))

(defun format-solution-code (day &key stream)
  "Generate the initial code for a solution to a daily problem and write it to a stream."
  (flet ((form (control &rest args)
           (apply #'format stream control args)
           (terpri stream)))
    (form "~
(defpackage :aoc~D-~2,'0D
  (:use :cl :aoc~D-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))"
          *year* day *year*)
    (form "~%(in-package :aoc~D-~2,'0D)" *year* day)
    (form "~%(defvar *lines* (input-file-lines ~D))" day)
    (form "~%~
(defun solve-1 ()
  ;; TODO
  nil)")
    (form "~%~
(defun solve-2 ()
  ;; TODO
  nil)")))

(defun solution-file-path (day)
  "Return the absolute path of a solution file in the repository."
  (let ((system (format nil "aoc~D" *year*))
        (subpath (make-pathname :directory '(:relative "src")
                                :name (format nil "day-~2,'0D" day)
                                :type "lisp")))
    (asdf:system-relative-pathname system subpath)))

(defun generate-solution-file (day)
  "Generate the code for the solution file and write it. Return the path of the
file."
  (let ((path (solution-file-path day)))
    (with-open-file (file path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (format-solution-code day :stream file))
    path))

(defun open-problem-page (day)
  "Open the web page for a specific daily problem in Firefox."
  (let* ((uri (format nil "https://adventofcode.com/~D/day/~D" *year* day))
         (command `("firefox" ,uri)))
    (uiop:run-program command :force-shell t :output nil :error-output t)
    nil))
