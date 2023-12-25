(in-package :advent-of-code)
(5am:def-suite :aoc-all)
(5am:def-suite :aoc :in :aoc-all)

;; Configuration

(defvar *config-file* (merge-pathnames ".aocrc" (user-homedir-pathname))
  "The path to the Advent of Code configuration file.")
(defvar *config-key-session* "session")
(defvar *config-key-cachedir* "cachedir")
(defvar *user-agent-string* "Deril; Drakma; Common Lisp;")

(defparameter *configuration* (py-configparser:make-config))
(py-configparser:read-files *configuration* (list *config-file*))

(defun get-config-key (key)
  (py-configparser:get-option *configuration* "advent-of-code" key))

(defun get-cachedir ()
  (get-config-key *config-key-cachedir*))

;; Data from package name

(defun get-current-year ()
  (multiple-value-bind (match groups)
      (ppcre:scan-to-strings "^ADVENT-OF-CODE-(\\d+)"
                             (package-name *package*))
    (unless match
      (error "Current package does not have an Advent of Code year: ~A" *package*))
    (parse-integer (svref groups 0))))

(defun get-current-day ()
  (multiple-value-bind (match groups)
      (ppcre:scan-to-strings "^ADVENT-OF-CODE-\\d+-(\\d+)"
                             (package-name *package*))
    (unless match
      (error "Current package does not have an Advent of Code day: ~A" *package*))
    (parse-integer (svref groups 0))))

;; Tests

(5am:def-suite :aoc-2023 :in :aoc-all)
(5am:def-suite :aoc-2015 :in :aoc-all)
;; DEFSUITE-MARKER - Do not edit this line.

(defun 5am-year-suite ()
  (intern (format nil "AOC-~4,'0D" (get-current-year)) 'keyword))

(defun 5am-day-suite ()
  (intern (format nil "AOC-~4,'0D-~2,'0D" (get-current-year) (get-current-day))))

(defun test (&optional suite)
  "Runs the test suite for the current day's package."
  (5am:run! (or suite (5am-day-suite))))

(flet ((gen-answer-test (number answer)
         (let ((suite (5am-day-suite))
               (get-answer (intern (format nil "GET-ANSWER-~A" number))))
           `(5am:test (,(intern (format nil "ANSWER-~A" number))
                       :suite ,suite)
              (5am:is ,(cond
                         ((typep answer 'number)
                          `(= ,answer (,get-answer)))
                         ((typep answer 'string)
                          `(string= ,answer (,get-answer)))
                         (t
                          `(equalp ,answer (,get-answer)))))))))
  (defmacro define-day (answer-1 answer-2)
    "Sets up the test suites for the current day's package."
    (let ((suite (5am-day-suite)))
      `(progn
         (5am:def-suite ,suite :in ,(5am-year-suite))
         ,(when answer-1
            (gen-answer-test 1 answer-1))
         ,(when answer-2
            (gen-answer-test 2 answer-2))))))

(defmacro given (part-number &body body)
  "Set's up a test for the examples given in the day's problem."
  `(5am:test (,(intern (format nil "GIVEN-~D" part-number))
              :suite ,(5am-day-suite))
     ,@(loop for form in body
             collect `(5am:is ,form))))

(defmacro deftest (name &body body)
  "Defines a test, leaving all details up to the caller."
  `(5am:test (,name :suite ,(5am-day-suite))
     ,@body))

;; Input and parsing

(defun read-lines-from-file (filename)
  (let ((lines (iterate
                 (for line in-file filename using #'read-line)
                 (collect line into lines)
                 (finally (return lines)))))
    (if (endp (cdr lines))
        (car lines)
        lines)))

(defun apply-parse-rule (parse-rule lines)
  (flet ((parse-line (line)
           (multiple-value-bind (result parsedp) (parseq:parseq parse-rule line)
             (unless parsedp
               (error (format nil "Failed to parse \"~A\" as ~A" line parse-rule)))
             result)))
    (if (consp lines)
        (mapcar #'parse-line lines)
        (parse-line lines))))

(defun fetch-input (day year)
  "Gets the input for the given day and year. You should probably use 'input' instead."
  (let* ((session-cookie (make-instance 'drakma:cookie
                                        :name "session"
                                        :domain "adventofcode.com"
                                        :value (get-config-key *config-key-session*)))
         (cookie-jar (make-instance 'drakma:cookie-jar
                                    :cookies (list session-cookie)))
         (url (format nil "https://adventofcode.com/~A/day/~A/input" year day))
         (cache-file (merge-pathnames (format nil "input.~4,'0D-~2,'0D" year day)
                                      (get-cachedir))))
    (unless (probe-file cache-file)
      (ensure-directories-exist (get-cachedir))
      (multiple-value-bind (data return-code)
          (drakma:http-request url
                               :cookie-jar cookie-jar
                               :user-agent *user-agent-string*)
        (assert (= 200 return-code)
                (day year)
                "Error code when fetching data for ~4,'0D-12-~2,'0D: ~A" year day data)
        (with-open-file (cache cache-file :direction :output)
          (write-line data cache))))
    cache-file))

(defun input (&key year day parse-line)
  "Fetches and returns for a problem. Call with no parameters to
fetch the Advent of Code input associated with the current package.

If PARSE-LINE is not provided, the data will be split at any newlines
and returned as a list of strings. If there are no newlines, a single
string will be returned (not a list).

If PARSE-LINE is provided, it should be a symbol for a parseq rule. The
input will be split at newlines and the parsing rule will be applied
separately to each line. If any line fails to parse, an error will be
signaled."

  (let ((real-day (or day (get-current-day)))
        (real-year (or year (get-current-year))))
    (let ((input-file
            (cond
              (t (fetch-input real-day real-year)))))
      (cond
        (parse-line
         (apply-parse-rule parse-line (read-lines-from-file input-file)))
        (t
         (read-lines-from-file input-file))))))

(defun submit (part answer &key day year)
  "Submits the answer for the given year, day, and part.  (DAY and YEAR
  are taken from the current package if not specified)."
  (let* ((real-day  (or day  (get-current-day)))
         (real-year (or year (get-current-year)))
         (url (format nil "https://adventofcode.com/~A/day/~A/answer" real-year real-day))
         (session-cookie (make-instance 'drakma:cookie
                                        :name "session"
                                        :domain "adventofcode.com"
                                        :value (get-config-key *config-key-session*)))
         (cookie-jar (make-instance 'drakma:cookie-jar
                                    :cookies (list session-cookie))))
    (multiple-value-bind (data return-code)
        (drakma:http-request
         url
         :method :post
         :parameters (list (cons "level" (format nil "~A" part))
                           (cons "answer" (format nil "~A" answer)))
         :cookie-jar cookie-jar
         :user-agent *user-agent-string*)
      (assert (= 200 return-code)
              (real-day real-year part)
              "Error code when submitting answer for ~4,'0D-12-~2,'0D, part ~A: ~A"
              real-year real-day part data)
      (let ((docroot (lquery:$ (initialize data))))
        (lquery:$1 docroot "article" (render-text))))))

(parseq:defrule integer-string ()
    (and (? "-") (+ digit))
  (:string)
  (:function #'parse-integer))

;;; Handles patterns of the following forms:
;;;
;;;  * a
;;;  * a and b
;;;  * a and b and c
;;;  * a, b, and c
;;;  * a, b, c
;;;  * a, b and c
;;;
(parseq:defrule comma-list (subrule)
    (and (* (and subrule (or ", and " ", " "," " and "))) subrule)
  (:destructure (beginning last)
                (nconc (mapcar #'first beginning) (list last))))

(5am:test (comma-list :suite :aoc)
  (let ((pairs '(((1) "1")
                 ((1 2) "1 and 2")
                 ((1 2 3) "1 and 2 and 3")
                 ((1 2 3) "1, 2, and 3")
                 ((1 2 3) "1, 2, 3")
                 ((1 2 3) "1,2,3")
                 ((1 2 3) "1, 2 and 3"))))
    (iter (for (result input) in pairs)
      (5am:is (eql result (parseq:parseq '(comma-list integer-string) input))))))
