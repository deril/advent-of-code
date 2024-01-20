(in-package :aoc-2015-11)

(aoc:define-day "hxbxxyzz" "hxcaabcc")

;; Input

(defparameter *input* (aoc:input))

;; Part 1

(defun increment (password)
  (loop for i from (1- (length password)) downto 0
        for c = (char password i)
        do (if (char= c #\z)
               (setf (char password i) #\a)
               (progn
                 (setf (char password i) (code-char (1+ (char-code c))))
                 (return password)))))

(defun increasing-straight-p (password)
  (loop for i from 0 to (- (length password) 3)
        for c1 = (char password i)
        for c2 = (char password (1+ i))
        for c3 = (char password (+ 2 i))
        when (and (char= c1 (code-char (1- (char-code c2))))
                  (char= c2 (code-char (1- (char-code c3)))))
          return t))

(defun no-ambiguous-chars-p (password)
  (not (find-if (lambda (c) (find c "iol"))
                password)))

(let ((s (cl-ppcre:create-scanner "(.)\\1.*?(.)\\2")))
  (defun two-pairs-p (password)
    (cl-ppcre:scan s password)))

(defun valid-password-p (password)
  (and (increasing-straight-p password)
       (no-ambiguous-chars-p password)
       (two-pairs-p password)))

(defun next-valid-password (current-password)
  (iter (for password = (increment current-password))
    (until (valid-password-p password))
    (finally (return password))))

(defun get-answer-1 (&optional (current-password *input*))
  (next-valid-password (copy-seq current-password)))

;; Part 2

(defun get-answer-2 (&optional (current-password *input*))
  (iter (repeat 2)
    (for password initially  (copy-seq current-password) then (next-valid-password password))
    (finally (return password))))
