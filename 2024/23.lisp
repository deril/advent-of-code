(in-package :aoc-2024-23)

(aoc:define-day 926 "AZ,ED,HZ,IT,LD,NH,PC,TD,TY,UX,WC,YG,ZZ")

;; Parsing

(parseq:defrule computer ()
    (rep 2 alpha)
  (:string)
  (:lambda (c) (read-from-string c)))

(parseq:defrule connection ()
    (and computer "-" computer)
  (:choose 0 2))

;; Input

(defun make-connection-graph (connections)
  (let ((graph (make-hash-table :test #'eq)))
    (dolist (connection connections)
      (destructuring-bind (a b) connection
        (push b (gethash a graph))
        (push a (gethash b graph))))
    graph))

(defparameter *connections*
  (make-connection-graph
   (mapcar (alexandria:curry #'parseq:parseq 'connection)
           (aoc:input))))

(defparameter *example-connections*
  (make-connection-graph
   (mapcar (alexandria:curry #'parseq:parseq 'connection)
           '("kh-tc"
             "qp-kh"
             "de-cg"
             "ka-co"
             "yn-aq"
             "qp-ub"
             "cg-tb"
             "vc-aq"
             "tb-ka"
             "wh-tc"
             "yn-cg"
             "kh-ub"
             "ta-co"
             "de-co"
             "tc-td"
             "tb-wq"
             "wh-td"
             "ta-ka"
             "td-qp"
             "aq-cg"
             "wq-ub"
             "ub-vc"
             "de-ta"
             "wq-aq"
             "wq-vc"
             "wh-yn"
             "ka-de"
             "kh-ta"
             "co-tc"
             "wh-qp"
             "tb-vc"
             "td-yn"))))

;; Part 1

(defun cliques (graph &aux cliques)
  (labels ((bron-kerbosch (r p x)
             (if (and (null x) (null p))
                 (push r cliques)
                 (loop for v in p
                       collect
                       (let ((n (gethash v graph)))
                         (bron-kerbosch (union (list v) r)
                                        (intersection (set-difference p r) n)
                                        (intersection x n)))
                       do (setf p (remove v p)
                                x (union (list v) x))))))
    (bron-kerbosch nil (alexandria:hash-table-keys graph) nil))
  (mapcar #'(lambda (clique) (sort clique #'string<)) cliques))

(defun cliques-of-size (cliques size)
  (remove-duplicates
   (loop for clique in cliques
         append
         (cond ((= (length clique) size) `(,clique))
               ((> (length clique) size)
                (let ((collection nil))
                  (alexandria:map-combinations
                   #'(lambda (subset) (push subset collection)) clique
                   :length size)
                  collection))))
   :test #'equal))

(defun get-answer-1 (&optional (connections *connections*))
  (count-if #'(lambda (c)
                (some #'(lambda (e) (char= (char (string e) 0) #\T)) c))
            (cliques-of-size (cliques connections) 3)))

(aoc:given 1
  (= 7 (get-answer-1 *example-connections*)))

;; Part 2

(defun max-clique (cliques)
  (reduce #'(lambda (a b) (if (> (length a) (length b)) a b)) cliques))

(defun get-answer-2 (&optional (connections *connections*))
  (format nil "~{~A~^,~}" (max-clique (cliques connections))))

(aoc:given 2
  (equal "CO,DE,KA,TA" (get-answer-2 *example-connections*)))
