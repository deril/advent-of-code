(in-package :aoc-2024-15)

(aoc:define-day 1430536 1452348) ;; Part 2 is ugly. Need refactoring

;;; Input

(defparameter *input* (aoc:input))

(defparameter *example1*
  '("########"
    "#..O.O.#"
    "##@.O..#"
    "#...O..#"
    "#.#.O..#"
    "#...O..#"
    "#......#"
    "########"
    ""
    "<^^>>>vv<v>>v<<"))

(defparameter *example2*
  '("##########"
    "#..O..O.O#"
    "#......O.#"
    "#.OO..O.O#"
    "#..O@..O.#"
    "#O#..O...#"
    "#O..O..O.#"
    "#.OO.O.OO#"
    "#....O...#"
    "##########"
    ""
    "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^"
    "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v"
    "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<"
    "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^"
    "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><"
    "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^"
    ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^"
    "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>"
    "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>"
    "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"))

(defparameter *example3*
  '("#######"
    "#...#.#"
    "#.....#"
    "#.@OO.#"
    "#..O..#"
    "#.....#"
    "#######"
    ""
    "<vv<<^^<<^^"))

;;; Part 1

(defconstant +robot+ #\@)
(defconstant +wall+ #\#)
(defconstant +box+ #\O)
(defconstant +empty+ #\.)
(defconstant +box-left+ #\[)
(defconstant +box-right+ #\])

(defun split-input (input)
  (let ((split (position "" input :test #'string=)))
    (values (subseq input 0 split)
            (subseq input (1+ split)))))

(defun find-initial-position (map)
  (iter
    (for i from 0 below (array-dimension map 0))
    (iter
      (for j from 0 below (array-dimension map 1))
      (when (char= (aref map i j) +robot+)
        (setf (aref map i j) +empty+)
        (return-from find-initial-position (complex j i))))))

(defun instruction-to-direction (instruction)
  (ecase instruction
    (#\^ #C(0 -1))
    (#\v #C(0 1))
    (#\> #C(1 0))
    (#\< #C(-1 0))))

(defun next-valid-position (position direction map)
  (let ((next-position (+ position direction)))
    (case (aoc:cref map next-position)
      (#.+wall+ nil)
      (#.+empty+ next-position)
      (otherwise (next-valid-position next-position direction map)))))

(defun move-box (box-position direction map)
  (let ((next-box-position (next-valid-position box-position direction map)))
    (if next-box-position
        (progn
          (setf (aoc:cref map next-box-position) +box+
                (aoc:cref map (+ box-position direction)) +empty+)
          (+ box-position direction))
        box-position)))

(defun collect-big-boxes (start-pos map direction)
  (let ((connected-boxes '())
        (boxes (fset:empty-seq)))
    (case (aoc:cref map start-pos)
      (#.+box-left+ (fset:push-last boxes (list start-pos (+ start-pos #C(1 0)))))
      (#.+box-right+ (fset:push-last boxes (list (+ start-pos #C(-1 0)) start-pos))))
    (do ((visited (fset:empty-set))
         (pos start-pos))
        ((fset:empty? boxes) connected-boxes)
      (let ((current-box (fset:pop-first boxes)))
        (unless (fset:contains? visited current-box)
          (fset:includef visited current-box)
          (iter
            (for coord in (if (= direction -1) (reverse current-box) current-box))
            (push coord connected-boxes)
            (setq pos (+ coord direction))
            (for next-box = (case (aoc:cref map pos)
                              (#.+wall+ (return-from collect-big-boxes nil))
                              (#.+box-left+ (list pos (+ pos #C(1 0))))
                              (#.+box-right+ (list (+ pos #C(-1 0)) pos))))
            (when (and next-box (not (fset:contains? visited next-box)))
              (fset:push-last boxes next-box))))))))

(defun move-big-box (position map dir)
  (let* ((new-position (+ position dir))
         (connected-boxes (collect-big-boxes new-position map dir)))
    (dolist (coord connected-boxes)
      (setf (aoc:cref map (+ coord dir)) (aoc:cref map coord)
            (aoc:cref map coord) +empty+))
    (if connected-boxes new-position position)))

(defun follow-instruction (instruction position map)
  (let* ((dir (instruction-to-direction instruction))
         (new-position (+ position dir)))
    (case (aoc:cref map new-position)
      (#.+wall+ position)
      (#.+empty+ new-position)
      (#.+box+ (move-box position dir map))
      ((#.+box-left+ #.+box-right+) (move-big-box position map dir)))))

(defun follow-instructions (instructions map)
  (reduce #'(lambda (position instruction) (follow-instruction instruction position map))
          instructions
          :initial-value (find-initial-position map)))

(defun calc-boxes-gps (map)
  (iter outer
    (for i from 0 below (array-dimension map 0))
    (iter
      (for j from 0 below (array-dimension map 1))
      (when (or (char= (aref map i j) +box+)
                (char= (aref map i j) +box-left+))
        (in outer (summing (+ (* 100 i) j)))))))

(defun get-answer-1 (&optional (input *input*))
  (multiple-value-bind (map movements) (split-input input)
    (let ((map (aoc:parse-grid-to-array map))
          (instructions (format nil "~{~A~^~}" movements)))
      (follow-instructions instructions map)
      (calc-boxes-gps map))))

(aoc:given 1
  (= 2028 (get-answer-1 *example1*))
  (= 10092 (get-answer-1 *example2*)))

;;; Part 2

(defun expand-line (line)
  (with-output-to-string (output)
    (loop for char across line
          do (case char
               (#.+wall+ (write-string "##" output))
               (#.+box+ (write-string "[]" output))
               (#.+empty+ (write-string ".." output))
               (#.+robot+ (write-string "@." output))))))

(defun expand-map (input)
  (mapcar #'expand-line input))

(defun get-answer-2 (&optional (input *input*))
  (multiple-value-bind (map movements) (split-input input)
    (let ((map (aoc:parse-grid-to-array (expand-map map)))
          (instructions (format nil "~{~A~^~}" movements)))
      (follow-instructions instructions map)
      (calc-boxes-gps map))))
