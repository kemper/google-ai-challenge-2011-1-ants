;;;; game-state.lisp

(in-package :ants-bot)


;;; Functions

(defun parse-game-parameters ()
  "Parses turn 0 game parameters and sets them in *STATE*.  Also creates
  initial game map and assigns it to (GAME-MAP *STATE*)."
  (loop for line = (read-line (input *state*) nil)
        until (starts-with line "ready")
        do (cond ((starts-with line "attackradius2 ")
                  (setf (slot-value *state* 'attack-radius2) (par-value line)))
                 ((starts-with line "cols ")
                  (setf (slot-value *state* 'cols) (par-value line)))
                 ((starts-with line "loadtime ")
                  (setf (slot-value *state* 'load-time)
                        (/ (par-value line) 1000.0)))
                 ((starts-with line "rows ")
                  (setf (slot-value *state* 'rows) (par-value line)))
                 ((starts-with line "spawnradius2 ")
                  (setf (slot-value *state* 'spawn-radius2) (par-value line)))
                 ((starts-with line "turns ")
                  (setf (slot-value *state* 'turns) (par-value line)))
                 ((starts-with line "turntime ")
                  (setf (slot-value *state* 'turn-time)
                        (/ (par-value line) 1000.0)))
                 ((starts-with line "viewradius2 ")
                  (setf (slot-value *state* 'view-radius2) (par-value line)))))
  (setf (slot-value *state* 'game-map)
        (make-array (list (rows *state*) (cols *state*))
                    :initial-element +land+)))


(defun parse-game-state ()
  "Calls either PARSE-TURN or PARSE-GAME-PARAMETERS depending on the line
  on standard input.  Modifies *STATE* and returns T if the game has ended,
  otherwise NIL."
  (setf (slot-value *state* 'turn-start-time) (wall-time))
  (reset-some-state)
  (loop for line = (read-line (input *state*) nil)
        until (> (length line) 0)  ; TODO needs clarification
        finally (return (cond ((starts-with line "end")
                               (parse-turn)
                               t)
                              ((starts-with line "turn 0")
                               (setf (slot-value *state* 'turn) 0)
                               (parse-game-parameters)
                               nil)
                              ((starts-with line "turn ")
                               (setf (slot-value *state* 'turn)
                                     (par-value line))
                               (parse-turn)
                               nil)))))


(defun parse-turn ()
  "Parses a typical turn.  Modifies *STATE* indirectly through RESET-GAME-MAP
  and the SET-* functions."
  (reset-game-map)
  (loop for line = (read-line (input *state*) nil)
        until (starts-with line "go")
        do (cond ((starts-with line "f ") (set-food line))
                 ((starts-with line "w ") (set-water line))
                 ((starts-with line "a ") (set-ant line))
                 ((starts-with line "d ") (set-dead line)))))


(defun reset-game-map ()
  "Sets all tiles on the map to land if they're not already land or water.
  Modifies (GAME-MAP *STATE*)."
  (loop with dim = (array-dimensions (game-map *state*))
        for row from 0 below (first dim)
        do (loop for col from 0 below (second dim)
                 for tile-type = (type-of (tile-at row col))
                 unless (or (landp (tile-at row col))
                            (waterp (tile-at row col)))
                   do (setf (tile-at row col) +land+))))


(defun reset-some-state ()
  "Sets (ENEMY-ANTS *STATE*), (MY-ANTS *STATE*) and (FOOD *STATE*) to NIL."
  (setf (enemy-ants *state*) nil
        (my-ants *state*)    nil
        (food *state*)       nil))


(defun set-ant (string)
  "Parses the \"a row col owner\" STRING and sets the specific map tile to
  an ant of owner.  Modifies (ENEMY-ANTS *STATE*), (MY-ANTS *STATE*) and
  (GAME-MAP *STATE*)."
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2)))
         (owner (parse-integer (elt split 3)))
         (ant (make-instance 'ant :row row :col col :pid owner)))
    (if (= owner 0)
        (push ant (my-ants *state*))
        (push ant (enemy-ants *state*)))
    (setf (tile-at row col) ant)))


;; TODO check if the ant's already at row,col and set it to dead if so
;; TODO merge with set-ant
(defun set-dead (string)
  "Parses the \"d row col owner\" STRING and sets the specific map tile to
  a dead ant of owner.  Modifies (GAME-MAP *STATE*)."
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2)))
         (owner (parse-integer (elt split 3))))
    (unless (foodp (tile-at row col))
      (setf (tile-at row col)
            (make-instance 'ant :row row :col col :pid owner)))))


(defun set-food (string)
  "Parses the \"f row col\" STRING and sets the specific map tile to food.
  Modifies (FOOD *STATE*) and (GAME-MAP *STATE*)."
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2))))
    (let ((food (make-instance 'food :row row :col col
                               :start-turn (turn *state*))))
      (push food (food *state*))
      (setf (tile-at row col) food))))


(defun set-water (string)
  "Parses the \"w row col\" STRING and sets the specific map tile to water.
  Modifies (GAME-MAP *STATE*)."
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2))))
    (setf (tile-at row col) (make-instance 'water :row row :col col))))


(defun split-state-string (string)
  (loop with result = nil
        with value = nil
        for c across string
        when (and (char= c #\space) value)
          do (push (coerce (nreverse value) 'string) result)
             (setf value nil)
        when (char/= c #\space)
          do (push c value)
        finally (when value
                  (push (coerce (nreverse value) 'string) result))
                (return (nreverse result))))
