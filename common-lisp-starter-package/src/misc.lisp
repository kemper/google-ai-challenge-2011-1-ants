;;;; misc.lisp

(in-package :ants-bot)


;;; Functions

;; TODO put send-go in common and call that
(defun finish-turn ()
  "Prints the \"finish turn\" string to standard output."
  (format (output *state*) "~&go~%")
  (force-output (output *state*)))


(defun issue-order (row col direction)
  "Prints a formatted order for ROW,COL and DIRECTION to standard output."
  (if (not (member direction '(:north :east :south :west)))
      (errmsg "[issue-order] Illegal direction: " direction)
      (format (output *state*) "~&o ~D ~D ~A~%" row col
              (case direction
                (:north "N")
                (:east  "E")
                (:south "S")
                (:west  "W")))))


(defun turn-time-remaining ()
  "Returns the turn time remaining in seconds (as a FLOAT)."
  (- (+ (turn-start-time *state*) (turn-time *state*))
     (wall-time)))


(defun turn-time-used ()
  (- (wall-time) (turn-start-time *state*)))
