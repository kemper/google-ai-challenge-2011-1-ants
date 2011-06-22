;;;; test-objects.lisp

(in-package :ants-bot)


;;; State

(setf *state* (make-instance 'ants-bot-state :output nil))


;;; Turns

;; In case you don't see them: there's some trailing spaces.
(defvar *turn-0* "turn 0
loadtime 2500
turntime 2000 
rows  15
cols  20 
turns 500  
viewradius2   93
attackradius2   6  
spawnradius2 6
ready
")

;;   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9
;; 0 . % % % % . . . . . . . . . . . . . . .
;; 1 . . % % . * . . . . . . . . . . . . . .
;; 2 . * % . . . b . . . . . . . . . . . . .
;; 3 . . . . a b . . . . . . . . . . . . . .
;; 4 . . . . . . . . . . . . . . . . . . . .
;; 5 . . . . . . . . . . . . . . . . . . . .
;; 6 . . . . . . . . . . . . . . . . . . . .
;; 7 . . . . . . . . . . . . . . . . . . . .
;; 8 . . . . . . . . . . . . . . . % % . . .
;; 9 . . . . . . . . . . . . . . . % % . . .
;; 0 . . . . . . . . . . . . . . . % % % . .
;; 1 . . . . . . . . . . . . . . . . % % . .
;; 2 . . . . . . . . . . . . . . . . % . . .
;; 3 . . . . . . . . . . . . . . . . . . . .
;; 4 . . . . . . . . . . . . . . . . . . . .
(defvar *turn-1* "turn 1
f 1 5
f 2 1
w 0 1
w 0 2
w 0 3
w 0 4
w 1 2
w 1 3
w 2 2
w 8 15
w 8 16
w 9 15
w 9 16
w 10 15
w 10 16
w 10 17
w 11 16
w 11 17
w 12 16
a 2 6 1
a 3 4 0
a 3 5 1
go
")
