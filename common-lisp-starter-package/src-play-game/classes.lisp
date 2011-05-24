;;;; classes.lisp

(in-package :play-game)


;;; Classes

(defclass tile ()
  ((row :reader row :initarg :row)
   (col :reader col :initarg :col)))

(defclass land (tile) ())
(defclass water (tile) ())


(defclass food (tile)
  ((start-turn :reader start-turn :initarg :start-turn)
   (conversion-turn :reader conversion-turn :initform nil)))


(defclass ant (food)
  ((initial-row :reader initial-row :initarg :initial-row)
   (initial-col :reader initial-col :initarg :initial-col)
   (end-turn :reader end-turn :initform nil)
   (dead :reader dead :initform nil)
   (player-id :reader pid :initarg :pid)
   (orders :reader orders
           :initform (make-array 0 :element-type 'character :fill-pointer 0))))


(defclass play-game-state (state)
  ((log-stream :reader log-stream :initform *debug-io*)
   (ants :reader ants :initform nil)
   (bots :reader bots :initarg :bots :initform nil)
   (map-file :reader map-file :initform nil)
   (orders :accessor orders :initarg :orders :initform nil)
   (players :reader players :initarg :players :initform nil)
   (procs :reader procs :initarg :procs :initform nil)
   (scores :reader scores :initarg :scores :initform nil)))
