;;;; classes.lisp

(in-package :ants-common)


;;; Classes

(defclass tile ()
  ((row :reader row :initarg :row)
   (col :reader col :initarg :col)))


(defclass land (tile) ())
(defvar +land+ (make-instance 'land))


(defclass food (tile)
  ((start-turn :reader start-turn :initarg :start-turn :initform 0)
   (conversion-turn :reader conversion-turn :initform 0)))


(defclass ant (food)
  ((initial-row :reader initial-row :initarg :initial-row)
   (initial-col :reader initial-col :initarg :initial-col)
   (end-turn :reader end-turn :initform 0)
   (dead :reader dead :initform nil)
   (player-id :reader pid :initarg :pid)
   (orders :reader orders :initform (make-array 0 :fill-pointer 0))))


(defclass water (tile)
  ((seen-by :reader seen-by :initarg :seen-by)))


(defclass state ()
  ((rows :reader rows :initform nil)
   (cols :reader cols :initform nil)
   (game-map :reader game-map :initform nil)
   (turn :reader turn :initform nil)
   (turns :reader turns :initform nil)
   (turn-start-time :reader turn-start-time :initform nil)
   (turn-time :reader turn-time :initform nil)
   (load-time :reader load-time :initform nil)
   (food :reader food :initform nil)
   (attack-radius2  :reader attack-radius2 :initform nil)
   (spawn-radius2 :reader spawn-radius2 :initform nil)
   (view-radius2 :reader view-radius2 :initform nil)
   (input :reader input :initarg :input :initform *standard-input*)
   (output :reader output :initarg :output :initform *standard-output*)
   (error-stream :reader error-stream :initarg :error-stream
                 :initform *error-output*)
   (log-stream :reader log-stream :initform nil)))  ; TODO? *debug-io*
