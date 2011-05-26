;;;; classes.lisp

(in-package :play-game)


;;; Classes

(defclass tile ()
  ((row :reader row :initarg :row)
   (col :reader col :initarg :col)))

(defclass land (tile) ())
(defclass water (tile) ())


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


(let ((counter 0))
  (flet ((unique-bot-id ()
           (- (incf counter) 1)))
    (defclass bot ()
      ((bot-id :reader bid :initform (unique-bot-id))
       (status :reader status :initform "survived")
       (command-line :reader command-line :initarg :command-line :initform nil)
       (process :reader process :initarg :process :initform nil)
       (ants :reader ants :initform nil)
       (scores :reader scores
               :initform (make-array 1 :fill-pointer 0 :element-type 'fixnum
                                       :initial-element 0))))))


(defclass state ()
  ((input :reader input :initarg :input :initform *standard-input*)
   (output :reader output :initarg :output :initform *standard-output*)
   (error-stream :reader error-stream :initarg :error-stream
                 :initform *error-output*)
   (log-stream :reader log-stream :initform nil)  ; TODO? *debug-io*
   (turn :reader turn :initform nil)
   (turn-start-time :reader turn-start-time :initform nil)
   (attack-radius2  :reader attack-radius2 :initform 4)
   (load-time :reader load-time :initform 3000)
   (spawn-radius2 :reader spawn-radius2 :initform 1)
   (turn-time :reader turn-time :initform 1000)
   (turns :reader turns :initform 200)
   (view-radius2 :reader view-radius2 :initform 55)
   (rows :reader rows :initform nil)
   (cols :reader cols :initform nil)
   (game-map :reader game-map :initform nil)
   ;; TODO move enemy-ants and my-ants to a subclass in :ants-bot
   (enemy-ants :reader enemy-ants :initform nil)
   (my-ants :reader my-ants :initform nil)
   (food :reader food :initform nil)))


(defclass play-game-state (state)
  ((log-stream :reader log-stream :initform *debug-io*)
   (ants :reader ants :initform nil)
   (antz :accessor antz :initform nil)  ; yeah...
   (bots :reader bots :initarg :bots :initform nil)
   (map-file :reader map-file :initform nil)
   (orders :accessor orders :initarg :orders :initform nil)
   (n-players :reader n-players :initarg :n-players :initform nil)
   (procs :reader procs :initarg :procs :initform nil)
   (scores :reader scores :initarg :scores :initform nil)))
