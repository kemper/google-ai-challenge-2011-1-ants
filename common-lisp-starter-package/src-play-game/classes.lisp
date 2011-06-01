;;;; classes.lisp
;;;;
;;;; Almost all slots are initialized to NIL.  The actual
;;;; default values are set in PROCESS-COMMAND-LINE-OPTIONS
;;;; and defined in DEFSYNOPSIS.

(in-package :play-game)


;;; Classes

(defclass tile ()
  ((row :reader row :initarg :row)
   (col :reader col :initarg :col)))


(defclass land (tile) ())


(defclass food (tile)
  ((start-turn :reader start-turn :initarg :start-turn :initform 0)
   (conversion-turn :reader conversion-turn :initform 0)))


(defclass water (tile)
  ((seen-by :reader seen-by :initarg :seen-by)))


(defclass ant (food)
  ((initial-row :reader initial-row :initarg :initial-row)
   (initial-col :reader initial-col :initarg :initial-col)
   (end-turn :reader end-turn :initform 0)
   (dead :reader dead :initform nil)
   (player-id :reader pid :initarg :pid)
   (orders :reader orders :initform (make-array 0 :fill-pointer 0))))


(let ((counter -1))
  (flet ((unique-bot-id () (incf counter)))
    (defclass bot ()
      ((bot-id :reader bot-id :initform (unique-bot-id))
       (status :reader status :initform "survived")
       (command-line :reader command-line :initarg :command-line :initform nil)
       (process :reader process :initarg :process :initform nil)
       (ants :reader ants :initform nil)
       (dead-ants :reader dead-ants :initform nil)
       (scores :reader scores
               :initform (make-array 1 :element-type 'fixnum :fill-pointer 1
                                       ;:initial-element 0))))))
                                       :initial-element 1))))))


(defclass state ()
  ((input :reader input :initarg :input :initform *standard-input*)
   (output :reader output :initarg :output :initform *standard-output*)
   (error-stream :reader error-stream :initarg :error-stream
                 :initform *error-output*)
   (log-stream :reader log-stream :initform nil)  ; TODO? *debug-io*
   (turn :reader turn :initform nil)
   (turn-start-time :reader turn-start-time :initform nil)
   (attack-radius2  :reader attack-radius2 :initform nil)
   (load-time :reader load-time :initform nil)
   (spawn-radius2 :reader spawn-radius2 :initform nil)
   (turn-time :reader turn-time :initform nil)
   (turns :reader turns :initform nil)
   (view-radius2 :reader view-radius2 :initform nil)
   (rows :reader rows :initform nil)
   (cols :reader cols :initform nil)
   (game-map :reader game-map :initform nil)
   ;; TODO move enemy-ants and my-ants to a subclass in :ants-bot
   (enemy-ants :reader enemy-ants :initform nil)
   (my-ants :reader my-ants :initform nil)
   (food :reader food :initform nil)))


(defclass play-game-state (state)
  ((log-stream :reader log-stream :initform *debug-io*)
   (bots :reader bots :initform (make-array 0 :fill-pointer 0))
   (end-wait :reader end-wait :initform nil)
   (food-method :reader food-method :initform nil)
   (map-file :reader map-file :initform nil)
   (n-players :reader n-players :initarg :n-players :initform nil)
   (orders :accessor orders :initarg :orders :initform nil)
   (replay-dir :reader replay-dir :initform nil)
   (rounds :reader rounds :initform nil)))
