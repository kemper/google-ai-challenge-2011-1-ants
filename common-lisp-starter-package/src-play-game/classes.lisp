;;;; classes.lisp
;;;;
;;;; Almost all slots are initialized to NIL.  The actual
;;;; default values are set in PROCESS-COMMAND-LINE-OPTIONS
;;;; and defined in DEFSYNOPSIS.

(in-package :play-game)


;;; Classes

(let ((counter -1))
  (flet ((unique-bot-id () (incf counter)))
    (defclass bot ()
      ((bot-id :reader bot-id :initform (unique-bot-id))
       (status :reader status :initform "survived")
       (command-line :reader command-line :initarg :command-line :initform nil)
       (process :reader process :initarg :process :initform nil)
       (ants :reader ants :initform nil)
       (dead-ants :reader dead-ants :initform nil)
       ; TODO add removed-food
       (scores :reader scores
               :initform (make-array 1 :element-type 'fixnum :fill-pointer 1
                                       ;:initial-element 0))))))
                                       :initial-element 1))))))


(defclass play-game-state (state)
  ((orders :accessor orders :initarg :orders :initform nil)
   (bots :reader bots :initform (make-array 0 :fill-pointer 0))
   (n-players :reader n-players :initarg :n-players :initform nil)
   (map-file :reader map-file :initform nil)
   (rounds :reader rounds :initform nil)
   (end-wait :reader end-wait :initform nil)
   (food-method :reader food-method :initform nil)
   (replay-dir :reader replay-dir :initform nil)
   (log-stream :reader log-stream :initform *debug-io*)))
