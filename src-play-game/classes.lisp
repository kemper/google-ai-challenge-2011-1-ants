;;;; classes.lisp
;;;;
;;;; Almost all slots are initialized to NIL.  The actual
;;;; default values are set in PROCESS-COMMAND-LINE-OPTIONS
;;;; and defined in DEFSYNOPSIS.
;;;;
;;;; TODO use structs: much faster

(in-package :play-game)


;;; Classes

(let ((counter -1))
  (flet ((unique-bot-id () (incf counter)))
    (defclass bot ()
      ((bot-id :reader bot-id :initform (unique-bot-id))
       (status :accessor status :initform "survived")
       (command-line :reader command-line :initarg :command-line :initform nil)
       (process :reader process :initarg :process :initform nil)
       (string-output-stream :reader string-output-stream
                             :initform (make-string-output-stream))
       (thread-status :accessor thread-status :initform :wait)
       (orders :accessor orders :initform nil)
       (ants :reader ants :initform nil)
       (dead-ants :reader dead-ants :initform nil)
       ; TODO add removed-food
       (scores :reader scores
               :initform (make-array 1 :element-type 'float :fill-pointer 1
                                       :initial-element 0))))))


(defstruct order
  (bot-id 0 :type fixnum)
  (direction :none :type symbol)
  (src-row 0 :type fixnum)
  (src-col 0 :type fixnum)
  (dst-row 0 :type fixnum)
  (dst-col 0 :type fixnum))


(defclass play-game-state (state)
  ((orders :accessor orders :initarg :orders :initform nil)
   (bots :reader bots :initform (make-array 0 :fill-pointer 0))
   (food :accessor food :initform nil)
   (contested-food :accessor contested-food :initform nil)
   (n-players :reader n-players :initarg :n-players :initform nil)
   (map-file :reader map-file :initform nil)
   (rounds :reader rounds :initform nil)
   (end-wait :reader end-wait :initform nil)
   (food-method :reader food-method :initform nil)
   (replay-dir :reader replay-dir :initform nil)
   (log-stream :reader log-stream :initform *debug-io*)))
