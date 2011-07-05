;;;; classes.lisp

(in-package :ants-common)


;;; Classes

(defclass tile ()
  ((row :reader row :initarg :row)
   (col :reader col :initarg :col)))


(defclass land (tile) ())
(defvar +land+ (make-instance 'land))


(defclass food (tile)
  (;; needed in case the food is contested and destroyed
   (conversion-turn :accessor conversion-turn :initform 0
                    :initarg :conversion-turn)
   (start-turn :reader start-turn :initarg :start-turn :initform 0)))


(defclass ant (food)
  ((initial-row :reader initial-row :initarg :initial-row)
   (initial-col :reader initial-col :initarg :initial-col)
   (end-turn :reader end-turn :initform 0)
   (hit-points :accessor hp :initform 1)
   (dead :reader dead :initform nil)
   (player-id :reader pid :initarg :pid)
   (orders :reader orders :initform (make-array 0 :fill-pointer 0))))


(defclass water (tile)
  ;((seen-by :reader seen-by :type simple-vector :initarg :seen-by)))
  ((seen-by :reader seen-by :initarg :seen-by)))


(defclass state ()
  ((rows :reader rows :initform nil)
   (cols :reader cols :initform nil)
   (game-map :reader game-map :initform nil)
   (turn :accessor turn :initform 0)
   (turns :reader turns :initform nil)
   (turn-start-time :reader turn-start-time :initform nil)
   (turn-time :reader turn-time :initform nil)
   (load-time :reader load-time :initform nil)
   (food :accessor food :initform nil)
   (attack-radius2  :reader attack-radius2 :initform nil)
   (spawn-radius2 :reader spawn-radius2 :initform nil)
   (view-radius2 :reader view-radius2 :initform nil)
   (input :reader input :initarg :input :initform *standard-input*)
   (output :reader output :initarg :output :initform *standard-output*)
   (error-stream :reader error-stream :initarg :error-stream
                 :initform *error-output*)
   (log-stream :reader log-stream :initform nil)))  ; TODO? *debug-io*


;;; PRINT-OBJECT Methods

(defmethod print-object ((obj ant) stream)
  (format stream "<A ~D:~D,~D>" (pid obj) (row obj) (col obj)))


(defmethod print-object ((obj food) stream)
  (format stream "<F ~D,~D>" (row obj) (col obj)))
