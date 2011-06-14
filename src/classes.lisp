;;;; classes.lisp

(in-package :ants-bot)


;;; Classes

(defclass ants-bot-state (state)
  ((enemy-ants :accessor enemy-ants :initform nil)
   (my-ants :accessor my-ants :initform nil)))
