;;;; classes.lisp

(in-package :ants-bot)


;;; Classes

(defclass ants-bot-state (state)
  ((enemy-ants :reader enemy-ants :initform nil)
   (my-ants :reader my-ants :initform nil)))
