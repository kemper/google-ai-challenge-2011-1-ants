;;;; specials.lisp

(in-package :ants-common)


;;; Both shadowed in MAIN, never assign to directly!

(defvar *state* nil)
(defvar *verbose* nil)


;;; Constants

;; See classes.lisp
;(defvar +land+ (make-instance 'land))

(defvar 2pi (* 2 pi))
