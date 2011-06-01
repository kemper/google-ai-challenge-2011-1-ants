;;;; package.lisp

(in-package :cl-user)

(defpackage :play-game
  (:use :cl :ants-common :com.dvlsoft.clon :parse-number :split-sequence)
  (:shadow :getopt))
