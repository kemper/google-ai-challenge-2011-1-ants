;;;; predicates.lisp

(in-package :ants-common)


;;; Functions

(defun alivep (tile)
  (not (dead tile)))


(defun antp (tile)
  (typep tile 'ant))


(defun enemyp (ant)
  (> (pid ant) 0))


(defun foodp (tile)
  (and (not (antp tile))
       (typep tile 'food)))


(defun friendlyp (ant)
  (= 0 (pid ant)))


(defun landp (tile)
  (typep tile 'land))


(defun waterp (tile)
  (typep tile 'water))
