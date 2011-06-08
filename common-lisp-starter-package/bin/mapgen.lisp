#!/usr/bin/sbcl --script
;;;;
;;;; mapgen.lisp

;;; Packages

(require :asdf)
(setf asdf:*central-registry* '("/usr/local/pub/ekwis/software/black-tie/"))
(asdf:oos 'asdf:load-op :black-tie)
(use-package :black-tie)


;;; Constants & Variables

(defvar +min-rows+ 16)
(defvar +max-rows+ 32)

(defvar +min-cols+ 32)
(defvar +max-cols+ 78)

(defvar +min-players+ 2)
(defvar +max-players+ 8)


(defparameter *rows* nil)
(defparameter *cols* nil)
(defparameter *players* nil)


;;; Functions

;; TODO needs a check for islands
(defun make-water (map)
  (let* ((random-z (random most-positive-single-float))
         (scale (- 1.0 (random 0.9)))
         (treshold (+ 0.2 (random 0.13)))  ; between 0.2 and 0.33
         (dim (array-dimensions map))
         (rows (elt dim 0))
         (cols (elt dim 1)))
    (loop for row from 0 below rows
          do (loop for col from 0 below cols
                   for pn = (perlin-noise (* row scale) (* col scale) random-z)
                   do (setf (aref map row col)
                            (if (> pn treshold)
                                #\%
                                #\.))))))


(defun players-characters (players)
  (loop with base = (char-code #\a)
        with vec = (make-array players :fill-pointer 0)
        for i from base below (+ base players)
        do (vector-push-extend (code-char i) vec)
        finally (return vec)))


(defun place-players (map players)
  (let* ((dim (array-dimensions map))
         (rows (elt dim 0))
         (cols (elt dim 1)))
    (loop for c across (players-characters players)
          do (loop with spot-found = nil
                   until spot-found
                   for rrow = (random rows)
                   for rcol = (random cols)
                   do (when (char= #\. (aref map rrow rcol))
                        (setf (aref map rrow rcol) c
                              spot-found t))))))


(defun write-map-file (file-name map players)
  (let* ((dim (array-dimensions map))
         (rows (elt dim 0))
         (cols (elt dim 1)))
    (with-open-file (f file-name :direction :output :if-exists :supersede)
      (format f "rows ~D~%cols ~D~%players ~D~%" rows cols players)
      (loop for row from 0 below rows
            do (princ "m " f)
               (loop for col from 0 below cols
                     do (princ (aref map row col) f))
               (terpri f)))))


(defun wrapped-row-col (row col)
  (vector (cond ((< row 0) (+ *rows* row))  ; adding negative number
                ((>= row *rows*) (- row *rows*))
                (t row))
          (cond ((< col 0) (+ *cols* col))  ; adding negative number
                ((>= col *cols*) (- col *cols*))
                (t col))))


;;; Main Program

(defun main ()
  (let* ((*random-state* (make-random-state t))
         (map nil))
    (setf *rows* (+ +min-rows+ (random (- +max-rows+ +min-rows+)))
          *cols* (+ +min-cols+ (random (- +max-cols+ +min-cols+)))
          *players* (+ +min-players+ (random (- +max-players+ +min-players+)))
          map (make-array (list *rows* *cols*) :element-type 'character
                          :initial-element #\.))
    (make-water map)
    (place-players map *players*)
    (write-map-file "tmp.map" map *players*)))


(main)
