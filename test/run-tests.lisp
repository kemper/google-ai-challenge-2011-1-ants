;;;; run-tests.lisp

(require :asdf)

;; lisp-unit generates some style warnings when being compiled
(setf *muffled-warnings* 'style-warning)


;;; Packages

(format t "~&Loading lisp-unit...~%")
(load "test/lisp-unit")

(format t "Loading ant-bot...~%")
(load (merge-pathnames "3rd-party/asdf-init.lisp" *default-pathname-defaults*))
(asdf:oos 'asdf:load-op :ants-bot)

(in-package :ants-bot)
(use-package :lisp-unit)

(format t "Loading test objects...~%")
(load "test/test-objects")


;;; Tests

(define-test host2str
  (assert-equal "192.168.0.1" (host2str #(192 168 0 1)))
  (assert-equal "192.168.0.1" (host2str "192.168.0.1"))
  (assert-equalp #(192 168 0 1 0) (host2str #(192 168 0 1 0))))  ; meh...

(define-test issue-order
  (assert-equal (format nil "~&o 1 1 N~%") (issue-order 1 1 :north))
  (assert-equal (format nil "~&o 2 2 E~%") (issue-order 2 2 :east))
  (assert-equal (format nil "~&o 2 3 S~%") (issue-order 2 3 :south))
  (assert-equal (format nil "~&o 3 2 W~%") (issue-order 3 2 :west)))

(define-test last1
  (assert-equal nil (last1 '()))
  (assert-equal nil (last1 #()))
  (assert-equal 1 (last1 '(1)))
  (assert-equal 1 (last1 #(1)))
  (assert-equal 3 (last1 '(1 2 3)))
  (assert-equal 3 (last1 #(1 2 3))))

(define-test mkstr
  (assert-equal "abc1de23fG456HI" (mkstr "abc" 1 "de" 23 "f" 'g 456 'hi)))

(define-test par-value
  (assert-equal 1 (par-value "a 1"))
  (assert-equal 234 (par-value "bc 234"))
  ;; Ought to be an error but it isn't for now.  Added test so we know when
  ;; the implementation changes.
  (assert-equal 1 (par-value " 1"))
  (assert-error 'type-error (par-value "")))

;; TODO unSBCLificate and test for 'error?
#+sbcl (define-test par-value-sbcl
         (assert-error 'sb-int:simple-parse-error (par-value " "))
         (assert-error 'sb-int:simple-parse-error (par-value "a "))
         (assert-error 'sb-int:simple-parse-error (par-value "a 1.23"))
         (assert-error 'sb-int:simple-parse-error (par-value "a 1,23"))
         (assert-error 'sb-int:simple-parse-error (par-value "a b")))

(define-test split-state-string
  (assert-equal '("abc") (split-state-string "abc"))
  (assert-equal '("a" "bc") (split-state-string " a bc"))
  (assert-equal '("a" "b" "c") (split-state-string "a b c "))
  (assert-equal '("a") (split-state-string " a ")))

(define-test starts-with
  (assert-true (starts-with "abc" "a"))
  (assert-true (starts-with "abc" "ab"))
  (assert-true (starts-with "abc" "abc"))
  (assert-false (starts-with "abc" ""))
  (assert-false (starts-with "abc" "b"))
  (assert-false (starts-with "abc" "ac"))
  (assert-false (starts-with "abc" "abcd"))
  ;(assert-false (starts-with "" ""))  ; undefined
  (assert-false (starts-with "" "a"))
  (assert-false (starts-with "" "ab")))


;;; Manipulating *STATE*

(setf (slot-value *state* 'input) (make-string-input-stream turn-0))
(parse-game-state)

(define-test distance
  (assert-equal 0.0       (distance 0 0 0 0))
  (assert-equal 1.0       (distance 0 0 1 0))
  (assert-equal 1.0       (distance 0 0 0 1))
  (assert-equal 1.4142135 (distance 0 0 1 1))
  (assert-equal 1.0       (distance 0 0 0 6))
  (assert-equal 1.0       (distance 0 0 3 0))
  (assert-equal 1.4142135 (distance 0 0 3 6))
  (assert-equal 3.0       (distance 0 0 0 3))
  (assert-equal 3.0       (distance 0 0 0 4))
  (assert-equal 2.236068  (distance 0 0 1 2))
  (assert-equal 3.6055512 (distance 0 0 2 3))
  (assert-equal 3.6055512 (distance 0 0 2 4))
  (assert-equal 2.236068  (distance 0 0 3 5)))

(define-test distance2
  (assert-equal  0 (distance2 0 0 0 0))
  (assert-equal  1 (distance2 0 0 1 0))
  (assert-equal  1 (distance2 0 0 0 1))
  (assert-equal  2 (distance2 0 0 1 1))
  (assert-equal  1 (distance2 0 0 0 6))
  (assert-equal  1 (distance2 0 0 3 0))
  (assert-equal  2 (distance2 0 0 3 6))
  (assert-equal  9 (distance2 0 0 0 3))
  (assert-equal  9 (distance2 0 0 0 4))
  (assert-equal  5 (distance2 0 0 1 2))
  (assert-equal 13 (distance2 0 0 2 3))
  (assert-equal 13 (distance2 0 0 2 4))
  (assert-equal  5 (distance2 0 0 3 5)))

(define-test game-parameters
  (assert-equal 6 (attack-radius2 *state*))
  (assert-equal 7 (cols *state*))
  (assert-equal 2.5 (load-time *state*))
  (assert-equal 4 (rows *state*))
  (assert-equal 6 (spawn-radius2 *state*))
  ;; This is somewhat counter-intuitive, but RUN-TESTS is run later and in the
  ;; meantime (TURN *STATE*) has been further modified.
  (assert-equal 1 (turn *state*))
  (assert-equal 500 (turns *state*))
  (assert-equal 2.0 (turn-time *state*))
  (assert-equal 93 (view-radius2 *state*)))

(define-test new-location
  (assert-equalp #(1 3) (new-location 2 3 :north))
  (assert-equalp #(2 4) (new-location 2 3 :east))
  (assert-equalp #(3 3) (new-location 2 3 :south))
  (assert-equalp #(2 2) (new-location 2 3 :west))
  (assert-equalp #(3 0) (new-location 0 0 :north))
  (assert-equalp #(0 6) (new-location 0 0 :west))
  (assert-equalp #(3 0) (new-location 3 6 :east))
  (assert-equalp #(0 6) (new-location 3 6 :south)))

(define-test wrapped-row-col
  (assert-equalp #(2 3) (wrapped-row-col 2 3))
  (assert-equalp #(2 3) (wrapped-row-col 2 3))
  (assert-equalp #(2 3) (wrapped-row-col 2 3))
  (assert-equalp #(2 3) (wrapped-row-col 2 3))
  (assert-equalp #(3 0) (wrapped-row-col -1 0))
  (assert-equalp #(0 6) (wrapped-row-col 0 -1))
  (assert-equalp #(3 0) (wrapped-row-col 3 7))
  (assert-equalp #(0 6) (wrapped-row-col 4 6))
  (assert-equalp #(3 6) (wrapped-row-col -1 -1))
  (assert-equalp #(3 0) (wrapped-row-col -1  7))
  (assert-equalp #(0 6) (wrapped-row-col  4 -1))
  (assert-equalp #(0 0) (wrapped-row-col  4  7)))

(setf (slot-value *state* 'input) (make-string-input-stream turn-1))
(parse-game-state)

(define-test antp
  (assert-equal 2 (length (enemy-ants *state*)))
  (assert-equal 1 (length (my-ants *state*)))
  (assert-true (antp (tile-at 2 6)))
  (assert-true (antp (tile-at 3 4)))
  (assert-true (antp (tile-at 3 5)))
  (assert-false (antp (tile-at 0 0)))
  (assert-false (antp (tile-at 0 1)))
  (assert-false (antp (tile-at 1 5))))

(define-test enemyp
  (assert-true (enemyp (tile-at 2 6)))
  (assert-true (enemyp (tile-at 3 5)))
  (assert-false (enemyp (tile-at 3 4)))
  ;; We don't really care about the specific error.
  (assert-error 'error (enemyp (tile-at 0 0)))
  (assert-error 'error (enemyp (tile-at 0 1)))
  (assert-error 'error (enemyp (tile-at 1 5))))

(define-test friendlyp
  (assert-true (friendlyp (tile-at 3 4)))
  (assert-false (friendlyp (tile-at 2 6)))
  (assert-false (friendlyp (tile-at 3 5)))
  ;; We don't really care about the specific error.
  (assert-error 'error (friendlyp (tile-at 0 0)))
  (assert-error 'error (friendlyp (tile-at 0 1)))
  (assert-error 'error (friendlyp (tile-at 1 5))))

(define-test foodp
  (assert-equal 2 (length (food *state*)))
  (assert-true (foodp (tile-at 2 1)))
  (assert-true (foodp (tile-at 1 5)))
  (assert-false (foodp (tile-at 0 0)))
  (assert-false (foodp (tile-at 0 1)))
  (assert-false (foodp (tile-at 2 6)))
  (assert-false (foodp (tile-at 3 4))))

(define-test landp
  (assert-true (landp (tile-at 0 0)))
  (assert-true (landp (tile-at 3 3)))
  (assert-false (landp (tile-at 0 1)))
  (assert-false (landp (tile-at 1 5)))
  (assert-false (landp (tile-at 2 6)))
  (assert-false (landp (tile-at 3 4))))

(define-test waterp
  (assert-true (waterp (tile-at 0 1)))
  (assert-true (waterp (tile-at 1 3)))
  (assert-false (waterp (tile-at 0 0)))
  (assert-false (waterp (tile-at 1 5)))
  (assert-false (landp (tile-at 2 6)))
  (assert-false (landp (tile-at 3 4))))

(format t "~&nearby-ants: ~S~%" (nearby-ants 3 4 5))  ; why not <A 1:3,5> too?
(define-test nearby-ants
  (assert-equal 0 (length (nearby-ants 3 4 0.5)))
  (assert-equal 1 (length (nearby-ants 3 4 1)))
  (assert-equal 2 (length (nearby-ants 3 4 5))))


;;; Run the tests.

(format t "--- running tests ---~%")
(run-tests)
(format t "~&")
(cl-user::quit)
