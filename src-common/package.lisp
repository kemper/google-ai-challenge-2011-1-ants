;;;; package.lisp

(in-package :cl-user)

(defpackage :ants-common
  (:use :cl)
  (:export ;; specials
           :*state* :*verbose* :+land+ :2pi
           ;; classes and accessors / readers
           :ant :food :land :state :water
           :attack-radius2 :col :cols :conversion-turn :dead :end-turn
           :error-stream :food :game-map :hp :initial-col :initial-row :input
           :load-time :log-stream :orders :output :pid :row :rows :seen-by
           :spawn-radius2 :start-turn :turn :turn-start-time :turn-time :turns
           :view-radius2
           ;; handlers
           :address-in-use :connection-lost :connection-refused :error-handler
           :socket-error-handler :user-interrupt
           ;; predicates
           :alivep :antp :enemyp :foodp :friendlyp :landp :waterp
           ;; functions
           :current-date-time-string :distance :dist :distance2 :dist2 :errmsg
           :host2str :last1 :logmsg :mkstr :nearby-ants :new-location
           :par-value :print-game-map :quit :random-elt :slimesg :starts-with
           :tile-at :tile-if-reachable :wall-time :wrapped-row :wrapped-col
           :wrapped-row-col))
