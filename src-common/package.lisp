;;;; package.lisp

(in-package :cl-user)

(defpackage :ants-common
  (:use :cl)
  (:export ;; specials
           :*state* :*verbose* :+land+
           ;; classes and accessors / readers
           :ant :food :land :state :water
           :attack-radius2 :col :cols :conversion-turn :dead :end-turn
           :enemy-ants :error-stream :food :game-map :hp :initial-col
           :initial-row :input :last1 :load-time :log-stream :my-ants :orders
           :output :pid :row :rows :seen-by :spawn-radius2 :start-turn :turn
           :turn-start-time :turn-time :turns :view-radius2
           ;; handlers
           :address-in-use :connection-lost :connection-refused :error-handler
           :socket-error-handler :user-interrupt
           ;; functions
           :alivep :antp :current-date-time-string :distance :distance2 :enemyp
           :errmsg :foodp :friendlyp :host2str :landp :logmsg :mkstr
           :nearby-ants :new-location :par-value :print-game-map :quit
           :random-elt :slimesg :starts-with :tile-at :tile-if-reachable
           :wall-time :water? :waterp :wrapped-row-col))
