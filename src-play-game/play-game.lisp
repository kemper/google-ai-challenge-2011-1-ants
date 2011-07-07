;;;; play-game.lisp
;;;;
;;;; This is really a hack job currently.
;;;;
;;;; Let me see if I can get this right: (bot-id bot) is assigned in the order
;;;; of how they appear on the commandline by looping through (remainder)
;;;; and calling (unique-bot-id) each time a bot class is instantiated.
;;;;
;;;; The initial ants / "ant tiles", however, are assigned IDs in the order
;;;; they appear on the map (by going through it from left-to-right, top-to-
;;;; bottom).  So the alphabetic character used on the map to signify an
;;;; ant tile has NO bearing on the ID it will get.  These are only used to
;;;; differentiate between ant tiles.
;;;;
;;;; This is all in the name of compatibility with playgame.py.

(in-package :play-game)


;;; Functions

(defun adjust-score (bot delta)
  (incf (last1 (scores bot)) delta))


(defun all-ants ()
  (loop for bot across @bots append (ants bot)))


(defun ant-owner (ant)
  (aref @bots (pid ant)))


(defun ant-with-less-or-equal (ants number-of-enemies)
  (loop for ant in ants
        for n-enemies = (length (nearby-ants (row ant) (col ant)
                                             @attack-radius2 (pid ant)))
        when (<= n-enemies number-of-enemies)
          do (return-from ant-with-less-or-equal ant)))


;; http://github.com/aichallenge/aichallenge/wiki/Ants-focus-battle-resolution-method
(defun battle-resolution ()
  ;; TODO add (declare ...) ?
  (loop with ar2 = @attack-radius2
        with mark-as-land = nil
        for ant in (all-ants)
        for apid = (pid ant)
        for nearby-enemies = (nearby-ants (row ant) (col ant) ar2 apid)
        for n-enemies = (length nearby-enemies)
        for killer = (when (> n-enemies 0)
                       (ant-with-less-or-equal nearby-enemies n-enemies))
        do (when killer
             (let ((bot (aref @bots apid)))
               (push ant (slot-value bot 'dead-ants))
               (pushnew ant mark-as-land)
               (setf (slot-value ant 'dead) t
                     (slot-value ant 'end-turn) @turn
                     (slot-value bot 'ants) (remove ant (ants bot)))
               (adjust-score (ant-owner killer) 1)))
        finally (loop for land in mark-as-land
                      do (setf (tile-at (row land) (col land)) +land+))))


(let ((mapping (make-array 0 :fill-pointer 0 :element-type 'character)))
  (defun char2pid (char)
    (unless (find char mapping)
      (vector-push-extend char mapping))
    (position char mapping)))


;; This function only sets (DEAD ANT) to true.  They're actually removed in
;; MOVE-ANTS.  (Why?  It's mostly a quick fix because the replay format was
;; otherwise invalid because we removed the order.)
(defun check-collisions ()
  (loop for order-a in @orders
        for bot-a-id = (order-bot-id order-a)
        for srow-a = (order-src-row order-a)
        for scol-a = (order-src-col order-a)
        for row-a = (order-dst-row order-a)
        for col-a = (order-dst-col order-a)
        ;do (loop for order-b in (remove order-a @orders)
        do (loop for order-b in @orders
                 for bot-b-id = (order-bot-id order-b)
                 for srow-b = (order-src-row order-b)
                 for scol-b = (order-src-col order-b)
                 for row-b = (order-dst-row order-b)
                 for col-b = (order-dst-col order-b)
                 do (when (and (not (equal order-b order-a))
                               (= row-a row-b)
                               (= col-a col-b))
                      (let* ((bot-a (aref @bots bot-a-id))
                             (bot-b (aref @bots bot-b-id))
                             (ant-a (tile-at srow-a scol-a))
                             (ant-b (tile-at srow-b scol-b)))
                        (push ant-a (slot-value bot-a 'dead-ants))
                        (push ant-b (slot-value bot-b 'dead-ants))
                        (setf (slot-value ant-a 'dead) t
                              (slot-value ant-b 'dead) t
                              (slot-value ant-a 'end-turn) @turn
                              (slot-value ant-b 'end-turn) @turn))))))


;; TODO remove (it's done in move-ants now)
(defun check-positions ()
  (loop for order in (copy-seq @orders)
        for bot-id = (order-bot-id order)
        for row = (order-src-row order)
        for col = (order-src-col order)
        do (when (/= bot-id (pid (tile-at row col)))
             (logmsg "Bot " bot-id " issued an order for a position it "
                     "doesn't occupy. Ignoring...~%")
             (logmsg "o: " order "~%")
             (logmsg "p: " (pid (tile-at row col)) "~%")
             ;; TODO use DELETE?
             (setf @orders (remove order @orders)))))


;; TODO remove (it's done in move-ants now)
(defun check-water ()
  (loop for order in (copy-seq @orders)
        for new-location = (new-location (order-src-row order)
                                 (order-src-col order) (order-direction order))
        do (when (waterp (tile-at (elt new-location 0) (elt new-location 1)))
             ;; TODO report row col dir
             (logmsg "Bot " (order-bot-id order) " ordered an ant into water. "
                     "Ignoring...~%")
             ;; TODO use DELETE?
             (setf @orders (remove order @orders)))))


;; TODO remove? (not used in move-ants anymore)
(defun clear-dead-ants ()
  (loop for row from 0 below @rows
        do (loop for col from 0 below @cols
                 for tile = (tile-at row col)
                 do (when (and (antp tile) (dead tile))
                      (setf (tile-at row col) +land+)))))


(defun dir2key (direction)
  (cond ((equal direction "N") :north)
        ((equal direction "E") :east)
        ((equal direction "S") :south)
        ((equal direction "W") :west)))


(defun do-turn ()
  (init-scores-for-new-turn)
  (setf @orders nil)
  ;(revitalize-ants)
  (loop for bot across @bots do (setf (thread-status bot) :new-turn))
  ;; TODO could use some work (and its own function)
  (loop with bots-ready = 0
        until (= bots-ready @n-players)
        do (sleep +sleep-time+)
           (loop with n-ready = 0
                 for bot across @bots
                 do (when (equal (thread-status bot) :ready)
                      (incf n-ready))
                 finally (setf bots-ready n-ready)))
  (setf @orders (loop for bot across @bots append (orders bot)
                      do (setf (thread-status bot) :wait)))
  (move-ants)
  (battle-resolution)  ; !!! optimisations up and including to here !!!
  (spawn-ants)
  ;; TODO move check into spawn-food
  (unless (equal @food-method :none) (spawn-food))
  ;; TODO move into update-bot-status function
  (loop for bot across @bots
        do (when (and (equal "survived" (status bot))
                      (= 0 (length (ants bot))))
             (format @log-stream "turn ~4D bot ~D eliminated~%"
                     @turn (bot-id bot))
             (force-output @log-stream)
             (setf (status bot) "eliminated"))))


(defun getopt (name)
  "Wrapper for CLON:GETOPT. Returns value of option NAME."
  (if (> (length name) 1)
      (com.dvlsoft.clon:getopt :long-name name)
      (com.dvlsoft.clon:getopt :short-name name)))


(defun getopt-key (name)
  "Wrapper for CLON:GETOPT. Returns value of option NAME as a keyword."
  (intern (string-upcase (if (> (length name) 1)
                             (com.dvlsoft.clon:getopt :long-name name)
                             (com.dvlsoft.clon:getopt :short-name name)))
          :keyword))


(defun getopt-nr (name)
  "Wrapper for CLON:GETOPT. Returns option NAME as an integer."
  (parse-number (if (> (length name) 1)
                    (com.dvlsoft.clon:getopt :long-name name)
                    (com.dvlsoft.clon:getopt :short-name name))))


(defun init-scores-for-new-turn ()
  (loop for bot across @bots
        do (when (equal "survived" (status bot))
             (vector-push-extend (aref (scores bot) (- @turn 1)) (scores bot))
             ;; hack: dunno how playgame.py gets those initial scores
             (when (= 1 @turn)
               (adjust-score bot (length (ants bot)))))))


(defun key2dir (key)
  (cond ((equal key :north) #\n)
        ((equal key :east)  #\e)
        ((equal key :south) #\s)
        ((equal key :west)  #\w)))


(defun log-new-turn-stats ()
  (when *verbose*
    (format @log-stream "turn ~4D stats: ant_count: [~A]~%"
            @turn (players-ant-count-string :sep ", "))
    (force-output @log-stream)))


;; http://www.sbcl.org/manual/index.html#Waitqueue_002fcondition-variables
(defun make-io-thread (bot-id)
  (sb-thread:make-thread
   (lambda ()
     (loop with bot = (aref @bots bot-id)
           while t
           do (wait-for-new-turn bot)
              (send-game-state bot)
              (loop until (listen (sb-ext:process-output (process bot)))
                    do (sleep +sleep-time+))
              (setf (orders bot) nil)
              (receive-bot-orders bot)
              (setf (thread-status bot) :ready)))
   :name (mkstr "bot-io-thread-" bot-id)))


;; If needed for performance CHECK-POSITIONS and CHECK-WATER could be moved
;; into the loop.
(defun move-ants ()
  ;(clear-dead-ants)  ; TODO needed?
  ;(check-positions)
  ;(check-water)
  (check-collisions)
  (loop for order in @orders
        for bot-id = (order-bot-id order)
        for src-row = (order-src-row order)
        for src-col = (order-src-col order)
        for dst-row = (order-dst-row order)
        for dst-col = (order-dst-col order)
        for tile = (tile-at src-row src-col)
        do (cond ((/= bot-id (pid tile))
                  (logmsg "Not owner of tile: " order ". Ignoring...~%"))
                 ((waterp (tile-at dst-row dst-col))
                  (logmsg "Ordered into water: " order ". Ignoring...~%"))
                 ((antp tile)
                  (vector-push-extend (key2dir (order-direction order))
                                      (orders tile))
                  (setf (slot-value tile 'row) dst-row
                        (slot-value tile 'col) dst-col
                        (tile-at src-row src-col) +land+)
                  (unless (dead tile)
                    (setf (tile-at dst-row dst-col) tile))
                  (when (dead tile)
                    (let ((bot (ant-owner tile)))
                      (setf (slot-value bot 'ants)
                            (delete tile (ants bot))))))))
  (update-immobile-ant-orders))


(defun no-turn-time-left-p (turn-start-time)
  (not (turn-time-left-p turn-start-time)))


(defun parse-map (file-name)
  (with-open-file (f file-name)
    (loop with rows = 0
          for line = (read-line f nil)
          while line
          do (cond ((starts-with line "cols ")
                    (setf (slot-value *state* 'cols) (par-value line)))
                   ((starts-with line "rows ")
                    (setf (slot-value *state* 'rows) (par-value line)))
                   ((starts-with line "players ")
                    (setf (slot-value *state* 'n-players) (par-value line)))
                   ((and (starts-with line "m ") (null @cols))
                    (errmsg "~&Map missing \"cols n\" line. Aborting...~%")
                    (quit 1))
                   ((and (starts-with line "m ") (null @rows))
                    (errmsg "~&Map missing \"rows n\" line. Aborting...~%")
                    (quit 1))
                   ((and (starts-with line "m ") (null @n-players))
                    (errmsg "~&Map missing \"players n\" line. Aborting...~%")
                    (quit 1))
                   ((and (starts-with line "m ") (< (length (remainder))
                                                    @n-players))
                    (errmsg "~&Map needs " @n-players " players but only "
                            (length (remainder)) " were entered on the "
                            "command-line. Aborting...~%")
                    (quit 1))
                   ((and (starts-with line "m ") (null @game-map))
                    (setf (slot-value *state* 'game-map)
                          (make-array (list @rows @cols) :initial-element :zz))
                    (parse-map-line @game-map line rows)
                    (incf rows))
                   ((starts-with line "m ")
                    (parse-map-line @game-map line rows)
                    (incf rows)))
          finally (when (/= rows @rows)
                    (errmsg "~&Actual map rows (" rows ") not equal to "
                            "specified number of rows (" @rows "). "
                            "Aborting...~%")
                    (quit 1)))))


(defun parse-map-line (map-array string row)
  (when (/= (- (length string) 2) @cols)
    (errmsg "~&Actual map columns (" (- (length string) 2) ") for this line "
            "not equal to specified number of~%columns (" @cols ") for this "
            "map. Aborting...~%")
    (quit 1))
  (loop for c across (subseq string 2)
        for col from 0
        do (setf (aref map-array row col)
                 (case c
                   (#\. +land+)
                   (#\% (make-instance 'water :row row :col col
                          :seen-by (make-array (length (bots *state*))
                                               :element-type 'boolean
                                               :initial-element nil)))
                   (#\* (let ((food (make-instance 'food :row row :col col
                                      :start-turn 0 :conversion-turn 0)))
                          (push food @food)
                          food))
                   (otherwise
                    (let ((ant (make-instance 'ant :row row :col col
                                 :initial-row row :initial-col col
                                 :pid (char2pid c) :start-turn 0)))
                      (push ant (slot-value (ant-owner ant) 'ants))
                      ant))))))


(defun play-game ()
  (loop for turn from 0 to @turns
        do (setf @turn turn)
           (log-new-turn-stats)
           (cond ((= turn 0) (send-initial-game-state))
                 ((> turn 0) (do-turn)))))


(defun players-ant-count-string (&key (sep " "))
  (with-output-to-string (s)
    (loop for bot across @bots
          for i from 1
          do (princ (length (ants bot)) s)
             (when (< i (length @bots))
               (princ sep s)))))


(defun players-score-string (&key (sep " "))
  (with-output-to-string (s)
    (loop for bot across (bots *state*)
          for i from 1
          do (princ (last1 (scores bot)) s)
             (when (< i (length (bots *state*)))
               (princ sep s)))))


(defun players-status-string (&key (sep " ") (quotes nil))
  (with-output-to-string (s)
    (loop for bot across (bots *state*)
          for i from 1
          do (when quotes (princ "\"" s))
             (princ (status bot) s)
             (when quotes (princ "\"" s))
             (when (< i (length (bots *state*)))
               (princ sep s)))))


(defun process-command-line-options ()
  (cond ((or (getopt "?") (getopt "h") (= 1 (length (cmdline))))
         (help)
         (quit))
        ;; use :SAVE-RUNTIME-OPTIONS to SAVE-LISP-AND-DIE if you want --version
        ((getopt "release")
         (format t "~&play-game (Ant Wars) version ~A~%" +version+)
         (quit)))
  (setf *verbose*                            (getopt     "v")
        (slot-value *state* 'food-method)    (getopt-key "f")
        (slot-value *state* 'map-file)       (getopt     "m")
        (slot-value *state* 'rounds)         (getopt-nr  "r")
        (slot-value *state* 'turns)          (getopt-nr  "t")
        (slot-value *state* 'attack-radius2) (getopt-nr  "attackradius2")
        (slot-value *state* 'end-wait)       (getopt-nr  "end_wait")
        (slot-value *state* 'load-time)      (getopt-nr  "loadtime")
        (slot-value *state* 'replay-dir)     (getopt     "log_dir")
        (slot-value *state* 'spawn-radius2)  (getopt-nr  "spawnradius2")
        (slot-value *state* 'turn-time)      (getopt-nr  "turntime")
        (slot-value *state* 'view-radius2)   (getopt-nr  "viewradius2"))
  (unless (member (food-method *state*) '(:none :random))
    (help)
    (errmsg "~%Error: unknown food method: " (food-method *state*) "~%")
    (quit))
  (unless (map-file *state*)
    (help)
    (quit)))
  ;; (remainder) is processed in (start-bots)


(defun queue-ant-order (bot-id string)
  (let* ((split (split-sequence #\space (string-upcase string)))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2)))
         (dir (dir2key (elt split 3)))
         (nl (new-location row col dir)))
    ; TODO check for illegal moves here
    (push (make-order :bot-id bot-id :direction dir :src-row row :src-col col
                      :dst-row (elt nl 0) :dst-col (elt nl 1))
          (orders *state*))))


(defun receive-bot-orders (bot)
  (loop for line = (read-line (sb-ext:process-output (process bot)) nil)
        until (starts-with line "go")
        do (when (starts-with line "o ")
             (let* ((split (split-sequence #\space (string-upcase line)))
                    (row (parse-integer (elt split 1)))
                    (col (parse-integer (elt split 2)))
                    (dir (dir2key (elt split 3)))
                    (nl (new-location row col dir)))
               (push (make-order :bot-id (bot-id bot) :direction dir
                                 :src-row row :src-col col
                                 :dst-row (elt nl 0) :dst-col (elt nl 1))
                     (orders bot))))))


(defun revitalize-ants ()
  (loop for ant in (all-ants) do (setf (hp ant) 1)))


(defun run-program (program &optional (args nil))
  ;; If there's a space in PROGRAM assume it needs to be started with an
  ;; interpreter which needs a slightly different call to RUN-PROGRAM.
  ;; Perhaps just checking for "python", "ruby", etc. would be better...
  (if (find #\space program)
      (let ((split (split-sequence #\space program)))
        (sb-ext:run-program (car split) (cdr split) :search t :wait nil
                            :input :stream :output :stream))
      (sb-ext:run-program (merge-pathnames program) args :wait nil
                          :input :stream :output :stream)))


;; See: https://github.com/aichallenge/aichallenge/wiki/Ants-replay-format
;; Perhaps I should have used a JSON lib :-|
(defun save-replay (&optional (round 0))
  (with-open-file (f (mkstr (replay-dir *state*) round ".replay")
                   :direction :output :if-exists :supersede)
    (format f (mkstr "{~%"
                     "    \"challenge\": \"ants\",~%"
                     "    \"game_id\": 0,~%"
                     "    \"location\": \"localhost\",~%"
                     "    \"player_info\": ["))
    (loop for i from 0 below (n-players *state*)
          do (princ "{}" f)
             (when (< i (- (n-players *state*) 1))
               (princ ", " f)))
    (format f (mkstr "],~%"
                     "    \"rank\": ["))
    (loop with ranking = (loop for bot across (bots *state*)
                               collect (last1 (scores bot)) into scores
                               finally (return (sort (remove-duplicates scores)
                                                     #'>)))
          for bot across (bots *state*)
          for i from 1
          do (princ (position (last1 (scores bot)) ranking) f)
             (when (< i (n-players *state*))
               (princ ", " f)))
    (format f (mkstr "],~%"
                     "    \"replayformat\": \"json\",~%"
                     "    \"replaydata\": {~%"
                     "        \"revision\": 2,~%"
                     "        \"players\": " (n-players *state*) ",~%"
                     "        \"loadtime\": " (load-time *state*) ",~%"
                     "        \"turntime\": " (turn-time *state*) ",~%"
                     "        \"turns\": " (turns *state*) ",~%"
                     "        \"viewradius2\": " (view-radius2 *state*) ",~%"
                   "        \"attackradius2\": " (attack-radius2 *state*) ",~%"
                     "        \"spawnradius2\": " (spawn-radius2 *state*) ",~%"
                     "        \"map\": {~%"
                     "             \"rows\": " (rows *state*) ",~%"
                     "             \"cols\": " (cols *state*) ",~%"
                     "             \"data\": [~%"))
    (loop for row from 0 below (rows *state*)
          do (format f "                     \"")
             (loop for col from 0 below (cols *state*)
                   for tile = (aref (game-map *state*) row col)
                   for type = (type-of tile)
                   do (case type
                        (land  (princ #\. f))
                        (water (princ #\% f))
                        (food  (princ #\* f))
                        (ant (if (dead tile)
                                 (princ (code-char (+ (pid tile) 65)) f)
                                 (princ (code-char (+ (pid tile) 97)) f)))
                        (otherwise (error "Unknown tile type: ~S (~D,~D)"
                                          tile row col))))
             (if (< (+ row 1) (rows *state*))
                 (format f "\",~%")
                 (format f "\"~%")))
    (format f (mkstr "             ]~%"
                     "        },~%"
                     "        \"ants\": [~%"))
    (loop with food-length = (+ (length (food *state*))
                                (length (contested-food *state*)))
          for bot across (bots *state*)
          for i from 1
          do (loop for ant in (append (reverse (ants bot))
                                      (reverse (dead-ants bot)))
                   for j from 1
                   do (format f "            [ ~D, ~D, ~D, ~D, ~D, ~D, ~S ]~A~%"
                              (initial-row ant)
                              (initial-col ant)
                              (start-turn ant)
                              (conversion-turn ant)
                              (if (dead ant)
                                  (end-turn ant)
                                  (+ (turns *state*) 1))
                              (pid ant)
                              (coerce (orders ant) 'string)
                              (if (or (< i (length (bots *state*)))
                                      (< j (+ (length (ants bot))
                                              (length (dead-ants bot))))
                                      (> food-length 0))
                                   ","
                                   ""))))
    (loop with food-length = (+ (length (food *state*))
                                (length (contested-food *state*)))
          for food in (append (reverse (food *state*))
                              (reverse (contested-food *state*)))
          for i from 1
          do (format f "            [ ~D, ~D, ~D, ~D ]~A~%"
                     (row food)
                     (col food)
                     (start-turn food)
                     (if (= 0 (conversion-turn food))
                         (+ (turns *state*) 1)
                         (conversion-turn food))
                     (if (< i food-length) "," "")))
    (format f (mkstr "        ],~%"
                     "        \"scores\": [~%"))
    (loop for bot across (bots *state*)
          for i from 1
          do (princ "            [" f)
             (loop for n across (scores bot)
                   for j from 1
                   do (princ n f)
                      (when (< j (length (scores bot)))
                        (princ "," f)))
             (princ "]" f)
             (when (< i (length (bots *state*)))
               (princ "," f))
             (terpri f))
    (format f (mkstr "        ]~%"
                     "    },~%"
                     "    \"score\": [~A],~%"
                     "    \"status\": [~A]~%")
            (players-score-string :sep ", ")
            (players-status-string :sep ", " :quotes t))
    (format f (mkstr "}~%"))))


(defun send-ant (stream row col bot-id tile)
  (declare (inline + < = > pid princ write-string terpri)
           (optimize (speed 3))
           (type fixnum row col bot-id))
  (write-string (if (dead tile) "d " "a ") stream)
  (princ row stream)
  (write-string " " stream)
  (princ col stream)
  (write-string " " stream)
  (princ (cond ((= bot-id (the fixnum (pid tile))) 0)
               ((< bot-id (the fixnum (pid tile))) (pid tile))
               ((> bot-id (the fixnum (pid tile)))
                (+ (the fixnum (pid tile)) 1)))
         stream)
  (terpri stream))


(defun send-food (stream row col)
  (declare (inline princ write-string terpri)
           (optimize (speed 3)))
  (write-string "f " stream)
  (princ row stream)
  (write-string " " stream)
  (princ col stream)
  (terpri stream))


(defun send-go (&optional (stream *standard-output*))
  (format stream "~&go~%")
  (force-output stream))


(defun send-turn (stream turn)
  (format stream "turn ~D~%" turn))


(defun send-water (stream row col id tile)
  (declare (inline elt princ write-string terpri)
           (optimize (speed 3)))
  (unless (elt (the simple-vector (seen-by tile)) id)
    (setf (elt (the simple-vector (seen-by tile)) id) t)
    (write-string "w " stream)
    (princ row stream)
    (write-string " " stream)
    (princ col stream)
    (terpri stream)))


;; If you need to optimize, try here first.
(defun send-game-state (bot)
  (declare (inline + - dist2 elt floor send-ant send-food send-water sqrt
                   tile-at wrapped-row wrapped-col)
           (optimize (speed 3)))
  (let ((str (string-output-stream bot))
        (stream (sb-ext:process-input (process bot))))
    (send-turn str @turn)
    (loop with id = (bot-id bot)
          with vr2 = (the fixnum @view-radius2)
          with vr = (floor (sqrt vr2))
          for ant in (ants bot)
          for arow = (the fixnum (row ant))
          for acol = (the fixnum (col ant))
          do (loop for roff from (- arow vr) to (+ arow vr)
                   for row = (wrapped-row roff)
                   do (loop for coff from (- acol vr) to (+ acol vr)
                           for col = (wrapped-col coff)
                           when (<= (the fixnum (dist2 arow acol row col)) vr2)
                           do (let ((tile (tile-at row col)))
                                (typecase tile
                                  (water (send-water str row col id tile))
                                  ;; order matters: ant is a subclass of food
                                  (ant (send-ant str row col id tile))
                                  (food (send-food str row col)))))))
    (write-string (get-output-stream-string str) stream)
    (send-go stream)))


(defun send-initial-game-state ()
  (loop for bot across @bots
        for command-line = (command-line bot)
        for id = (bot-id bot)
        for proc = (process bot)
        for pin = (sb-ext:process-input proc)
        for pout = (sb-ext:process-output proc)
        do (if (equal :running (sb-ext:process-status proc))
               (progn
                 (format pin (mkstr "turn 0~%loadtime ~D~%turntime ~D~%"
                                    "rows ~D~%cols ~D~%turns ~D~%"
                                    "viewradius2 ~D~%attackradius2 ~D~%"
                                    "spawnradius2 ~D~%ready~%")
                         @load-time @turn-time @rows @cols @turns @view-radius2
                         @attack-radius2 @spawn-radius2)
                 (force-output pin))
               (logmsg id ":" command-line " has stopped running...~%"))
           (when (equal :running (sb-ext:process-status proc))
             (let ((turn-start (wall-time)))
               (wait-for-output pout turn-start)
               (cond ((no-turn-time-left-p turn-start)
                      (logmsg id ":" command-line " timed out.~%"))
                     ((listen pout)
                      (let ((line (read-line pout nil)))
                        (unless (starts-with line "go")
                          (logmsg id ":" command-line " sent junk: " line
                                "~%")))))))))


(defun spawn-ants ()
  (loop with mark-as-ant = nil
        with sr2 = @spawn-radius2
        with sr = (floor (sqrt sr2))
        for food in (copy-seq @food)  ; we're modifying (food *state*)
        for frow = (row food)
        for fcol = (col food)
        for ants = nil
        for pids = nil
        do (loop for roff from (- frow sr) to (+ frow sr)
                 do (loop for coff from (- fcol sr) to (+ fcol sr)
                         for tile = (tile-if-reachable sr2 frow fcol roff coff)
                         when tile
                           do (when (and (antp tile) (alivep tile))
                                (pushnew (pid tile) pids)
                                (push tile ants))))
           (cond ;; spawn an ant
                 ((or (= (length ants) 1) (= (length pids) 1))
                  (setf @food (remove food @food))
                  (let* ((pid (pid (first ants)))
                         (bot (aref @bots pid))
                         (ant (make-instance 'ant :row frow :col fcol
                               :start-turn (start-turn food)
                               :conversion-turn @turn
                               :initial-row frow :initial-col fcol :pid pid)))
                    (push ant (slot-value bot 'ants))
                    (pushnew ant mark-as-ant)
                    (adjust-score bot 1)))
                 ;; contested food, destroy it
                 ((and (> (length ants) 1) (> (length pids) 1))
                  (push food @contested-food)
                  (setf (conversion-turn food) @turn
                        @food (remove food @food)
                        (tile-at frow fcol) +land+)))
        finally (loop for ant in mark-as-ant
                      do (setf (tile-at (row ant) (col ant)) ant))))


;; Very simple random food spawning.  Collects all land tile's coordinates
;; and randomly picks a random number of tiles between 0 and the number of
;; players (-1?).
(defun spawn-food ()
  (let ((food (loop for row from 0 below (rows *state*)
                    append (loop for col from 0 below (cols *state*)
                                 for tile = (aref (game-map *state*) row col)
                                 when (typep tile 'land)
                                   collect (vector row col))
                      into result
                    finally (return (loop repeat (random (n-players *state*))
                                          collect (random-elt result))))))
    (loop for rc in food
          for row = (elt rc 0)
          for col = (elt rc 1)
          do (let ((food (make-instance 'food :row row :col col
                                        :start-turn (turn *state*))))
               (push food (slot-value *state* 'food))
               (setf (aref (game-map *state*) row col) food)))))


(defun start-bots ()
  (loop for command-line in (com.dvlsoft.clon:remainder)
        for proc = (run-program command-line)
        for bot = (make-instance 'bot :command-line command-line :process proc)
        do (vector-push-extend bot @bots)
           (make-io-thread (bot-id bot))))


(defun turn-time-left-p (turn-start-time)
  (<= (- (wall-time) turn-start-time)
      (/ (turn-time *state*) 1000)))


;; TODO scoring for ants that died this turn
(defun update-immobile-ant-orders ()
  (loop for ant in (all-ants)
        for orders = (orders ant)
        when (< (length orders) (- @turn (conversion-turn ant)))
          do (vector-push-extend #\- orders)))


(defun wait-for-new-turn (bot)
  (loop until (equal :new-turn (thread-status bot))
        do (sleep +sleep-time+)))


(defun wait-for-output (stream turn-start-time)
  (loop until (or (listen stream)
                  (no-turn-time-left-p turn-start-time))
        do (sleep +sleep-time+)))


;;; Main Program

(defsynopsis (:postfix "BOT1 BOT2 .. BOTn")
  (text :contents "Game engine for Ant Wars.
")
  (group (:header "Game options:")
    (path :short-name "m" :long-name "map_file" :argument-name "MAP"
          :type :file :description "Name of the map file. (required)")
    (stropt :short-name "t" :long-name "turns" :argument-name "TURNS"
            :default-value "200"
            :description "Number of turns in the game.")
    (flag :long-name "serial"
      :description "Run bots in serial, instead of parallel. (defunct)")
    (stropt :long-name "loadtime" :argument-name "LOADTIME"
            :default-value "3000"
            :description "Amount of time to give for load, in milliseconds.")
    (stropt :long-name "turntime" :argument-name "TURNTIME"
            :default-value "1000"
            :description "Amount of time to give each bot, in milliseconds.")
    (stropt :short-name "r" :long-name "rounds" :argument-name "ROUNDS"
            :default-value "1"
            :description "Number of rounds to play.")
    (stropt :long-name "end_wait" :argument-name "END_WAIT"
            :default-value "0"
            :description "Seconds to wait at end for bots to process end.")
    (stropt :short-name "f" :long-name "food" :argument-name "FOOD"
            :default-value "random"
            :description "Food spawning method. (none, random).")
    (stropt :long-name "viewradius2" :argument-name "VIEWRADIUS2"
            :default-value "55"
            :description "Vision radius of ants squared.")
    (stropt :long-name "spawnradius2" :argument-name "SPAWNRADIUS2"
            :default-value "1"
            :description "Spawn radius of ants squared.")
    (stropt :long-name "attackradius2" :argument-name "ATTACKRADIUS2"
            :default-value "5"
            :description "Attack radius of ants squared.")
    (path :short-name "l" :long-name "log_dir" :argument-name "LOG_DIR"
          :type :directory :default-value #p"game_logs/"
          :description "Directory to dump replay files to."))
  (group (:header "Debug options:")
    (flag :short-name "v" :long-name "verbose"
      :description "Print out status as game goes."))
  (group (:header "Immediate exit options:")
    (flag :short-name "?"
      :description "Print this help and exit.")
    (flag :short-name "h" :long-name "help"
      :description "Print this help and exit.")
    (flag :long-name "release"
	  :description "Print version/release number and exit.")))


(defun debug-output ()
  (logmsg "===============================================================~%")
  (describe *state*)
  (loop for bot across (bots *state*)
        do (describe bot))
  (logmsg "===============================================================~%"))


(defun main ()
  (make-context)
  (setf *state* (make-instance 'play-game-state :error-stream *debug-io*))
  (process-command-line-options)
  (start-bots)
  (parse-map @map-file)
  (logmsg "running for " @turns " turns~%")
  (handler-bind (#+sbcl (sb-sys:interactive-interrupt #'user-interrupt))
    (play-game))
  (logmsg "score " (players-score-string) "~%")
  (logmsg "status " (players-status-string) "~%")
  (save-replay)
  (sleep (end-wait *state*)))
