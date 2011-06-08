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

;; all-alive-ants actually
(defun all-ants ()
  (loop for bot across (bots *state*) append (ants bot)))

(defun oisoidosdi ())
(defun battle-resolution ()
  ;; distribute damage
  (loop with enemy-ants = nil
        with ar2 = (attack-radius2 *state*)
        with ar = (floor (sqrt ar2))
        for ant in (all-ants)
        for arow = (row ant)
        for acol = (col ant)
        do (loop for roff from (- arow ar) to (+ arow ar)
                 do (loop for coff from (- acol ar) to (+ acol ar)
                         for tile = (tile-if-reachable ar2 arow acol roff coff)
                         when tile
                           do (when (and (antp tile) (alivep tile)
                                         (/= (pid ant) (pid tile)))
                                (push tile enemy-ants))))
           (let ((n-enemy-ants (length enemy-ants)))
             (when (> n-enemy-ants 0)
               (loop with dmg = (/ 1 n-enemy-ants)
                     for enemy-ant in enemy-ants
                     do (decf (hp enemy-ant) dmg))
               (setf enemy-ants nil))))
  ;; kill ants with hit-points <= 0
  (loop for ant in (all-ants)
        do (when (<= (hp ant) 0)
             ;; TODO duplicated in other functions, needs its own function
             (let ((bot (aref (bots *state*) (pid ant))))
               (push ant (slot-value bot 'dead-ants))
               (setf (aref (game-map *state*) (row ant) (col ant)) +land+
                     (slot-value ant 'dead) t
                     (slot-value ant 'end-turn) (turn *state*)
                     (slot-value bot 'ants) (remove ant (ants bot)))))))


(let ((mapping (make-array 0 :fill-pointer 0 :element-type 'character)))
  (defun char2pid (char)
    (unless (find char mapping)
      (vector-push-extend char mapping))
    (position char mapping)))


;; This function only sets (DEAD ANT) to true.  They're actually removed in
;; MOVE-ANTS.  (Why?  It's mostly a quick fix because the replay format was
;; otherwise invalid because we removed the order.)
;;
;; TODO check only squares near order-a
(defun check-collisions ()
  (loop for order-a in (orders *state*)
        for bot-a-id = (order-bot-id order-a)
        for srow-a = (order-src-row order-a)
        for scol-a = (order-src-col order-a)
        for row-a = (order-dst-row order-a)
        for col-a = (order-dst-col order-a)
        do (loop for order-b in (remove order-a (orders *state*))
                 for bot-b-id = (order-bot-id order-b)
                 for srow-b = (order-src-row order-b)
                 for scol-b = (order-src-col order-b)
                 for row-b = (order-dst-row order-b)
                 for col-b = (order-dst-col order-b)
                 do (when (and (= row-a row-b)
                               (= col-a col-b))
                      (let* ((bot-a (aref (bots *state*) bot-a-id))
                             (bot-b (aref (bots *state*) bot-b-id))
                             (ant-a (aref (game-map *state*) srow-a scol-a))
                             (ant-b (aref (game-map *state*) srow-b scol-b)))
                        (push ant-a (slot-value bot-a 'dead-ants))
                        (push ant-b (slot-value bot-b 'dead-ants))
                        (setf (slot-value ant-a 'dead) t
                              (slot-value ant-b 'dead) t
                              (slot-value ant-a 'end-turn) (turn *state*)
                              (slot-value ant-b 'end-turn) (turn *state*)))))))


(defun check-positions ()
  (loop for order in (copy-seq (orders *state*))
        for bot-id = (order-bot-id order)
        for row = (order-src-row order)
        for col = (order-src-col order)
        do (when (/= bot-id (pid (aref (game-map *state*) row col)))
             ;; TODO report row col dir
             (logmsg "Bot " bot-id " issued an order for a position it "
                     "doesn't occupy. Ignoring...~%")
             (logmsg "o: " order "~%")
             (logmsg "p: " (pid (aref (game-map *state*) row col)) "~%")
             ;; TODO use DELETE?
             (setf (orders *state*) (remove order (orders *state*))))))


(defun check-water ()
  (loop for order in (copy-seq (orders *state*))
        for bot-id = (order-bot-id order)
        for row = (order-src-row order)
        for col = (order-src-col order)
        for dir = (order-direction order)
        do (when (water? row col dir)
             ;; TODO report row col dir
             (logmsg "Bot " bot-id " ordered an ant into water. Ignoring...~%")
             ;; TODO use DELETE?
             (setf (orders *state*) (remove order (orders *state*))))))


(defun clear-dead-ants ()
  (loop for row from 0 below (rows *state*)
        do (loop for col from 0 below (cols *state*)
                 for tile = (aref (game-map *state*) row col)
                 do (when (and (typep tile 'ant) (dead tile))
                      (setf (aref (game-map *state*) row col) +land+)))))


(defun dir2key (direction)
  (cond ((equal direction "N") :north)
        ((equal direction "E") :east)
        ((equal direction "S") :south)
        ((equal direction "W") :west)))


(defun distance2 (row1 col1 row2 col2)
  (declare (inline * + - abs cols min rows vector)
           (optimize (speed 3))
           (type fixnum row1 col1 row2 col2))
  (let* ((drow (abs (- row1 row2)))
         (dcol (abs (- col1 col2)))
         (minrow (min drow (- (rows *state*) drow)))
         (mincol (min dcol (- (cols *state*) dcol))))
    (+ (* minrow minrow) (* mincol mincol))))


(defun do-turn (turn)
  (setf (orders *state*) nil)
  (revitalize-ants)
  (loop for bot across (bots *state*)
        for command-line = (command-line bot)
        for id = (bot-id bot)
        for proc = (process bot)
        for pin = (sb-ext:process-input proc)
        for pout = (sb-ext:process-output proc)
        do (if (equal :running (sb-ext:process-status proc))
               (send-game-state bot pin turn)
               (logmsg id ":" command-line " has stopped running...~%"))
           ;; TODO don't do this when bot has stopped running
           (let ((turn-start (wall-time)))
             (wait-for-output pout turn-start)
             (loop with end-loop = nil
                   until end-loop
                   do (cond ((no-turn-time-left-p turn-start)
                             (logmsg id ":" command-line " timed out.~%")
                             (setf end-loop t))
                            ((listen pout)
                             (let ((line (read-line pout nil)))
                               (when (starts-with line "go")
                                 (loop-finish))
                               (when (starts-with line "o ")
                                 (queue-ant-order id line))))))))
  (move-ants)
  (battle-resolution)
  (spawn-ants)
  (unless (equal :none (food-method *state*))
    (spawn-food)))


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
  (loop for bot across (bots *state*)
        for prev-turn-score = (aref (scores bot) (- (turn *state*) 1))
        do (vector-push-extend prev-turn-score (scores bot))))


(defun key2dir (key)
  (cond ((equal key :north) #\n)
        ((equal key :east)  #\e)
        ((equal key :south) #\s)
        ((equal key :west)  #\w)))


(defun log-new-turn-stats ()
  (when *verbose*
    (format (log-stream *state*) "turn ~4D stats: ant_count: [~A]~%"
            (turn *state*) (players-ant-count-string :sep ", "))
    (force-output (log-stream *state*))))


;; If needed for performance CHECK-POSITIONS and CHECK-WATER could be moved
;; into the loop.
(defun move-ants ()
  (clear-dead-ants)  ; TODO needed?
  (check-positions)
  (check-water)
  (check-collisions)
  (loop for order in (orders *state*)
        for bot-id = (order-bot-id order)
        for src-row = (order-src-row order)
        for src-col = (order-src-col order)
        for dst-row = (order-dst-row order)
        for dst-col = (order-dst-col order)
        for ant = (aref (game-map *state*) src-row src-col)
        do (vector-push-extend (key2dir (order-direction order)) (orders ant))
           (setf (slot-value ant 'row) dst-row
                 (slot-value ant 'col) dst-col
                 (aref (game-map *state*) src-row src-col) +land+)
           (unless (dead ant)
             (setf (aref (game-map *state*) dst-row dst-col) ant))
           (when (dead ant)
             (let ((bot (aref (bots *state*) bot-id)))
               (setf (slot-value bot 'ants) (remove ant (ants bot))))))
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
                   ((and (starts-with line "m ") (null (cols *state*)))
                    (errmsg "~&Map missing \"cols n\" line. Aborting...~%")
                    (quit 1))
                   ((and (starts-with line "m ") (null (rows *state*)))
                    (errmsg "~&Map missing \"rows n\" line. Aborting...~%")
                    (quit 1))
                   ((and (starts-with line "m ") (null (n-players *state*)))
                    (errmsg "~&Map missing \"players n\" line. Aborting...~%")
                    (quit 1))
                   ((and (starts-with line "m ")
                         (< (length (remainder)) (n-players *state*)))
                    (errmsg "~&Map needs " (n-players *state*) " players but "
                            "only " (length (remainder)) " were entered on "
                            "the command-line. Aborting...~%")
                    (quit 1))
                   ((and (starts-with line "m ") (null (game-map *state*)))
                    (setf (slot-value *state* 'game-map)
                          (make-array (list (rows *state*) (cols *state*))
                                      :initial-element 'abc123))
                    (parse-map-line (game-map *state*) line rows)
                    (incf rows))
                   ((starts-with line "m ")
                    (parse-map-line (game-map *state*) line rows)
                    (incf rows)))
          finally (when (/= rows (rows *state*))
                    (errmsg "~&Actual map rows (" rows ") not equal to "
                            "specified number of rows (" (rows *state*)
                            "). Aborting...~%")
                    (quit 1)))))


(defun parse-map-line (map-array string row)
  (when (/= (- (length string) 2) (cols *state*))
    (errmsg "~&Actual map columns (" (- (length string) 2) ") for this line "
            "not equal to specified number of~%columns (" (cols *state*) ") "
            "for this map. Aborting...~%")
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
                          (push food (slot-value *state* 'food))
                          food))
                   (otherwise
                    (let* ((pid (char2pid c))
                           (ant (make-instance 'ant :initial-row row
                                  :initial-col col :row row :col col :pid pid
                                  :start-turn 0)))
                      (push ant (slot-value (aref (bots *state*) pid) 'ants))
                      ant))))))


(defun play-game ()
  (loop for turn from 0 to (turns *state*)
        do (setf (slot-value *state* 'turn) turn)
           (log-new-turn-stats)
           (cond ((= turn 0) (send-initial-game-state))
                 ((> turn 0) (init-scores-for-new-turn)
                             (do-turn turn)))))


(defun players-ant-count-string (&key (sep " "))
  (with-output-to-string (s)
    (loop for bot across (bots *state*)
          for i from 1
          do (princ (length (ants bot)) s)
             (when (< i (length (bots *state*)))
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
    (loop for i from 0 below (n-players *state*)
          do (princ "0" f)
             (when (< i (- (n-players *state*) 1))
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
    (loop with food-length = (length (food *state*))
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
    (loop with food-length = (length (food *state*))
          for food in (reverse (food *state*))
          for i from 1
          do (format f "            [ ~D, ~D, ~D, ~D ]~A~%"
                     (row food)
                     (col food)
                     (start-turn food)
                     (+ (turns *state*) 1)
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


;; A hack to make to scores compatible with playgame.py.  (This is mainly
;; for debugging, running diffs and unit tests.)
(defun scores-compatibility-hack ()
  (loop for bot across (bots *state*)
        do (setf (aref (scores bot) 0) 0)))


(defun send-ant (stream row col bot-id tile)
  ;(format stream "~A ~D ~D ~D~%" (if (dead tile) "d" "a") row col
  ;        (cond ((= bot-id (pid tile)) 0)
  ;              ((< bot-id (pid tile)) (pid tile))
  ;              ((> bot-id (pid tile)) (+ (pid tile) 1)))))
  (write-string (if (dead tile) "d " "a ") stream)
  (princ row stream)
  (write-string " " stream)
  (princ col stream)
  (write-string " " stream)
  (princ (cond ((= bot-id (pid tile)) 0)
               ((< bot-id (pid tile)) (pid tile))
               ((> bot-id (pid tile)) (+ (pid tile) 1)))
         stream)
  (terpri stream))


(defun send-food (stream row col)
  ;(format stream "f ~D ~D~%" row col))
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
  (unless (elt (seen-by tile) id)
    (setf (elt (seen-by tile) id) t)
    ;(format stream "w ~D ~D~%" row col)))
    (write-string "w " stream)
    (princ row stream)
    (write-string " " stream)
    (princ col stream)
    (terpri stream)))


(defun send-game-state (bot stream turn)
  (send-turn stream turn)
  (loop with id = (bot-id bot)
        with vr2 = (view-radius2 *state*)
        with vr = (floor (sqrt vr2))
        for ant in (ants bot)
        for arow = (row ant)
        for acol = (col ant)
        do (loop for roff from (- arow vr) to (+ arow vr)
                 do (loop for coff from (- acol vr) to (+ acol vr)
                          for wrc = (wrapped-row-col roff coff)
                          for wrow = (elt wrc 0)
                          for wcol = (elt wrc 1)
                          for dist2 = (distance2 arow acol wrow wcol)
                          when (<= dist2 vr2) do
                            (let* ((tile (aref (game-map *state*) wrow wcol))
                                   (type (type-of tile)))
                              (case type
                                (water (send-water stream wrow wcol id tile))
                                (food (send-food stream wrow wcol))
                                (ant (send-ant stream wrow wcol id tile)))))))
  (send-go stream))


(defun send-initial-game-state ()
  (loop for bot across (bots *state*)
        for command-line = (command-line bot)
        for id = (bot-id bot)
        for proc = (process bot)
        for pin = (sb-ext:process-input proc)
        for pout = (sb-ext:process-output proc)
        do (if (equal :running (sb-ext:process-status proc))
               (progn
                 (format pin "turn 0~%loadtime ~D~%turntime ~D~%rows ~D~%cols ~D~%turns ~D~%viewradius2 ~D~%attackradius2 ~D~%spawnradius2 ~D~%ready~%"
                         (load-time *state*) (turn-time *state*) (rows *state*)
                         (cols *state*) (turns *state*)
                         (view-radius2 *state*)
                         (attack-radius2 *state*)
                         (spawn-radius2 *state*))
                 (force-output pin))
               (logmsg id ":" command-line " has stopped running...~%"))
           ;; TODO don't do this when bot has stopped running
           (let ((turn-start (wall-time)))
             (wait-for-output pout turn-start)
             (cond ((no-turn-time-left-p turn-start)
                    (logmsg id ":" command-line " timed out.~%"))
                   ((listen pout)
                    (let ((line (read-line pout nil)))
                      (unless (starts-with line "go")
                        (logmsg id ":" command-line " sent junk: " line
                                "~%"))))))))


(defun spawn-ants ()
  (loop with sr2 = (spawn-radius2 *state*)
        with sr = (floor (sqrt sr2))
        for food in (copy-seq (food *state*))  ; we're modifying (food *state*)
        for frow = (row food)
        for fcol = (col food)
        for ants = nil
        do (loop for roff from (- frow sr) to (+ frow sr)
                 do (loop for coff from (- fcol sr) to (+ fcol sr)
                         for tile = (tile-if-reachable sr2 frow fcol roff coff)
                         when tile
                           do (when (and (antp tile) (alivep tile))
                                (push tile ants))))
           (cond ;; spawn an ant
                 ((= (length ants) 1)
                  (setf (slot-value *state* 'food)
                        (remove food (food *state*)))
                  (let* ((pid (pid (first ants)))
                         (ant (make-instance 'ant :row frow :col fcol
                               :start-turn (start-turn food)
                               :conversion-turn (turn *state*)
                               :initial-row frow :initial-col fcol :pid pid)))
                    (setf (aref (game-map *state*) frow fcol) ant)
                    (push ant (slot-value (aref (bots *state*) pid) 'ants))))
                 ;; contested food, destroy it
                 ((> (length ants) 1)
                  (setf (slot-value *state* 'food)
                        (remove food (food *state*)))
                  (setf (aref (game-map *state*) frow fcol) +land+)))))


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
  (loop for command-line in (remainder)
        for proc = (run-program command-line)
        for bot = (make-instance 'bot :command-line command-line :process proc)
        do (vector-push-extend bot (bots *state*))))


(defun tile-if-reachable (radius2 src-row src-col dst-row dst-col)
  (let* ((wrc (wrapped-row-col dst-row dst-col))
         (wrow (elt wrc 0))
         (wcol (elt wrc 1)))
    (when (<= (distance2 src-row src-col wrow wcol) radius2)
      (aref (game-map *state*) wrow wcol))))


(defun turn-time-left-p (turn-start-time)
  (<= (- (wall-time) turn-start-time)
      (/ (turn-time *state*) 1000)))


(defun update-immobile-ant-orders ()
  (loop for bot across (bots *state*)
        do (loop for ant in (ants bot)
                 for orders = (orders ant)
                 when (< (length orders)
                         (- (turn *state*) (conversion-turn ant)))
                   do (vector-push-extend #\- orders))))


(defun wait-for-output (output turn-start-time)
  (loop until (or (listen output)
                  (no-turn-time-left-p turn-start-time))))


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
            :default-value "4"
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
  (let ((*state* (make-instance 'play-game-state :error-stream *debug-io*))
        (*verbose* nil)) ; set in PROCESS-COMMAND-LINE-OPTIONS
    (process-command-line-options)
    (start-bots)
    (parse-map (map-file *state*))
    (logmsg "running for " (turns *state*) " turns~%")
    (handler-bind (#+sbcl (sb-sys:interactive-interrupt #'user-interrupt))
                   ;(error #'error-handler))
      ;(sb-sprof:with-profiling (:loop nil :report :flat)
        (play-game)
      ;  )
      )
    (logmsg "score " (players-score-string) "~%")
    (logmsg "status " (players-status-string) "~%")
    (scores-compatibility-hack)
    (save-replay)
    (sleep (end-wait *state*))))
