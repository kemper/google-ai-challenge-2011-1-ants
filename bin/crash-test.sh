#!/bin/bash

BOT=$1;

if [ "$BOT" = "" ]; then
    echo "Usage: crash-test.sh BOT";
    exit 1;
fi

if [ `pwd | grep ants` != "" ] && [ ! -x playgame.py ] && [ ! -d maps ]; then
    echo "You should run this from the ~aichallenge/ants directory!";
    exit 1;
fi

CPU="python dist/sample_bots/python/LeftyBot.py";

for MAP in `find maps/{octagonal,symmetric}_maps/ -name \*.map`
do
    echo $MAP;
    STATUS=`./playgame.py --fill --nolaunch --verbose --log_dir game_logs --engine_seed 2 --player_seed 42 --turns 10 --map_file $MAP "$BOT" "$CPU" | grep status | cut -d\  -f 2`;
    if [ "$STATUS" != "survived" ]; then
        echo "$MAP: $STATUS";
    fi
done
