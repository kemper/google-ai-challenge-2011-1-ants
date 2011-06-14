#!/bin/sh

bin/run-play-game.sbcl -v --food none --turns 40 --map_file ../../aichallenge/ants/maps/symmetric_maps/symmetric_10.map "python ../../aichallenge/ants/dist/sample_bots/python/LeftyBot.py" "python ../../aichallenge/ants/dist/sample_bots/python/LeftyBot.py" "python ../../aichallenge/ants/dist/sample_bots/python/LeftyBot.py" "python ../../aichallenge/ants/dist/sample_bots/python/LeftyBot.py";
