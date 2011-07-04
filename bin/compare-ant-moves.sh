#!/bin/sh

BOT=$1;

if [ "$1" = "" ] && [ "$2" = "" ] ; then
    echo "Usage: compare-ant-moves.sh REPLAY-A REPLAY-B";
    exit 1;
fi

jsonpp $1 | grep "                \"[nesw-]" | sort > compare-ant-moves-a.replay-pp;
jsonpp $2 | grep "                \"[nesw-]" | sort > compare-ant-moves-b.replay-pp;

diff compare-ant-moves-a.replay-pp compare-ant-moves-b.replay-pp;
rm -f compare-ant-moves-a.replay-pp compare-ant-moves-b.replay-pp;
