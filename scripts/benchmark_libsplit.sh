#!/bin/sh

if [ -z "$KDB" ]; then
	KDB=kdb
fi

BENCHMARK=./build/bin/large
CURRTIME=`date +"%H-%M-%S_%m-%d-%Y"`
LOGFILE="./doc/benchmark/logs/$(echo $KDB)_log_$CURRTIME.log"


echo "Runtime as IEEE Std 1003.2-1992 (``POSIX.2'')" >> $LOGFILE
for run in {0..20}; do
	echo "RUN: #$run"
	WALLTIME=$((time -p $BENCHMARK) 2>&1)
	echo $WALLTIME | awk -F"real" '{print $1;}' # print programs stdout
	echo $WALLTIME | awk -F"real" '{print $2;}' | awk '{print $1;}' >> $LOGFILE
done