#!/bin/sh

if [ -z "$KDB" ]; then
	KDB=kdb
fi

BENCHMARK=./build/bin/large
CURRTIME=`date +"%H-%M-%S_%m-%d-%Y"`
#LOGFILE="./doc/benchmark/logs/$(echo $KDB)_log_$CURRTIME.log"
RUNS=21

echo "Runtime as IEEE Std 1003.2-1992 (``POSIX.2'')"
echo "Doing $RUNS runs:"
i=0
while [ "$i" -lt "$RUNS" ] ; do
	WALLTIME=$( (time -p $BENCHMARK) 2>&1 )
	#echo $WALLTIME | awk -F"real" '{print $1;}' # print programs stdout
	echo $WALLTIME | awk -F"real" '{print $2;}' | awk '{print $1;}'
	i=$((i+1))
done
