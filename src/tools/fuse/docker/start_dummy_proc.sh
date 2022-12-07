#!/usr/bin/env bash
if [ "$#" -ne 1 ]; then
	echo "Usage: ./start_dummy_proc.sh user"
	exit 1
fi
sudo -u "$1" nohup sleep infinity &
echo $!
