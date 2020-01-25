#/usr/bin/env bash

in=''
if ! [ -z "$1" ]; then
	in="$1"
fi

output="$(python3 complete_files.py ${in})"
echo "${output}"
