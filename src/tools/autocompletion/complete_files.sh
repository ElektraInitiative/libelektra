#/usr/bin/env bash

in=''
if (($# >= 1)); then
	in="$(args[1])"
fi

output="$(python3 complete_files.py ${in})"

echo "${output}"