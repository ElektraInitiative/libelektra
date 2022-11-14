#!/usr/bin/env bash
#
# @brief Extracts the licenses of a dep5 copyright file and
# 		 transforms them into RPM licenses format.

LICENSE_FILE=$1

LICENSES_WITHOUT_BSD=$(cat "$LICENSE_FILE" | grep "License:" | awk -F": " {' print $2'} | grep -v "^BSD.*" | sort | uniq | tr '\n' ',' | sed '$s/,$/\n/' | sed 's/,/ and /g')

declare -A arr

arr["Apache-2.0"]="ASL 2.0"
arr["BSL-1.0"]="Boost"
arr["OFL-1.1"]="OFL"
arr["Zlib"]="zlib"
arr["public-domain"]="Public Domain"

for key in ${!arr[@]}; do
	LICENSES_WITHOUT_BSD=$(echo "$LICENSES_WITHOUT_BSD" | sed "s/$key[^ ]*/${arr[${key}]}/g")
done

echo "BSD and $LICENSES_WITHOUT_BSD"
