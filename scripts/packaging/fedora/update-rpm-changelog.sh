#!/bin/sh
#
# @brief Adds an entry to the rpm changelog file

USAGE=$(
	cat << EOF
Usage: [-h] [-a AUTHOR] [-f FILE] [-m MESSAGE] [-v VERSION]
Adds a new entry to the rpm changelog.
  -a=AUTHOR     set the author of the changelog. 
                Defaults to git config user.email
  -f=FILE       path to the changelog.
                Defaults to ./changelog.
  -m=MESSAGE    add a custom changelog message.
                Default is: New upstream version.
  -v=VERSION    set the version of this changelog entry.
                Default is: \$RVERSION.
  -h            display this help and exit.
EOF
)

FILE="changelog"
MESSAGE="New upstream version"
VERSION=$RVERSION

while getopts "ha:m:v:f:" opt; do
	case $opt in
	h)
		echo "${USAGE}"
		exit
		;;
	a)
		AUTHOR="${OPTARG}"
		;;
	f)
		FILE="${OPTARG}"
		;;
	m)
		MESSAGE="${OPTARG}"
		;;
	v)
		VERSION="${OPTARG}"
		;;
	:)
		printf "%s: missing argument for -%s\n" "${OPTARG}" >&2
		echo "${USAGE}"
		exit 1
		;;
	\?)
		printf "%s: illegal option: -%s\n" "${OPTARG}" >&2
		echo "${USAGE}"
		exit 1
		;;
	esac
done

if [ -z ${var+x} ]; then
	AUTHOR="$(git config user.email)"
fi

DATE=$(date "+%a %b %d %Y")

ENTRY_HEADER="* $DATE $AUTHOR - $VERSION"
ENTRY_LOG="- $MESSAGE"

sed -i "1s/^/$ENTRY_HEADER\n$ENTRY_LOG\n\n/" $FILE
