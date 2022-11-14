#!/bin/sh
#
# @brief Adds an entry to the rpm changelog file

USAGE=$(
	cat << EOF
Usage: [-h] [-a AUTHOR] [-f FILE] [-m MESSAGE] [-v VERSION] [-l VERSION_SUFFIX]
Adds a new entry to the rpm changelog.
  -a=AUTHOR     	set the author of the changelog.
                	Defaults to git config user.name and user.email
  -f=FILE       	path to the changelog.
                	Defaults to ./changelog.
  -m=MESSAGE    	add a custom changelog message.
                	Default is: New upstream version.
  -v=VERSION    	set the version of this changelog entry.
                	Default is: \$RVERSION.
  -l=VERSION_SUFFIX	add a suffix to the RPM version number for a local build.
  -p				print latest package version from changelog
  -h				display this help and exit.
EOF
)

FILE="changelog"
MESSAGE="New upstream version"
VERSION=$RVERSION

while getopts "hpa:m:v:f:l:" opt; do
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
	l)
		VERSION_SUFFIX="${OPTARG}"
		;;
	m)
		MESSAGE="${OPTARG}"
		;;
	p)
		PRINT_VERSION_FLAG="TRUE"
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

if [ -n "${PRINT_VERSION_FLAG}" ]; then
	grep -Po '(?<=\s-\s)\d.\d.*' "$FILE" | head -1
	exit
fi

if [ -z ${AUTHOR+x} ]; then
	AUTHOR="$(git config user.name) <$(git config user.email)>"
fi

DATE=$(date "+%a %b %d %Y")

# add optional suffix to version
if [ -n "${VERSION_SUFFIX}" ] && [ -z "${VERSION}" ]; then
	# suffix is set but no version, so the last version will be extracted from the changelog file
	PREVIOUS_VERSION="$(grep -Po '(?<=\s-\s)\d.\d.*' "$FILE" | head -1)"
	VERSION="$PREVIOUS_VERSION$VERSION_SUFFIX"
elif [ -n "${VERSION_SUFFIX}" ] && [ -n "${VERSION}" ]; then
	# suffix and version set
	VERSION="$VERSION$VERSION_SUFFIX"
fi

ENTRY_HEADER="* $DATE $AUTHOR - $VERSION"
ENTRY_LOG="- $MESSAGE"

sed -i "1s/^/$ENTRY_HEADER\n$ENTRY_LOG\n\n/" "$FILE"
