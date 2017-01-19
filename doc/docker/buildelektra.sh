#!/bin/bash

set -o errexit
set -o pipefail

__FILE="$(basename "$0")"

USAGE=$(cat <<EOF
Usage: ${__FILE} [-h] [-b NAME] [-t DIR] TAG
Downloads the Elektra commit specified by TAG.
Then builds and installs Elektra.
  -b=NAME        do not install. build a .deb package with NAME instead.
  -s=SOURCE      download Elektra from another SOURCE.
                 Default is 'https://codeload.github.com/ElektraInitiative/libelektra/tar.gz/TAG'
  -t=DIR         download Elektra into DIR (relative to current working directory).
                 If DIR already exists, use sources from DIR.
                 Default DIR is './libelektra-TAG'.
  -h             display this help and exit.
EOF
)

while getopts "hb:s:t:" opt; do
	case $opt in
		h)
			echo "${USAGE}"
			exit
			;;
		b)
			NAME="${OPTARG}"
			;;
		s)
			SOURCE="${OPTARG}"
			;;
		t)
			DIR="${OPTARG}"
			;;
		:)
			printf "%s: missing argument for -%s\n" "${__FILE}" "${OPTARG}" >&2
			echo "${USAGE}"
			exit 1
			;;
		\?)
			printf "%s: illegal option: -%s\n" "${__FILE}" "${OPTARG}" >&2
			echo "${USAGE}"
			exit 1
			;;
		esac
done
shift $((OPTIND - 1))
if [ -z "${1}" ]; then
	printf "%s: no argument supplied" "${__FILE}" >&2
	echo "${USAGE}"
	exit 1
fi

TAG="${1}"
SOURCE="${SOURCE:-"https://codeload.github.com/ElektraInitiative/libelektra/tar.gz/${TAG}"}"
DIR="${DIR:-"libelektra-${TAG}"}"


# download Elektra
if [ ! -d "${DIR}" ]; then
	curl --fail --silent --show-error ${SOURCE} | tar xz --transform "s/libelektra-${TAG}/${DIR}/"
fi

# build Elektra
if [ ! -d "${DIR}/build" ]; then
	mkdir "${DIR}/build"
fi
cd "${DIR}/build"
cmake -DCMAKE_INSTALL_PREFIX=/usr .. && make -j

if [ -z "${NAME}" ]; then
	# install elektra ..
	make install
else
	# .. or build package
	checkinstall -D -y --pkgname "${NAME}" --pkgsource "${SOURCE}" --install=no --fstrans --pakdir ../..
fi
