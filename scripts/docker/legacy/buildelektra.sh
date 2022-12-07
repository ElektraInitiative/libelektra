#!/usr/bin/env sh

set -o errexit
set -o pipefail

__FILE="$(basename "$0")"

USAGE=$(
	cat << EOF
Usage: ${__FILE} [-h] [-b NAME] [-t DIR] [-j JOBS] TAG
Downloads the Elektra commit specified by TAG.
Then builds and installs Elektra.
  -b=NAME        do not install. build a .deb package with NAME instead.
  -s=SOURCE      download Elektra from another SOURCE.
                 Default is 'https://codeload.github.com/ElektraInitiative/libelektra/tar.gz/TAG'
  -t=DIR         download Elektra into DIR (relative to current working directory).
                 If DIR already exists, use sources from DIR.
                 Default DIR is './libelektra-TAG'.
  -j=JOBS        Number of simultaneous jobs/recipes executed by make (like make -j JOBS).
  -c=VERSION     Use clang. Set the version to use (either 5.0 or 3.8 in this container).
  -D=PARAMS      Pass additional parameters to cmake. Usage: -D="-DBINDINGS=cpp;glib".
  -h             display this help and exit.
EOF
)

while getopts "hb:s:t:j:c:D:" opt; do
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
	j)
		JOBS="${OPTARG}"
		;;
	c)
		CLANG_VERSION="${OPTARG}"
		;;
	D)
		CMAKE_PARAMS="${OPTARG}"
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
	curl --fail --silent --show-error "${SOURCE}" | tar xz --transform "s/libelektra-${TAG}/${DIR}/"
fi

# set compiler
if [ ! -z "${CLANG_VERSION}" ]; then
	export CC=clang-${CLANG_VERSION}
	export CXX=clang++-${CLANG_VERSION}
fi

# build Elektra
if [ ! -d "${DIR}/build" ]; then
	mkdir "${DIR}/build"
fi
cd "${DIR}/build"
cmake -DCMAKE_INSTALL_PREFIX=/usr "${CMAKE_PARAMS}" .. && make -j "${JOBS}"

if [ -z "${NAME}" ]; then
	# install elektra ..
	make install
else
	# .. or build package
	checkinstall -D -y --pkgname "${NAME}" --pkgsource "${SOURCE}" --install=no --fstrans --pakdir ../..
fi
