#!/bin/sh
#
# @brief Straces kdb, runs tests and stores logs

# quit with error if any command fails
set -ex

BASE_DIR=$1
VERSION=$2
SUFFIX=$3
STATIC=${4:-"ON"}

log_strace() {
	CONTEXT=$1
	mkdir -p "$BASE_DIR"/"$VERSION"/"$CONTEXT"

	strace -o "$BASE_DIR"/"$VERSION"/"$CONTEXT"/mount.strace kdb mount file.ecf user:/release_test
	strace -o "$BASE_DIR"/"$VERSION"/"$CONTEXT"/file.strace kdb file user:/release_test/b
	strace -o "$BASE_DIR"/"$VERSION"/"$CONTEXT"/set.strace kdb set user:/release_test/b ""
	strace -o "$BASE_DIR"/"$VERSION"/"$CONTEXT"/get.strace kdb get user:/release_test/b
	strace -o "$BASE_DIR"/"$VERSION"/"$CONTEXT"/rm.strace kdb rm user:/release_test/b
	strace -o "$BASE_DIR"/"$VERSION"/"$CONTEXT"/umount.strace kdb umount user:/release_test
}

run_log_tests() {
	CONTEXT=$1
	mkdir -p "$BASE_DIR"/"$VERSION"/"$CONTEXT"

	KDB=kdb kdb run_all -v 2>&1 | tee "$BASE_DIR"/"$VERSION"/"$CONTEXT"/run_all
	check_test_amount "$BASE_DIR"/"$VERSION"/"$CONTEXT"/run_all

	KDB=kdb-full kdb-full run_all -v 2>&1 | tee "$BASE_DIR"/"$VERSION"/"$CONTEXT"/run_all_full
	check_test_amount "$BASE_DIR"/"$VERSION"/"$CONTEXT"/run_all_full

	if [ "$STATIC" = "ON" ]; then
		KDB=kdb-static kdb-static run_all -v 2>&1 | tee "$BASE_DIR"/"$VERSION"/"$CONTEXT"/run_all_static
		check_test_amount "$BASE_DIR"/"$VERSION"/"$CONTEXT"/run_all_static
	fi
}

check_test_amount() {
	TEST_LOG_PATH=$1

	# Check if there are really >=241 or >=131 tests
	TEST_COUNT=$(tail -n 1 "$TEST_LOG_PATH" | awk '/^run_all RESULTS: / { print $3 }')
	if [ "$TEST_COUNT" -lt "131" ]; then
		printf >&2 "Only $TEST_COUNT test(s) run"
		exit 1
	fi
}

log_strace "strace-${SUFFIX}"
run_log_tests "test-${SUFFIX}"
