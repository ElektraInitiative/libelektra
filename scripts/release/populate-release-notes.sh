#!/bin/sh
#
# @brief Inserts hashsums of source archive and git statistics into release notes
#  	     and sets final name of release notes.
set -ex +f

KDB_VERSION=$1
ARCHIVE_DIR=$2

RELEASE_DIR=$(dirname "$0")
SCRIPTS_DIR=$(dirname "$RELEASE_DIR")
DOC_DIR=$(dirname "$SCRIPTS_DIR")/doc
NEWS_DIR="$DOC_DIR"/news
RELEASE_NOTE_PREPARATION_PATH="$NEWS_DIR/_preparation_next_release.md"

# KDB_VERSION_PATCH=$(echo "$KDB_VERSION" | grep -Po '\d+$')
CURRENT_DATE=$(date +'%Y-%m-%d')
RELEASE_NOTE_FILENAME="$CURRENT_DATE"_"$KDB_VERSION".md
RELEASE_NOTE_PATH="$NEWS_DIR/$RELEASE_NOTE_FILENAME"

# get previous release version from git tags
PREVIOUS_RELEASE=$(git tag -l --sort=version:refname | sed 's/v//' | grep -e '[0-9].[0-9].[0-9]*' | tail -n2 | head -n1)
# PREVIOUS_RELEASE_MAJOR_MINOR_VERSION=$(echo "$PREVIOUS_RELEASE" | grep -Po '^\d+.\d+')

generate_git_release_stats_minimal() {
	PREV=$(git tag --sort=version:refname --list | grep -e '[0-9].[0-9].[0-9]*' | tail -n2 | head -n1)
	CURRENT=$(git tag --sort=version:refname --list | grep -e '[0-9].[0-9].[0-9]*' | tail -n1)
	STATS=$("$SCRIPTS_DIR"/git-release-stats "$PREV" "$CURRENT")
	# extract necessary fields from stats
	NUM_AUTHORS=$(echo "$STATS" | grep -o Author | wc -l)
	FILES_CHANGED=$(echo "$STATS" | grep -Po "\d+ files changed" | grep -Po "\d+")
	INSERTIONS=$(echo "$STATS" | grep -Po "\d+ insertions" | grep -Po "\d+")
	DELETIONS=$(echo "$STATS" | grep -Po "\d+ deletions" | grep -Po "\d+")
	COMMITS=$(echo "$STATS" | grep -Po "Number commits: \d+" | grep -Po "\d+")
	echo "$NUM_AUTHORS $FILES_CHANGED $INSERTIONS $DELETIONS $COMMITS"
	# replace statistics placeholder with actual statistics
	STAT_RESULT="About $NUM_AUTHORS authors changed $FILES_CHANGED files with $INSERTIONS insertions(+) and $DELETIONS deletions(-) in $COMMITS commits."
	sed -i "s;<<\`scripts/git-release-stats $PREVIOUS_RELEASE.VER-1 $CURRENT\`>>;$STAT_RESULT;" "$RELEASE_NOTE_PATH"
}

generate_hashsums() {
	# generate hashsums of source archive and replace all newlines with a special symbol
	# this is necessary because otherwise the newline would get lost after the `sed` command
	HASHSUMS=$("$SCRIPTS_DIR"/generate-hashsums "$ARCHIVE_DIR"./elektra-"$KDB_VERSION".tar.gz | tr '\n' '∆')
	# replace hashsum placeholder with actual hashsums
	RELEASE_NOTES_HASHSUM_ESCAPED=$(sed "s;<<\`scripts/generate-hashsums elektra-$KDB_VERSION.tar.gz\`>>;$HASHSUMS;" "$RELEASE_NOTE_PATH")
	# re-replace special symbol with newlines and write result into release notes
	echo "$RELEASE_NOTES_HASHSUM_ESCAPED" | tr '∆' '\n' > "$RELEASE_NOTE_PATH"
}

update_alpine_release_image() {
	# update Alpine Linux image to new Elektra release
	sed -i "s/ELEKTRA_RELEASE=$PREVIOUS_RELEASE/ELEKTRA_RELEASE=$KDB_VERSION/g" "$SCRIPTS_DIR"/docker/alpine/*/release.Dockerfile
}

# copy release notes to new location
mv "$RELEASE_NOTE_PREPARATION_PATH" "$RELEASE_NOTE_PATH"
cp "$DOC_DIR"/todo/NEWS.md "$RELEASE_NOTE_PREPARATION_PATH"
# replace <<VERSION>> placeholder with actual version
sed -i "s/<<VERSION>>/$KDB_VERSION/g" "$RELEASE_NOTE_PATH"

generate_hashsums
generate_git_release_stats_minimal
update_alpine_release_image
