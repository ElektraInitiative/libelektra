#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA MERGE SCRIPTS TESTS
echo

check_version

if "$KDB" plugin-info storage provides 2> /dev/null | grep -q 'storage/ini'; then
	echo "This test does not work if Elektra uses the INI plugin as default storage"
	exit 0
fi

OURS_ROOT=$USER_ROOT/mergetest/ours
THEIRS_ROOT=$USER_ROOT/mergetest/theirs
BASE_ROOT=$USER_ROOT/mergetest/base
MERGED_ROOT=$USER_ROOT/mergetest/merged

echo "Testing trivial merging"

"$KDB" set $OURS_ROOT/key "init" > /dev/null
exit_if_fail "could not set"

"$KDB" set $THEIRS_ROOT/key "init" > /dev/null
exit_if_fail "could not set"

"$KDB" set $BASE_ROOT/key "init" > /dev/null
exit_if_fail "could not set"

"$KDB" ls $MERGED_ROOT

"$KDB" merge $OURS_ROOT $THEIRS_ROOT $BASE_ROOT $MERGED_ROOT
exit_if_fail "could not merge"

[ "x$("$KDB" ls $MERGED_ROOT/key 2> /dev/null)" = "x$MERGED_ROOT/key" ]
exit_if_fail "not exactly one result"

[ "x$("$KDB" get $MERGED_ROOT/key 2> /dev/null)" = "xinit" ]

"$KDB" rm -r $USER_ROOT/mergetest

echo "Testing conflict"

"$KDB" set $OURS_ROOT/key "ours" > /dev/null
exit_if_fail "could not set"

"$KDB" set $THEIRS_ROOT/key "theirs" > /dev/null
exit_if_fail "could not set"

"$KDB" set $BASE_ROOT/key "init" > /dev/null
exit_if_fail "could not set"

"$KDB" merge $OURS_ROOT $THEIRS_ROOT $BASE_ROOT $MERGED_ROOT 2> /dev/null
[ $? != 0 ]
succeed_if "Merging did not fail"

[ "x$("$KDB" ls $MERGED_ROOT/key 2> /dev/null)" = "x" ]
exit_if_fail "should be no result"

"$KDB" merge --strategy ours $OURS_ROOT $THEIRS_ROOT $BASE_ROOT $MERGED_ROOT
exit_if_fail "could not merge"

[ "x$("$KDB" ls $MERGED_ROOT/key 2> /dev/null)" = "x$MERGED_ROOT/key" ]
exit_if_fail "not exactly one result"

[ "x$("$KDB" get $MERGED_ROOT/key 2> /dev/null)" = "xours" ]
exit_if_fail "merge produced wrong result"

"$KDB" rm -r $MERGED_ROOT/key
"$KDB" rm -r $OURS_ROOT/key
"$KDB" rm -r $THEIRS_ROOT/key
"$KDB" rm -r $BASE_ROOT/key

echo "Testing metadata"

"$KDB" set $OURS_ROOT/key "init" > /dev/null
exit_if_fail "could not set"

"$KDB" meta-set $OURS_ROOT/key comment "init" > /dev/null
exit_if_fail "could not set meta"

"$KDB" set $THEIRS_ROOT/key "init" > /dev/null
exit_if_fail "could not set"

"$KDB" meta-set $THEIRS_ROOT/key comment "theirs" > /dev/null
exit_if_fail "could not set meta"

"$KDB" set $BASE_ROOT/key "init" > /dev/null
exit_if_fail "could not set"

"$KDB" meta-set $BASE_ROOT/key comment "base" > /dev/null
exit_if_fail "could not set meta"

"$KDB" merge $OURS_ROOT $THEIRS_ROOT $BASE_ROOT $MERGED_ROOT 2> /dev/null
[ $? != 0 ]
succeed_if "Merging did not failed"

[ "x$("$KDB" ls $MERGED_ROOT/key 2> /dev/null)" = "x" ]
exit_if_fail "should be no result"

"$KDB" merge --strategy ours $OURS_ROOT $THEIRS_ROOT $BASE_ROOT $MERGED_ROOT
exit_if_fail "could not merge"

[ "x$("$KDB" ls $MERGED_ROOT/key 2> /dev/null)" = "x$MERGED_ROOT/key" ]
exit_if_fail "not exactly one result"

[ "x$("$KDB" get $MERGED_ROOT/key 2> /dev/null)" = "xinit" ]

"$KDB" rm -r $USER_ROOT/mergetest

end_script
