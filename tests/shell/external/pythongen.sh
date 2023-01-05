#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK PYTHONGEN
echo

echo "Deprecated and will be removed soon"
exit

if ! command -v realpath; then
	echo "realpath is not installed, will skip"
	exit 0
fi

if pkg-config elektra; then
	echo "Installed Elektra will be used"
else
	echo "Elektra or pkg-config not installed, will exit"
	exit
fi

check_version

if is_plugin_available ni; then
	echo "ni plugin available"
else
	echo "no ni plugin available"
	exit
fi

if /usr/bin/env python2 -V &> /dev/null; then
	echo "python available"
else
	echo "no python available"
	exit
fi

if /usr/bin/env python2 -c 'from Cheetah.Template import Template' 2> /dev/null; then
	echo "Cheetah available"
else
	echo "Cheetah not available"
	exit
fi

[ -z "${CC}" ] && CC=gcc
[ -z "${CXX}" ] && CXX=g++
if "${CC}" --version 2> /dev/null | grep -Eq '^gcc'; then
	echo "GCC available"
else
	echo "GCC not available"
	exit
fi

GEN_FOLDER="@CMAKE_SOURCE_DIR@/src/tools/pythongen"
GEN="$GEN_FOLDER/pythongen"

TESTPROGS="./lift ./cpplift ./nestedlift ./dynamiccontextlift"
# ./staticcontextlift commented out

if "$GEN" -h | grep "^usage:"; then
	echo "$GEN available"
else
	echo "$GEN does not work (e.g. cheetah missing)"
	exit 1
fi

LIFT_FILE=test_lift.ini
LIFT_USERROOT=user:/test/lift
LIFT_SYSTEMROOT=system:/test/lift
LIFT_MOUNTPOINT=/test/lift
"$KDB" mount $LIFT_FILE $LIFT_MOUNTPOINT ni 1> /dev/null
succeed_if "could not mount: $LIFT_FILE at $LIFT_MOUNTPOINT"

MATERIAL_LIFT_FILE=test_material_lift.ini
MATERIAL_LIFT_MOUNTPOINT=/test/material_lift
"$KDB" mount $MATERIAL_LIFT_FILE $MATERIAL_LIFT_MOUNTPOINT ni 1> /dev/null
succeed_if "could not mount: $MATERIAL_LIFT_FILE at $MATERIAL_LIFT_MOUNTPOINT"

PERSON_LIFT_FILE=test_person_lift.ini
PERSON_LIFT_MOUNTPOINT=/test/person_lift
"$KDB" mount $PERSON_LIFT_FILE $PERSON_LIFT_MOUNTPOINT ni 1> /dev/null
succeed_if "could not mount: $PERSON_LIFT_FILE at $PERSON_LIFT_MOUNTPOINT"

HEAVY_MATERIAL_LIFT_FILE=test_heavy_material_lift.ini
HEAVY_MATERIAL_LIFT_MOUNTPOINT=/test/heavy_material_lift
"$KDB" mount $HEAVY_MATERIAL_LIFT_FILE $HEAVY_MATERIAL_LIFT_MOUNTPOINT ni 1> /dev/null
succeed_if "could not mount: $HEAVY_MATERIAL_LIFT_FILE at $HEAVY_MATERIAL_LIFT_MOUNTPOINT"

cd "$GEN_FOLDER" || exit

BUILD_DIR="@CMAKE_BINARY_DIR@" make CC="${CC}" CXX="${CXX}"

echo "Test defaults"

for TESTPROG in $TESTPROGS; do
	$TESTPROG | grep "delay: 0"
	succeed_if "default in $TESTPROG of delay not correct"

	$TESTPROG | grep "stops: true"
	succeed_if "default in $TESTPROG of stops not correct"

	$TESTPROG | grep "height #3: 2.5"
	succeed_if "default in $TESTPROG of height #3 not correct"

	$TESTPROG | grep "limit: 1"
	succeed_if "default in $TESTPROG of limit not correct"
done

./lift | grep "algorithm: stay"
succeed_if "default of algorithm not correct"

echo "Test commandline arguments"

./lift -d 2 | grep "delay: 2"
succeed_if "delay commandline argument not working"

./lift --delay 2 | grep "delay: 2"
succeed_if "delay commandline argument not working"

./lift -s | grep "stops: false"
succeed_if "stops commandline argument not working"

./lift --stops | grep "stops: false"
succeed_if "stops commandline argument not working"

./lift --number zwei | grep "number: zwei"
succeed_if "number commandline argument not working"

./lift -a go_base_floor | grep "algorithm: go_base_floor"
succeed_if "algorithm commandline argument not working"

./lift -a stay | grep "algorithm: stay"
succeed_if "algorithm commandline argument not working"

./lift -a xxx | grep "Error in parsing options"
succeed_if "algorithm commandline argument not working"

./lift -h 5.5 | grep "height #3: 5.5"
succeed_if "height commandline argument not working"

./lift -l 12 | grep "limit: 12"
succeed_if "limit commandline argument not working"

echo "Test wrong commandline arguments"

./lift -d -2 | grep "Error in parsing options 16"
succeed_if "negative number accepted"

# ./lift -d no | grep "Error in parsing options 8"
# succeed_if "string as int accepted"

./lift -a ksks | grep "Error in parsing options 64"
succeed_if "wrong string as enum accepted"

echo "Test with keys in KDB"

SKEY=$LIFT_SYSTEMROOT/emergency/delay
UKEY=$LIFT_USERROOT/emergency/delay
VALUE=3
"$KDB" set "$SKEY" "$VALUE" 1> /dev/null
succeed_if "could not set $SKEY to value $VALUE"

[ "x$("$KDB" get $SKEY 2> /dev/null)" = "x$VALUE" ]
succeed_if "cant get $SKEY"

for TESTPROG in $TESTPROGS; do
	$TESTPROG | grep "delay: $VALUE"
	succeed_if "value of delay not $VALUE for $TESTPROG"
done

./lift -d 2 | grep "delay: 2"
succeed_if "delay commandline parameter not working"

for TESTPROG in $TESTPROGS; do
	$TESTPROG | grep "delay: $VALUE"
	succeed_if "value of delay not $VALUE for $TESTPROG"
done

echo "test writeback to user using -w"

"$KDB" get $UKEY 1> /dev/null
[ $? != "0" ]
succeed_if "got nonexisting key $UKEY"

./lift -d 4 -w | grep "delay: 4"
succeed_if "delay commandline parameter with writeback not working"

[ "x$("$KDB" get $UKEY 2> /dev/null)" = "x4" ]
succeed_if "cant get $UKEY (writeback problem)"

[ "x$("$KDB" get $SKEY 2> /dev/null)" = "x$VALUE" ]
succeed_if "cant get $SKEY with $VALUE (writeback)"

for TESTPROG in $TESTPROGS; do
	$TESTPROG | grep "delay: 4"
	succeed_if "writeback  in $TESTPROG was not permenent"
done

"$KDB" rm "$SKEY" 1> /dev/null
succeed_if "cannot rm $SKEY"

"$KDB" rm "$UKEY" 1> /dev/null
succeed_if "cannot rm $UKEY"

echo "test override with limit"

UKEY=$LIFT_USERROOT/limit
OKEY=system:/test/material_lift/limit
VALUE=33
"$KDB" get $UKEY 1> /dev/null
[ $? != "0" ]
succeed_if "got nonexisting key $UKEY"

"$KDB" get $OKEY 1> /dev/null
[ $? != "0" ]
succeed_if "got nonexisting key $OKEY"

./lift -l 22 | grep "limit: 22"
succeed_if "limit commandline parameter without writeback not working"

./lift | grep "limit: 1"
succeed_if "changed without writeback"

./lift -l 81 | grep "limit: 1"
succeed_if "limit commandline above limit not working (with default)"

./lift -l 81 | grep "Error in parsing options"
succeed_if "limit commandline validation was not detected"

./lift -l 0 | grep "limit: 1"
succeed_if "limit commandline below limit not working (with default)"

./lift -l 0 | grep "Error in parsing options"
succeed_if "limit commandline validation was not detected"

./lift -l -1 | grep "limit: 1"
succeed_if "limit commandline negative not working (with default)"

./lift -l -1 | grep "Error in parsing options"
succeed_if "limit commandline validation was not detected"

./lift -l 22 -w | grep "limit: 22"
succeed_if "limit commandline parameter with writeback not working"

[ "x$("$KDB" get $UKEY 2> /dev/null)" = "x22" ]
succeed_if "cant get $UKEY (writeback problem)"

for TESTPROG in $TESTPROGS; do
	$TESTPROG | grep "limit: 22"
	succeed_if "writeback  in $TESTPROG was not permenent"
done

./lift -l 81 | grep "limit: 22"
succeed_if "limit commandline above limit not working (with param)"

./lift -l 0 | grep "limit: 22"
succeed_if "limit commandline below limit not working (with param)"

./lift -l -1 | grep "limit: 22"
succeed_if "limit commandline negativ not working (with param)"

"$KDB" set "$OKEY" "$VALUE" 1> /dev/null
succeed_if "could not set $OKEY to value $VALUE"

for TESTPROG in $TESTPROGS; do
	$TESTPROG | grep "limit: $VALUE"
	succeed_if "override  in $TESTPROG value $VALUE not found"
done

./lift -l 22 -w | grep "limit: $VALUE"
succeed_if "override was not in favour to commandline parameter"

[ "x$("$KDB" get $UKEY 2> /dev/null)" = "x22" ]
succeed_if "cant get $UKEY which will not be used"

for TESTPROG in $TESTPROGS; do
	$TESTPROG | grep "limit: $VALUE"
	succeed_if "override  in $TESTPROG was not in favour to writeback"
done

"$KDB" rm "$OKEY" 1> /dev/null
succeed_if "cannot rm $OKEY"

"$KDB" rm "$UKEY" 1> /dev/null
succeed_if "cannot rm $UKEY"

echo "test fallback with height"

./lift | grep "height #3: 2.5"
succeed_if "default of height #3 not correct"

VALUE=8.5
#the concrete (user) key
KKEY=user:/test/lift/floor/#3/height
#the fallback keys (user+system
UKEY=user:/test/lift/floor/height
SKEY=system:/test/lift/floor/height

"$KDB" set "$SKEY" "$VALUE" 1> /dev/null
succeed_if "could not set $SKEY to value $VALUE"

./lift | grep "height #3: $VALUE"
succeed_if "fallback of height $VALUE was not used"

VALUE=9.5
"$KDB" set "$UKEY" "$VALUE" 1> /dev/null
succeed_if "could not set $UKEY to value $VALUE"

for TESTPROG in $TESTPROGS; do
	$TESTPROG | grep "height #3: $VALUE"
	succeed_if "fallback in $TESTPROG of height $VALUE was not used"
done

./lift -h 14.4 | grep "height #3: 14.4"
succeed_if "commandline parameter did not overwrite fallback"

./lift -h 14.4 -w | grep "height #3: 14.4"
succeed_if "commandline parameter did not overwrite fallback"

[ "x$("$KDB" get $KKEY 2> /dev/null)" = "x14.4" ]
succeed_if "cant get $KKEY which was written back"

VALUE=18.8
"$KDB" set "$KKEY" "$VALUE" 1> /dev/null
succeed_if "could not set $KKEY to value $VALUE"

for TESTPROG in $TESTPROGS; do
	$TESTPROG | grep "height #3: $VALUE"
	succeed_if "fallback  in $TESTPROG of height $VALUE was not used"
done

"$KDB" rm "$KKEY" 1> /dev/null
succeed_if "cannot rm $KKEY"

"$KDB" rm "$SKEY" 1> /dev/null
succeed_if "cannot rm $SKEY"

"$KDB" rm "$UKEY" 1> /dev/null
succeed_if "cannot rm $UKEY"

make clean

"$KDB" umount $LIFT_MOUNTPOINT > /dev/null
succeed_if "could not umount $LIFT_MOUNTPOINT"

"$KDB" umount $MATERIAL_LIFT_MOUNTPOINT > /dev/null
succeed_if "could not umount $MATERIAL_LIFT_MOUNTPOINT"

"$KDB" umount $PERSON_LIFT_MOUNTPOINT > /dev/null
succeed_if "could not umount $PERSON_LIFT_MOUNTPOINT"

"$KDB" umount $HEAVY_MATERIAL_LIFT_MOUNTPOINT > /dev/null
succeed_if "could not umount $HEAVY_MATERIAL_LIFT_MOUNTPOINT"

rm -f "$USER_FOLDER"/test_*lift.ini
rm -f "$SYSTEM_FOLDER"/test_*lift.ini

end_script gen
