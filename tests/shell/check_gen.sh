@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK GEN
echo

check_version

if is_plugin_available ni
then
	echo "ni plugin available"
else
	echo "no ni plugin available"
	exit
fi

if test -x /usr/bin/python
then
	echo "python available"
else
	echo "no python available"
	exit
fi

GEN_FOLDER=@CMAKE_SOURCE_DIR@/src/gen
GEN="/usr/bin/python $GEN_FOLDER/gen.py"

if $GEN | grep "^Usage:"
then
	echo "gen available"
else
	echo "no gen available (e.g. cheetah missing)"
	exit
fi

LIFT_FILE=test_lift.ini
LIFT_USERROOT=user/test/lift
LIFT_SYSTEMROOT=system/test/lift
LIFT_MOUNTPOINT=/test/lift
LIFT_MOUNTNAME=_test_lift
$KDB mount $LIFT_FILE $LIFT_MOUNTPOINT ni 1>/dev/null
succeed_if "could not mount: $LIFT_FILE at $LIFT_MOUNTPOINT"

MATERIAL_LIFT_FILE=test_material_lift.ini
MATERIAL_LIFT_MOUNTPOINT=/test/material_lift
MATERIAL_LIFT_MOUNTNAME=_test_material_lift
$KDB mount $MATERIAL_LIFT_FILE $MATERIAL_LIFT_MOUNTPOINT ni 1>/dev/null
succeed_if "could not mount: $MATERIAL_LIFT_FILE at $MATERIAL_LIFT_MOUNTPOINT"

PERSON_LIFT_FILE=test_person_lift.ini
PERSON_LIFT_MOUNTPOINT=/test/person_lift
PERSON_LIFT_MOUNTNAME=_test_person_lift
$KDB mount $PERSON_LIFT_FILE $PERSON_LIFT_MOUNTPOINT ni 1>/dev/null
succeed_if "could not mount: $PERSON_LIFT_FILE at $PERSON_LIFT_MOUNTPOINT"

HEAVY_MATERIAL_LIFT_FILE=test_heavy_material_lift.ini
HEAVY_MATERIAL_LIFT_MOUNTPOINT=/test/heavy_material_lift
HEAVY_MATERIAL_LIFT_MOUNTNAME=_test_heavy_material_lift
$KDB mount $HEAVY_MATERIAL_LIFT_FILE $HEAVY_MATERIAL_LIFT_MOUNTPOINT ni 1>/dev/null
succeed_if "could not mount: $HEAVY_MATERIAL_LIFT_FILE at $HEAVY_MATERIAL_LIFT_MOUNTPOINT"



cd $GEN_FOLDER

make

echo "Test defaults"

./lift | grep "delay: 0"
succeed_if "default of delay not correct"

./cpplift | grep "delay: 0"
succeed_if "default of delay not correct"

./lift | grep "stops: true"
succeed_if "default of stops not correct"

./lift | grep "algorithm: stay"
succeed_if "default of algorithm not correct"

./lift | grep "height #3: 2.5"
succeed_if "default of height #3 not correct"

./lift -d 2 | grep "delay: 2"
succeed_if "delay commandline parameter not working"

SKEY=$LIFT_SYSTEMROOT/emergency/delay
UKEY=$LIFT_USERROOT/emergency/delay
VALUE=3
$KDB set "$SKEY" "$VALUE" 1>/dev/null
succeed_if "could not set $SKEY to value $VALUE"

[ "x`$KDB get $SKEY 2> /dev/null`" = "x$VALUE" ]
succeed_if "cant get $SKEY"

./lift | grep "delay: $VALUE"
succeed_if "value of delay not $VALUE"

./cpplift | grep "delay: $VALUE"
succeed_if "value of delay not $VALUE"

./lift -d 2 | grep "delay: 2"
succeed_if "delay commandline parameter not working"

./lift | grep "delay: $VALUE"
succeed_if "value of delay not $VALUE"

./cpplift | grep "delay: $VALUE"
succeed_if "value of delay not $VALUE"

echo "test writeback to user using -w"

$KDB get $UKEY 1> /dev/null
[ $? != "0" ]
succeed_if "got nonexisting key $UKEY"

./lift -d 4 -w true | grep "delay: 4"
succeed_if "delay commandline parameter not working"

$KDB ls user/test

[ "x`$KDB get $UKEY 2> /dev/null`" = "x4" ]
succeed_if "cant get $UKEY"

$KDB rm "$SKEY" 1>/dev/null
succeed_if "cannot rm $SKEY"



$KDB umount $LIFT_MOUNTNAME >/dev/null
succeed_if "could not umount $LIFT_MOUNTNAME"

$KDB umount $MATERIAL_LIFT_MOUNTNAME >/dev/null
succeed_if "could not umount $MATERIAL_LIFT_MOUNTNAME"

$KDB umount $PERSON_LIFT_MOUNTNAME >/dev/null
succeed_if "could not umount $PERSON_LIFT_MOUNTNAME"

$KDB umount $HEAVY_MATERIAL_LIFT_MOUNTNAME >/dev/null
succeed_if "could not umount $HEAVY_MATERIAL_LIFT_MOUNTNAME"

rm -f $USER_FOLDER/test_*lift.ini
rm -f $SYSTEM_FOLDER/test_*lift.ini

end_script gen
