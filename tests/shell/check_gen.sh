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
LIFT_MOUNTPOINT=/sw/lift
LIFT_MOUNTNAME=_sw_lift
$KDB mount $LIFT_FILE $LIFT_MOUNTPOINT ni 1>/dev/null
succeed_if "could not mount: $LIFT_FILE at $LIFT_MOUNTPOINT"

MATERIAL_LIFT_FILE=test_material_lift.ini
MATERIAL_LIFT_MOUNTPOINT=/sw/material_lift
MATERIAL_LIFT_MOUNTNAME=_sw_material_lift
$KDB mount $MATERIAL_LIFT_FILE $MATERIAL_LIFT_MOUNTPOINT ni 1>/dev/null
succeed_if "could not mount: $MATERIAL_LIFT_FILE at $MATERIAL_LIFT_MOUNTPOINT"

PERSON_LIFT_FILE=test_person_lift.ini
PERSON_LIFT_MOUNTPOINT=/sw/person_lift
PERSON_LIFT_MOUNTNAME=_sw_person_lift
$KDB mount $PERSON_LIFT_FILE $PERSON_LIFT_MOUNTPOINT ni 1>/dev/null
succeed_if "could not mount: $PERSON_LIFT_FILE at $PERSON_LIFT_MOUNTPOINT"

HEAVY_MATERIAL_LIFT_FILE=test_heavy_material_lift.ini
HEAVY_MATERIAL_LIFT_MOUNTPOINT=/sw/heavy_material_lift
HEAVY_MATERIAL_LIFT_MOUNTNAME=_sw_heavy_material_lift
$KDB mount $HEAVY_MATERIAL_LIFT_FILE $HEAVY_MATERIAL_LIFT_MOUNTPOINT ni 1>/dev/null
succeed_if "could not mount: $HEAVY_MATERIAL_LIFT_FILE at $HEAVY_MATERIAL_LIFT_MOUNTPOINT"

cd $GEN_FOLDER

make

./lift | grep "delay: 0, stops: true, algorithm: stay, height #3: 2.500000, write: false"
succeed_if "defaults not correct"

./lift -d 2 | grep "delay: 2, stops: true, algorithm: stay, height #3: 2.500000, write: false"
succeed_if "delay commandline parameter not working"

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
