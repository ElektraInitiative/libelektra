#include "lift.h"

#include <stdio.h>

int main(int argc, char**argv)
{
	Key *parentKey = keyNew("system/sw/lift", KEY_END);
	KDB *kdb = kdbOpen(parentKey);
	KeySet *conf = ksNew(0);

	// get by config
	kdbGet(kdb, conf, parentKey);
	keySetName(parentKey, "user/sw/lift");
	kdbGet(kdb, conf, parentKey);

	// get by params
	if (ksGetOpt(argc, argv, conf) != 0)
	{
		printf ("Error in parsing options\n");
	}

	int stops = get_sw_lift_emergency_action_stops(conf);
	enum algorithm a = get_sw_lift_algorithm(conf);
	int write = get_sw_lift_write(conf);

	printf ("delay: %d, stops: %s, algorithm: %s, height #3: %f, write: %s\n",
		get_sw_lift_emergency_delay(conf),
		bool_to_string(stops),
		algorithm_to_string(a),
		get_sw_lift_floor_3_height(conf),
		bool_to_string(write));

	// rewrite the same (does not change anything)
	set_sw_lift_algorithm(conf, a);
	set_sw_lift_emergency_action_stops(conf, stops);

	// set option to write out false
	set_sw_lift_write(conf, 0);

	// write back to user/ what we got by commandline
	if (write)
	{
		kdbSet(kdb, conf, parentKey);
	}

	ksDel(conf);
	kdbClose(kdb, parentKey);
	keyDel(parentKey);
	return 0;
}
