#include "lift.h"

#include <stdio.h>

// The application (just print out some config values in this case)
int lift(KeySet *conf)
{
	int stops = get_test_lift_emergency_action_stops(conf);
	enum algorithm a = get_test_lift_algorithm(conf);
	int write = get_test_lift_write(conf);

	printf ("delay: %d\n", get_test_lift_emergency_delay(conf));
	printf ("stops: %s\n", bool_to_string(stops));
	printf ("algorithm: %s\n", algorithm_to_string(a));
	printf ("height #3: %f\n", get_test_lift_floor_3_height(conf));
	printf ("write: %s\n", bool_to_string(write));

	// rewrite the same (does not change anything)
	set_test_lift_algorithm(conf, a);
	set_test_lift_emergency_action_stops(conf, stops);

	// set option to write out false
	set_test_lift_write(conf, 0);

	return write;
}

void kdbGetByName(KDB *kdb, KeySet *conf, Key *parentKey, char *where)
{
	keySetName(parentKey, "system");
	keyAddBaseName(parentKey, where);
	kdbGet(kdb, conf, parentKey);

	keySetName(parentKey, "user");
	keyAddBaseName(parentKey, where);
	kdbGet(kdb, conf, parentKey);
}

int main(int argc, char**argv)
{
	Key *parentKey = keyNew("", KEY_END);
	KDB *kdb = kdbOpen(parentKey);
	KeySet *conf = ksNew(0);

	// get all config files
	kdbGetByName(kdb, conf, parentKey, "/test/lift");
	kdbGetByName(kdb, conf, parentKey, "/test/material_lift");
	kdbGetByName(kdb, conf, parentKey, "/test/heavy_material_lift");
	kdbGetByName(kdb, conf, parentKey, "/test/person_lift");

	// get by params
	if (ksGetOpt(argc, argv, conf) != 0)
	{
		printf ("Error in parsing options\n");
	}

	// write back to user/test/lift what we got by commandline
	// that means overrides in *_lift are still active, but
	// fallbacks will be overriden.
	if (lift(conf))
	{
		keySetName(parentKey, "user/test/lift");
		kdbSet(kdb, conf, parentKey);
	}

	ksDel(conf);
	kdbClose(kdb, parentKey);
	keyDel(parentKey);
	return 0;
}
