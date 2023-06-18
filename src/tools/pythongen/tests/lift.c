/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./lift.h"
#include "./genopt.h"

#include <stdio.h>

// The application (just print out some config values in this case)
int lift (KeySet * conf)
{
	kdb_boolean_t stops = get_test_lift_emergency_action_stops (conf);
	enum algorithm a = get_test_lift_algorithm (conf);
	kdb_boolean_t write = get_test_lift_write (conf);

	printf ("delay: " ELEKTRA_LONG_F "\n", get_test_lift_emergency_delay (conf));
	printf ("stops: %s\n", bool_to_string (stops));
	printf ("algorithm: %s\n", algorithm_to_string (a));
	printf ("height #3: %f\n", get_test_lift_floor_3_height (conf));
	printf ("write: %s\n", bool_to_string (write));
	printf ("limit: " ELEKTRA_LONG_F "\n", get_test_lift_limit (conf));
	printf ("number: %s\n", get_test_lift_emergency_action_calls_number (conf));

	// rewrite the same (does not change anything)
	set_test_lift_algorithm (conf, a);
	set_test_lift_emergency_action_stops (conf, stops);

	// set option to write out false
	set_test_lift_write (conf, 0);

	return write;
}

void kdbGetByName (KDB * kdb, KeySet * conf, Key * parentKey, char * where)
{
	keySetName (parentKey, "system");
	keyAddName (parentKey, where);
	elektraKdbGet (kdb, conf, parentKey);

	keySetName (parentKey, "user");
	keyAddName (parentKey, where);
	elektraKdbGet (kdb, conf, parentKey);
}

int main (int argc, char ** argv)
{
	Key * parentKey = keyNew ("", KEY_END);
	KDB * kdb = elektraKdbOpen (NULL, parentKey);
	KeySet * conf = ksNew (0, KS_END);

	// get all config files
	kdbGetByName (kdb, conf, parentKey, "/test/lift");
	kdbGetByName (kdb, conf, parentKey, "/test/material_lift");
	kdbGetByName (kdb, conf, parentKey, "/test/heavy_material_lift");
	kdbGetByName (kdb, conf, parentKey, "/test/person_lift");

	// get by params
	int retval = ksGetOpt (argc, argv, conf);
	if (retval & 1)
	{
		printf ("%s Version 0.1\n", argv[0]);
		return 0;
	}
	else if (retval & 2)
	{
		printf ("Usage: %s [OPTIONS]\n"
			"%s\n"
			"Example that demonstrates elektra gen parameters\n",
			argv[0], elektraGenHelpText ());
		return 0;
	}
	else if (retval != 0)
	{
		printf ("Error in parsing options %d\n", retval);
	}

	// write back to user:/test/lift what we got by commandline
	// that means overrides in *_lift are still active, but
	// fallbacks will be overriden.
	if (lift (conf))
	{
		printf ("Write out config\n");
		keySetName (parentKey, "user:/test/lift");
		elektraKdbSet (kdb, conf, parentKey);
	}

	ksDel (conf);
	elektraKdbClose (kdb, parentKey);
	keyDel (parentKey);
	return retval;
}
