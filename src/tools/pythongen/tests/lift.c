/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "lift.h"
#include "genopt.h"

#include <stdio.h>

// The application (just print out some config values in this case)
int lift (ElektraKeyset * conf)
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

void kdbGetByName (ElektraKdb * kdb, ElektraKeyset * conf, ElektraKey * parentKey, char * where)
{
	elektraKeySetName (parentKey, "system");
	elektraKeyAddName (parentKey, where);
	elektraKdbGet (kdb, conf, parentKey);

	elektraKeySetName (parentKey, "user");
	elektraKeyAddName (parentKey, where);
	elektraKdbGet (kdb, conf, parentKey);
}

int main (int argc, char ** argv)
{
	ElektraKey * parentKey = elektraKeyNew ("", ELEKTRA_KEY_END);
	ElektraKdb * kdb = elektraKdbOpen (NULL, parentKey);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);

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
		elektraKeySetName (parentKey, "user:/test/lift");
		elektraKdbSet (kdb, conf, parentKey);
	}

	elektraKeysetDel (conf);
	elektraKdbClose (kdb, parentKey);
	elektraKeyDel (parentKey);
	return retval;
}
