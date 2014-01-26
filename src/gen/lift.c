#include "lift.h"

#include <stdio.h>

int main()
{
	Key *parentKey = keyNew("user/sw/app/lift", KEY_END);
	KDB *kdb = kdbOpen(parentKey);
	KeySet *conf = ksNew(0);
	kdbGet(kdb, conf, parentKey);

	printf ("delay: %d, stops: %s, algorithm: %s, height: %f\n",
		get_user_sw_app_lift_emergency_delay(conf),
		bool_to_string(get_user_sw_app_lift_emergency_action_stops(conf)),
		algorithm_to_string(get_user_sw_app_lift_algorithm(conf)),
		get_user_sw_app_lift_floor_height(conf));

	set_user_sw_app_lift_floor_height(conf, 22.2399);

	kdbSet(kdb, conf, parentKey);
	ksDel(conf);
	kdbClose(kdb, parentKey);
	keyDel(parentKey);
	return 0;
}
