#include "lift.h"

#include <stdio.h>

int main()
{
	Key *parentKey = keyNew("user/sw/app/lift", KEY_END);
	KDB *kdb = kdbOpen(parentKey);
	KeySet *conf = ksNew(0);
	kdbGet(kdb, conf, parentKey);

	printf ("delay: %d stops: %d, algorithm: %d\n",
		get_user_sw_app_lift_emergency_delay(conf),
		get_user_sw_app_lift_emergency_action_stops(conf),
		get_user_sw_app_lift_algorithm(conf));

	ksDel(conf);
	kdbClose(kdb, parentKey);
	keyDel(parentKey);
	return 0;
}
