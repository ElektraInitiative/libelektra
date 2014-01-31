#from opt_support import *
#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
#end compiler-settings
// start of a generated file
#include "kdb.h"
#include <unistd.h>

int ksGetOpt(int argc, char **argv, KeySet *ks)
{
	int c;
	opterr = 0;
	Key *found = 0;

	while ((c = getopt (argc, argv,
@for $key, $info in $parameters.items()
@if $info.get('opt'):
"$info.get('opt'):"
@end if
@end for
		)) != -1)
	{
		switch (c)
		{
@for $key, $info in $parameters.items()
@if $info.get('opt'):
			case '$info.get("opt")':
				found = ksLookupByName(ks, "$key", 0);
				if(!found)
				{
					ksAppendKey(ks, keyNew("$userkey(key)",
							KEY_VALUE, optarg,
							KEY_END));
				}
				else
				{
					keySetString(found, optarg);
				}
				break;
@end if
@end for
			case '?':
				return 1;
			default:
				return 1;
/*
			case '?':
				if (optopt == 'c')
					fprintf (stderr, "Option -%c requires an argument.\n", optopt);
				else if (isprint (optopt))
					fprintf (stderr, "Unknown option `-%c'.\n", optopt);
				else
					fprintf (stderr,
							"Unknown option character `\\x%x'.\n",
							optopt);
				return 1;
			default:
				abort ();
*/
		}
	}
	return 0;
}
