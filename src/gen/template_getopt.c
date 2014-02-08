#from opt_support import *
#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
#end compiler-settings
// start of a generated file
#include "kdb.h"
#include <unistd.h>

// for strcmp
#include <string.h>

// for strol
#include <stdlib.h>
#include <limits.h>
#include <stdio.h>
#include <errno.h>

#ifdef _GNU_SOURCE
#include <getopt.h>
#endif

#ifdef __cplusplus
using namespace ckdb;

extern "C"
{
#endif

int ksGetOpt(int argc, char **argv, KeySet *ks)
{
	int c;
	int retval = 0;
	opterr = 0;
	Key *found = 0;

#ifdef _GNU_SOURCE
	static struct option long_options[] = {
@for $key, $info in $parameters.items()
@if $info.get('opt/long'):
		{
			"$info.get('opt/long')", 
@if $info.get('type') == 'bool':
			no_argument,
@else:
			required_argument,
@end if
			NULL,
			'$info.get('opt')'
		},
@end if
@end for
	};


	while ((c = getopt_long (argc, argv,
#else
	while ((c = getopt (argc, argv,
#endif
@for $key, $info in $parameters.items()
@if $info.get('opt'):
@if $info.get('type') == 'bool':
		"$info.get('opt')"
@else:
		"$info.get('opt'):"
@end if
@end if
@end for
#ifdef _GNU_SOURCE
		, long_options, NULL
#endif
		)) != -1)
	{
		switch (c)
		{
@for $key, $info in $parameters.items()
	@if $info.get('opt'):
			case '$info.get("opt")':
		@if $info.get('range')
				{
					$typeof(info) check;
					char *endptr;
					errno = 0;
					check = strtol(optarg, &endptr, 10);
					if ((errno == ERANGE
							&& (check == LONG_MAX || check == LONG_MIN))
							|| (errno != 0 && check == 0))
					{
						retval = 5;
						break;
					}
					if (endptr == optarg)
					{
						retval = 6;
						break;
					}
					if (check < $min(info))
					{
						retval = 3;
						break;
					}
					if (check > $max(info))
					{
						retval = 4;
						break;
					}
				}
		@end if
		@if $isenum(info):
				if(!(
				   !strcmp(optarg, "${enumval(info)[0]}")
			@for $enum in $enumval(info)[1:]
				   || !strcmp(optarg, "$enum")
			@end for
				  ))
				{
					retval = 7;
					break;
				}
		@end if
				found = ksLookupByName(ks, "$userkey(key)", 0);
				if(!found)
				{
					ksAppendKey(ks, keyNew("$userkey(key)",
							KEY_VALUE, $optarg(info),
							KEY_END));
				}
				else
				{
					keySetString(found, $optarg(info));
				}
				break;
	@end if
@end for
			case '?':
				retval = 1;
				break;
			default:
				retval = 2;
				break;
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
*/
		}
	}
	return retval;
}

#ifdef __cplusplus
}
//extern "C"
#endif
