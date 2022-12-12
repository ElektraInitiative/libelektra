/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#compiler - settings
directiveStartToken = @cheetahVarStartToken = $
#end compiler - settings
					      @from util import util @from support.c import * @from support.genopt import * @set support =
	CSupport () $util.header ($args.output)
#ifdef __cplusplus
#include "genopt.hpp"
#else
#include "genopt.h"
#endif

#include <elektra/kdb.h>
#include <elektra/kdbtypes.h>

#include <unistd.h>

// for strcmp
#include <string.h>

// for strol
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef _GNU_SOURCE
#include <getopt.h>
#endif

#ifdef __cplusplus
		using namespace ckdb;

extern "C" {
#endif

const char * elektraGenHelpText (void)
{
	return
@for $key, $info in $parameters.items()
@if $info.get('opt/long'):
@if $info.get('opt'):
	"  -$info.get('opt')\t--$info.get('opt/long')\t$info.get('explanation')\n"
@else
	"     \t--$info.get('opt/long')\t$info.get('explanation')\n"
@end if
@else if $info.get('opt'):
	"  -$info.get('opt')\t    \t$info.get('explanation')\n"
@end if
@end for
	;
}

int ksGetOpt (int argc, char ** argv, KeySet * ks)
{
	int c;
	int retval = 0;
	opterr = 0;
	Key * found = 0;

@set counter = 502
#ifdef _GNU_SOURCE
	static struct option long_options[] = {
@for $key, $info in $parameters.items()
@if $info.get('opt/long'):
		{
			"$info.get('opt/long')",
@if $info.get('type') == 'boolean':
			no_argument,
@else:
			required_argument,
@end if
			NULL,
@if $info.get('opt')
			'$info.get('opt')'
@else
			$counter
@set info["opt"] = counter
@set counter = counter + 1
@end if
		},
@end if
@end for
		{
			"version",
			no_argument,
			NULL,
			500
		},
		{
			"help",
			no_argument,
			NULL,
			501
		}
	};


	while ((c = getopt_long (argc, argv,
#else
	while ((c = getopt (argc, argv,
#endif
@for $key, $info in $parameters.items()
@if $info.get('opt'):
@if $info.get('type') == 'boolean':
	@if not isinstance($info.get('opt'), int):
		"$info.get('opt')"
	@end if
@else:
	@if not isinstance($info.get('opt'), int):
		"$info.get('opt'):"
	@end if
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
		case 500:
			retval |= 1;
			break;
		case 501:
			retval |= 2;
			break;
@for $key, $info in $parameters.items()
	@if $info.get('opt'):
		@if isinstance($info.get('opt'), int):
			case $info.get("opt"):
		@else:
			case '$info.get("opt")':
		@end if
		@if $info.get('range')
{
	$support.typeof (info) check;
	char * endptr;
	errno = 0;
	check = strtol (optarg, &endptr, 10);
	if ((errno == ERANGE && (check == LONG_MAX || check == LONG_MIN)) || (errno != 0 && check == 0))
	{
		retval |= 4;
		break;
	}
	if (endptr == optarg)
	{
		retval |= 8;
		break;
	}
	if (check < $support.min (info))
	{
		retval |= 16;
		break;
	}
	if (check > $support.max (info))
	{
		retval |= 32;
		break;
	}
}
		@end if
		@if $support.isenum(info):
				if (!(
				   !strcmp(optarg, "${support.enumval(info)[0]}")
			@for $enum in $support.enumval(info)[1:]
				   || !strcmp(optarg, "$enum")
			@end for
				  ))
		{
			retval |= 64;
			break;
		}
		@end if found = ksLookupByName (ks, "$support.userkey(key)", 0);
		if (!found)
		{
			ksAppendKey (ks, keyNew ("$support.userkey(key)", KEY_VALUE, $optarg (info), KEY_END));
		}
		else
		{
			keySetString (found, $optarg (info));
		}
		break;
	@end if
@end for
			case '?':
				retval |= 128;
	break;
		default:
			retval |= 256;
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
// extern "C"
#endif
$util.footer ($args.output)
