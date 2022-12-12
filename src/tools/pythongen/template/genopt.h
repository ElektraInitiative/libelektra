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
					      @from support.genopt import *
					      @from util import util $util.header ($args.output)
#include <elektra/kdb.h>


#ifdef __cplusplus
						      extern "C"
{
#endif

	/** Parse commandline options and append it to keyset
	 * \param argc the argument counter
	 * \param argv the argument string array
	 * \param ks the keyset to store the configuration to
	 * needs template_getopt.c
	 */
	int ksGetOpt (int argc, char ** argv,
#ifdef __cplusplus
		      ckdb::KeySet * ks
#else
		KeySet *ks
#endif
	);

	/** \return Help text to be used for --help
	 */
	const char * elektraGenHelpText ();

#ifdef __cplusplus
}
#endif

$util.footer ($args.output)
