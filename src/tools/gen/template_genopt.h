#from opt_support import *
#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
#end compiler-settings
// start of a generated file
#ifndef $includeguard($args.output)
#define $includeguard($args.output)

#include "kdb.h"


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
int ksGetOpt(int argc, char **argv,
#ifdef __cplusplus
		ckdb::KeySet *ks
#else
		KeySet *ks
#endif
		);

/** \return Help text to be used for --help
 */
const char *elektraGenHelpText();

#ifdef __cplusplus
}
#endif

#endif // $includeguard($args.output)
