/**
 * @file
 *
 * @brief Convenience Methods for String Handling
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_UTILITY_STRING_H
#define ELEKTRA_UTILITY_STRING_H

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

char * elektraLskip (char const * const keyname);
char * elektraRstrip (char * const start, char ** end);
char * elektraStrip (char * text);

#ifdef __cplusplus
}
}
#endif


#endif // ELEKTRA_UTILITY_STRING_H
