/**
 * @file
 *
 * @brief types for extra functions exported by default resolver `resolver`
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_RESOLVER_SHARED_H
#define ELEKTRA_RESOLVER_SHARED_H

#include <elektra/core/namespace.h>
#include <elektra/core/types.h>

typedef struct
{
	char * relPath;
	char * dirname;
	char * fullPath;
	char * tmpFile;
} ElektraResolved;

typedef enum
{
	ELEKTRA_RESOLVER_TEMPFILE_NONE,
	ELEKTRA_RESOLVER_TEMPFILE_SAMEDIR,
	ELEKTRA_RESOLVER_TEMPFILE_TMPDIR,
} ElektraResolveTempfile;

typedef ElektraResolved * (*elektraResolveFileFunc) (elektraNamespace, const char *, ElektraResolveTempfile, Key *);
typedef void (*elektraFreeResolvedFunc) (ElektraResolved *);

#endif // ELEKTRA_RESOLVER_SHARED_H
