/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef ELEKTRA_PLUGIN_TYPE_H
#define ELEKTRA_PLUGIN_TYPE_H

#include <stdbool.h>

#include <kdbplugin.h>
#include <kdbtypes.h>

#ifdef __cplusplus
extern "C" {
namespace ckdb
{
#endif

typedef struct _Type Type;

struct boolean_pair
{
	const char * trueValue;
	const char * falseValue;
};

typedef struct
{
	kdb_long_long_t booleanRestore;
	struct boolean_pair * booleans;
	kdb_long_long_t booleanCount;
} TypeData;

int elektraTypeOpen (Plugin * handle, ElektraKey * errorKey);
int elektraTypeGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraTypeSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraTypeClose (Plugin * handle, ElektraKey * errorKey);
int elektraTypeCheckConf (ElektraKey * errorKey, ElektraKeyset * conf);

bool elektraTypeCheckType (const ElektraKey * key);
bool elektraTypeValidateKey (Plugin * handle, ElektraKey * key, ElektraKey * errorKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#ifdef __cplusplus
}
};
#endif

#endif
