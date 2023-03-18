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

#include <elektra/plugin/plugin.h>
#include <elektra/type/types.h>

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

int elektraTypeOpen (Plugin * handle, Key * errorKey);
int elektraTypeGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraTypeSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraTypeClose (Plugin * handle, Key * errorKey);
int elektraTypeCheckConf (Key * errorKey, KeySet * conf);

bool elektraTypeCheckType (const Key * key);
bool elektraTypeValidateKey (Plugin * handle, Key * key, Key * errorKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#ifdef __cplusplus
}
};
#endif

#endif
