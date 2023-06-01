#ifndef ELEKTRA_KDBDIFF_INTERNAL_H
#define ELEKTRA_KDBDIFF_INTERNAL_H

#include <elektra/core/types.h>

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

struct _ElektraDiff
{
	Key * parentKey;
	KeySet * addedKeys;
	/**
	 * stores the old versions of modified keys
	 */
	KeySet * modifiedKeys;
	KeySet * removedKeys;

	uint16_t refs;
};

struct _ElektraDiff * elektraDiffNew (KeySet * addedKeys, KeySet * removedKeys, KeySet * modifiedKey, Key * parentKey);



#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_KDBDIFF_INTERNAL_H
