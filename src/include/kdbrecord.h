/**
 * @file
 *
 * @brief Defines for record.
 *
 * @copyright BSD License (see doc/COPYING or https://www.libelektra.org)
 */
#ifndef ELEKTRA_KDBRECORD_H
#define ELEKTRA_KDBRECORD_H

#include <kdb.h>
#include <kdbtypes.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

void elektraRecordEnableRecording (KDB * handle, const Key * parentKey, Key * errorKey);
void elektraRecordDisableRecording (KDB * handle, Key * errorKey);
void elektraRecordClearSession (KDB * handle, Key * errorKey);

bool elektraRecordRecord (KDB * handle, KDB * sessionStorageHandle, KeySet * newKeys, Key * parentKey, Key * errorKey);
bool elektraRecordIsActive (KDB * handle);

#ifdef __cplusplus
}
}
#endif


#endif // ELEKTRA_KDBRECORD_H
