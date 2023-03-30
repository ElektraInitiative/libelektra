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

void elektraRecordSetup (void);

void elektraRecordEnableRecording (KDB * handle, const Key * parentKey, Key * errorKey);
void elektraRecordDisableRecording (KDB * handle, Key * errorKey);
void elektraRecordClearSession (KDB * handle, Key * errorKey);

#ifdef __cplusplus
}
}
#endif


#endif // ELEKTRA_KDBRECORD_H
