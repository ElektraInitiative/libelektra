#ifndef ELEKTRA_KDBCHANGETRACKING_H
#define ELEKTRA_KDBCHANGETRACKING_H

#include <kdb.h>
#include <kdbtypes.h>
#include <kdbplugin.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

typedef struct _ChangeTrackingContext ChangeTrackingContext;
typedef struct _ElektraDiff ElektraDiff;

const ChangeTrackingContext * elektraChangeTrackingGetContextFromKdb (KDB * kdb);
const ChangeTrackingContext * elektraChangeTrackingGetContextFromPlugin (Plugin * plugin);

ElektraDiff * elektraChangeTrackingCalculateDiff (KeySet * newKeys, const ChangeTrackingContext * context, Key * parentKey);

ElektraDiff * elektraDiffCalculate (KeySet * newKeys, KeySet * oldKeys, Key * parentKey);

void elektraDiffDel (ElektraDiff * ksd);
ElektraDiff * elektraDiffNew (KeySet * addedKeys, KeySet * removedKeys, KeySet * modifiedKey, Key * parentKey);
uint16_t elektraDiffIncRef (ElektraDiff * ksd);
uint16_t elektraDiffDecRef (ElektraDiff * ksd);

const Key * elektraDiffGetParentKey (const ElektraDiff * ksd);

KeySet * elektraDiffGetAddedKeys (const ElektraDiff * ksd);
KeySet * elektraDiffGetRemovedKeys (const ElektraDiff * ksd);
KeySet * elektraDiffGetModifiedKeys (const ElektraDiff * ksd); // Returns old keys (pre-modification)

bool elektraDiffKeyValueChanged (const ElektraDiff * ksd, Key * key);
bool elektraDiffKeyMetaChanged (const ElektraDiff * ksd, Key * key);

KeySet * elektraDiffGetAddedMetaKeys (const ElektraDiff * ksd, Key * key);
KeySet * elektraDiffGetRemovedMetaKeys (const ElektraDiff * ksd, Key * key);
KeySet * elektraDiffGetModifiedMetaKeys (const ElektraDiff * ksd, Key * key); // Returns old meta keys (pre-modification)

// For Testing
ChangeTrackingContext * elektraChangeTrackingCreateContextForTesting (KeySet * oldKeys);
void elektraChangeTrackingContextDel (ChangeTrackingContext * context);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_KDBCHANGETRACKING_H
