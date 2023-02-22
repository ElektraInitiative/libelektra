#ifndef ELEKTRA_KDBCHANGETRACKING_H
#define ELEKTRA_KDBCHANGETRACKING_H

#include <kdb.h>
#include <kdbtypes.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

typedef struct _ChangeTrackingContext ChangeTrackingContext;
typedef struct _KeySetDiff KeySetDiff;

const ChangeTrackingContext * elektraChangeTrackingGetContextFromKdb (KDB * kdb);

KeySetDiff * elektraChangeTrackingCalculateFromContext (KeySet * newKeys, const ChangeTrackingContext * context, Key * parentKey);
KeySetDiff * elektraChangeTrackingCalculateFromKeySets (KeySet * newKeys, KeySet * oldKeys, Key * parentKey);

void elektraChangeTrackingKeySetDiffDel (KeySetDiff * ksd);
const Key * elektraChangeTrackingKeySetDiffGetParentKey (const KeySetDiff * ksd);
KeySetDiff * elektraChangeTrackingCreateKeySetDiff (KeySet * addedKeys, KeySet * removedKeys, KeySet * modifiedKey, Key * parentKey);
uint16_t elektraChangeTrackingKeySetDiffIncRef (KeySetDiff * ksd);
uint16_t elektraChangeTrackingKeySetDiffDecRef (KeySetDiff * ksd);

KeySet * elektraChangeTrackingGetAddedKeys (const KeySetDiff * ksd);
KeySet * elektraChangeTrackingGetRemovedKeys (const KeySetDiff * ksd);
KeySet * elektraChangeTrackingGetModifiedKeys (const KeySetDiff * ksd); // Returns old keys (pre-modification)

bool elektraChangeTrackingValueChanged (const KeySetDiff * ksd, Key * key);
bool elektraChangeTrackingMetaChanged (const KeySetDiff * ksd, Key * key);

KeySet * elektraChangeTrackingGetAddedMetaKeys (const KeySetDiff * ksd, Key * key);
KeySet * elektraChangeTrackingGetRemovedMetaKeys (const KeySetDiff * ksd, Key * key);
KeySet * elektraChangeTrackingGetModifiedMetaKeys (const KeySetDiff * ksd, Key * key); // Returns old meta keys (pre-modification)

// For Testing
ChangeTrackingContext * elektraChangeTrackingCreateContextForTesting (KeySet * oldKeys);
void elektraChangeTrackingContextDel (ChangeTrackingContext * context);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_KDBCHANGETRACKING_H
