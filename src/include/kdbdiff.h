#ifndef ELEKTRA_KDBDIFF_H
#define ELEKTRA_KDBDIFF_H

#include <kdb.h>
#include <kdbtypes.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

typedef struct _ElektraDiff ElektraDiff;

ElektraDiff * elektraDiffCalculate (KeySet * newKeys, KeySet * oldKeys, Key * parentKey);

void elektraDiffDel (ElektraDiff * ksd);

uint16_t elektraDiffIncRef (ElektraDiff * ksd);
uint16_t elektraDiffDecRef (ElektraDiff * ksd);
uint16_t elektraDiffGetRef (ElektraDiff * ksd);

bool elektraDiffIsEmpty (const ElektraDiff * ksd);

const Key * elektraDiffGetParentKey (const ElektraDiff * ksd);

KeySet * elektraDiffGetAddedKeys (const ElektraDiff * ksd);
KeySet * elektraDiffGetRemovedKeys (const ElektraDiff * ksd);
KeySet * elektraDiffGetModifiedKeys (const ElektraDiff * ksd);

bool elektraDiffKeyValueChanged (const ElektraDiff * ksd, Key * key);
bool elektraDiffKeyOnlyMetaChanged (const ElektraDiff * ksd, Key * key);

KeySet * elektraDiffGetAddedMetaKeys (const ElektraDiff * ksd, Key * key);
KeySet * elektraDiffGetRemovedMetaKeys (const ElektraDiff * ksd, Key * key);
KeySet * elektraDiffGetModifiedMetaKeys (const ElektraDiff * ksd, Key * key); // Returns old meta keys (pre-modification)

#ifdef __cplusplus
}
}
#endif

#endif
