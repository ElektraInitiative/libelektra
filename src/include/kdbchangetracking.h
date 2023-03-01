#ifndef ELEKTRA_KDBCHANGETRACKING_H
#define ELEKTRA_KDBCHANGETRACKING_H

#include <kdb.h>
#include <kdbdiff.h>
#include <kdbplugin.h>
#include <kdbtypes.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

typedef struct _ChangeTrackingContext ChangeTrackingContext;

const ChangeTrackingContext * elektraChangeTrackingGetContextFromKdb (KDB * kdb) ELEKTRA_WEAK;
const ChangeTrackingContext * elektraChangeTrackingGetContextFromPlugin (Plugin * plugin) ELEKTRA_WEAK;

ElektraDiff * elektraChangeTrackingCalculateDiff (KeySet * newKeys, const ChangeTrackingContext * context, Key * parentKey);


// For Testing
ChangeTrackingContext * elektraChangeTrackingCreateContextForTesting (KeySet * oldKeys);
void elektraChangeTrackingContextDel (ChangeTrackingContext * context);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_KDBCHANGETRACKING_H
