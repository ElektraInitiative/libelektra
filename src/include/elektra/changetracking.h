#ifndef ELEKTRA_KDBCHANGETRACKING_H
#define ELEKTRA_KDBCHANGETRACKING_H

#include <elektra/diff.h>
#include <elektra/kdb/kdb.h>
#include <elektra/plugin/plugin.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

typedef struct _ChangeTrackingContext ChangeTrackingContext;

const ChangeTrackingContext * elektraChangeTrackingGetContextFromKdb (KDB * kdb) ELEKTRA_WEAK;
const ChangeTrackingContext * elektraChangeTrackingGetContextFromPlugin (Plugin * plugin) ELEKTRA_WEAK;

ElektraDiff * elektraChangeTrackingCalculateDiff (KeySet * newKeys, const ChangeTrackingContext * context, Key * parentKey);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_KDBCHANGETRACKING_H
