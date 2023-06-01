#ifndef ELEKTRA_KDBCHANGETRACKING_INTERNAL_H
#define ELEKTRA_KDBCHANGETRACKING_INTERNAL_H

#include <elektra/diff.h>
#include <elektra/kdb/kdb.h>
#include <elektra/plugin/plugin.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

struct _ChangeTrackingContext
{
	KeySet * oldKeys;
};

struct _ChangeTrackingContext * elektraChangeTrackingCreateContextForTesting (KeySet * oldKeys);
void elektraChangeTrackingContextDel (struct _ChangeTrackingContext * context);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_KDBCHANGETRACKING_INTERNAL_H
