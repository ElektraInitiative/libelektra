#ifndef ELEKTRA_PLUGIN_FUNCTIONS_H
#define ELEKTRA_PLUGIN_FUNCTIONS_H

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/plugin/plugin.h>

#include <stdbool.h>

#ifdef __cplusplus
#define KeySet ckdb::KeySet
#define Key ckdb::Key
#define Plugin ckdb::Plugin
extern "C" {
#endif

/* These define the type for pointers to all the kdb functions */
typedef int (*kdbOpenPtr) (Plugin *, Key * errorKey);
typedef int (*kdbClosePtr) (Plugin *, Key * errorKey);

typedef int (*kdbInitPtr) (Plugin * handle, KeySet * definition, Key * parentKey);
typedef int (*kdbGetPtr) (Plugin * handle, KeySet * returned, Key * parentKey);
typedef int (*kdbSetPtr) (Plugin * handle, KeySet * returned, Key * parentKey);
typedef int (*kdbErrorPtr) (Plugin * handle, KeySet * returned, Key * parentKey);
typedef int (*kdbCommitPtr) (Plugin * handle, KeySet * returned, Key * parentKey);

typedef int (*kdbHookGoptsGetPtr) (Plugin * handle, KeySet * returned, Key * parentKey);

typedef int (*kdbHookSpecCopyPtr) (Plugin * handle, KeySet * returned, Key * parentKey, bool isKdbGet);
typedef int (*kdbHookSpecRemovePtr) (Plugin * handle, KeySet * returned, Key * parentKey);

typedef int (*kdbHookSendNotificationGetPtr) (Plugin * handle, KeySet * returned, Key * parentKey);
typedef int (*kdbHookSendNotificationSetPtr) (Plugin * handle, KeySet * returned, Key * parentKey);

#ifdef __cplusplus
}
#undef Plugin
#undef Key
#undef KeySet
#endif

#endif // ELEKTRA_PLUGIN_FUNCTIONS_H
