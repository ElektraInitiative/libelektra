

#include "backend.h"

#include <kdbprivate.h>

int elektraBackendOpen (Plugin * handle, Key * errorKey)
{
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraBackendClose (Plugin * handle, Key * errorKey)
{
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraBackendGet (Plugin * handle, KeySet * ks, Key * parentKey)
{
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraBackendSet (Plugin * handle, KeySet * ks, Key * parentKey)
{
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraBackendCommit (Plugin * handle, KeySet * ks, Key * parentKey)
{
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport ("backend",
		ELEKTRA_PLUGIN_OPEN,	&elektraBackendOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraBackendClose,
		ELEKTRA_PLUGIN_GET,	&elektraBackendGet,
		ELEKTRA_PLUGIN_SET,	&elektraBackendSet,
		ELEKTRA_PLUGIN_COMMIT,	&elektraBackendCommit,
		ELEKTRA_PLUGIN_END);
}
