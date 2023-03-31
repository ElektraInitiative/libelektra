#include <kdbprivate.h>
#include <kdbrecord.h>

void elektraRecordEnableRecording (KDB * handle, const Key * parentKey, Key * errorKey)
{
	Key * configKey = keyNew (ELEKTRA_RECORD_CONFIG_KEY, KEY_END);

	KeySet * config = ksNew (0, KS_END);
	kdbGet (handle, config, configKey);

	elektraNamespace ns = KEY_NS_SYSTEM;
	Key * activeKey = ksLookupByName (config, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KDB_O_POP);
	if (activeKey != NULL)
	{
		ns = keyGetNamespace (activeKey);
		keyDel (activeKey);
	}

	activeKey = keyNew (ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KEY_VALUE, keyName (parentKey), KEY_END);
	keySetNamespace (activeKey, ns);

	ksAppendKey (config, activeKey);

	kdbSet (handle, config, configKey);

	ksAppendKey (handle->global, activeKey);

	keyDel (configKey);
	ksDel (config);
}

void elektraRecordDisableRecording (KDB * handle, Key * errorKey)
{
	Key * configKey = keyNew (ELEKTRA_RECORD_CONFIG_KEY, KEY_END);

	KeySet * config = ksNew (0, KS_END);
	kdbGet (handle, config, configKey);

	Key * activeKey = NULL;
	while ((activeKey = ksLookupByName (config, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KDB_O_POP)) != NULL)
	{
		keyDel (activeKey);
	}

	kdbSet (handle, config, configKey);

	while ((activeKey = ksLookupByName (handle->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, KDB_O_POP)) != NULL)
	{
		keyDel (activeKey);
	}

	keyDel (configKey);
	ksDel (config);
}

void elektraRecordClearSession (KDB * handle, Key * errorKey)
{
	Key * sessionKey = keyNew (ELEKTRA_RECORD_SESSION_KEY, KEY_END);

	KeySet * session = ksNew (0, KS_END);

	kdbGet (handle, session, sessionKey);
	ksDel (ksCut (session, sessionKey));
	kdbSet (handle, session, sessionKey);

	keyDel (sessionKey);
	ksDel (session);
}

bool elektraRecordIsActive (KDB * handle)
{
	if (handle == NULL)
	{
		return false;
	}

	const Key * activeKey = ksLookupByName (handle->global, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, 0);
	if (activeKey == NULL)
	{
		return false;
	}

	return true;
}
