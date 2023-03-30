#include <kdbprivate.h>
#include <kdbrecord.h>
#include <stdio.h>
#include <stdlib.h>

const char * const recordNamespaces[] = {
	"dir:",
	"user:",
	"system:",
};

const size_t numRecordNamespaces = sizeof (recordNamespaces) / sizeof (char *);

static void elektraRecordMount (void)
{
	// HACK! Replace with mounting API when it is finished
	char buf[2048] = "";
	for (size_t i = 0; i < numRecordNamespaces; i++)
	{
		snprintf (buf, 2047, "kdb mount record-session.cfg %s%s dump", recordNamespaces[i], ELEKTRA_RECORD_SESSION_KEY);
		fprintf (stderr, "%s\n", buf);
		system (buf);
	}
}

static void elektraRecordUnmount (void)
{
	// HACK! Replace with mounting API when it is finished
	char buf[2048] = "";
	for (size_t i = 0; i < numRecordNamespaces; i++)
	{
		snprintf (buf, 2047, "kdb umount %s%s", recordNamespaces[i], ELEKTRA_RECORD_SESSION_KEY);
		fprintf (stderr, "%s\n", buf);
		system (buf);
	}
}

/**
 * Setup all mountpoints etc for recording
 */
void elektraRecordSetup (void)
{
	elektraRecordUnmount ();
	elektraRecordMount ();
}

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
