#include "./elektra-xfconf.h"
#include "./elektra-xfconf-util.h"

#include <elektra/core.h>
#include <elektra/kdb/kdb.h>

#include <internal/macros/attributes.h>

#include <pthread.h>


pthread_rwlock_t channel_lock = PTHREAD_RWLOCK_INITIALIZER;
pthread_rwlock_t init_lock = PTHREAD_RWLOCK_INITIALIZER;
GList * channel_list = NULL;
KDB * elektraKdb = NULL;
KeySet * globalKeySet = NULL;

gboolean xfconf_init (GError ** error)
{
	trace ();
	if (pthread_rwlock_trywrlock (&init_lock) == 0 && pthread_rwlock_trywrlock (&channel_lock) == 0)
	{
		g_info ("channel lock successful, initialize structures if necessary");
		Key * elektraError = keyNew ("/elektra_error", KEY_END);
		if (globalKeySet == NULL)
		{
			globalKeySet = ksNew (0, KS_END);
		}
		if (elektraKdb == NULL)
		{
			g_info ("structures are not initialized yet, proceeding");
			elektraKdb = kdbOpen (NULL, elektraError);
			if (elektraKdb == NULL)
			{
				g_debug ("unable to open elektraKdb");
				*error = g_error_new (0, 1, "unable to open kdb: %s", keyString (elektraError));
				return FALSE;
			}
		}
		else
		{
			g_info ("structures were already initialized");
		}
		pthread_rwlock_unlock (&channel_lock);
	}
	else
	{
		g_info ("channel lock failed, structures seem to be already initialized");
	}
	return TRUE;
}
void xfconf_shutdown (void)
{
	trace ();
	// g_list_free (channel_list);
}

void xfconf_named_struct_register (const gchar * struct_name ELEKTRA_UNUSED, guint n_members ELEKTRA_UNUSED,
				   const GType * member_types ELEKTRA_UNUSED)
{
	unimplemented ();
}

void xfconf_array_free (GPtrArray * arr ELEKTRA_UNUSED)
{
	unimplemented ();
}

gchar ** xfconf_list_channels (void)
{
	trace ();
	Key * parentKey = keyNew (XFCONF_ROOT, KEY_END);
	KeySet * channelKeySet = ksNew (0, KS_END);

	require_channel_read_lock ();
	kdbGet (elektraKdb, channelKeySet, parentKey);
	ssize_t keySetLength = ksGetSize (channelKeySet);
	gchar ** channelNames = calloc (keySetLength + 1, sizeof (gchar *));
	const Key * currentKey;
	const char * currentKeyName;
	const char * currentChannelNameWithSuffix;
	const char * currentChannelNameEnd;
	for (elektraCursor i = 0, nameIndex = 0; i < keySetLength; i++)
	{
		//		This looks at the first level of the keyname (minus the namespace and root where all xfconf keys live) as
		// this is the actual channel name in xfconf.
		currentKey = ksAtCursor (channelKeySet, i);
		currentKeyName = keyName (currentKey);
		currentChannelNameWithSuffix = &currentKeyName[strlen (XFCONF_NAMESPACE) + strlen (XFCONF_ROOT) + 1];
		currentChannelNameEnd = strchr (currentChannelNameWithSuffix, '/');
		if (!currentChannelNameEnd)
		{
			currentChannelNameEnd = &currentChannelNameWithSuffix[strlen (currentChannelNameWithSuffix)];
		}
		gchar * firstLevelName = strndup (currentChannelNameWithSuffix, currentChannelNameEnd - currentChannelNameWithSuffix);
		g_debug ("found channel name %s", firstLevelName);
		if (i == 0 || strcmp (firstLevelName, channelNames[nameIndex - 1]) != 0)
		{
			g_debug ("appending %s", firstLevelName);
			channelNames[nameIndex] = firstLevelName;
			nameIndex++;
		}
		else
		{
			free (firstLevelName);
		}
	}
	release_channel_lock ();
	return channelNames;
}
