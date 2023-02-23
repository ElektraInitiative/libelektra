#include "elektra-xfconf.h"
#include "elektra-xfconf-util.h"

GList * channel_list = NULL;
KDB * elektraKdb = NULL;

gboolean xfconf_init (GError ** error)
{
	trace ();
	Key * elektraError = keyNew ("/elektra_error", KEY_END);
	elektraKdb = kdbOpen (NULL, elektraError);
	if (elektraKdb == NULL)
	{
		g_debug ("unable to open elektraKdb");
		*error = g_error_new (0, 1, "unable to open kdb: %s", keyString (elektraError));
		return FALSE;
	}
	return TRUE;
}
void xfconf_shutdown (void)
{
	trace ();
	g_list_free (channel_list);
}

void xfconf_named_struct_register (const gchar * struct_name, guint n_members, const GType * member_types)
{
	unimplemented ();
}

void xfconf_array_free (GPtrArray * arr)
{
	unimplemented ();
}

gchar ** xfconf_list_channels (void)
{
	trace ();
	Key * parentKey = keyNew (XFCONF_ROOT, KEY_END);
	KeySet * channelKeySet = ksNew (0, KS_END);
	kdbGet (elektraKdb, channelKeySet, parentKey);
	ssize_t keySetLength = ksGetSize (channelKeySet);
	gchar ** channelNames = calloc (keySetLength + 1, sizeof (gchar *));
	const Key * currentKey;
	const char * currentKeyName;
	const char * currentChannelNameWithSuffix;
	const char * currentChannelNameEnd;
	for (elektraCursor i = 0, nameIndex = 0; i < keySetLength; i++)
	{
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

	return channelNames;
}
