/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or https://www.libelektra.org)
 */

#include <kdberrors.h>

#include <string.h>

#ifdef __cplusplus
using namespace ckdb;
#endif

#define ELEKTRA_ERROR_CODE_RESOURCE "C01100"
#define ELEKTRA_ERROR_CODE_RESOURCE_NAME "Resource"
#define ELEKTRA_ERROR_CODE_OUT_OF_MEMORY "C01110"
#define ELEKTRA_ERROR_CODE_OUT_OF_MEMORY_NAME "Out of Memory"
#define ELEKTRA_ERROR_CODE_INSTALLATION "C01200"
#define ELEKTRA_ERROR_CODE_INSTALLATION_NAME "Installation"
#define ELEKTRA_ERROR_CODE_INTERNAL "C01310"
#define ELEKTRA_ERROR_CODE_INTERNAL_NAME "Internal"
#define ELEKTRA_ERROR_CODE_INTERFACE "C01320"
#define ELEKTRA_ERROR_CODE_INTERFACE_NAME "Interface"
#define ELEKTRA_ERROR_CODE_PLUGIN_MISBEHAVIOR "C01330"
#define ELEKTRA_ERROR_CODE_PLUGIN_MISBEHAVIOR_NAME "Plugin Misbehavior"
#define ELEKTRA_ERROR_CODE_CONFLICTING_STATE "C02000"
#define ELEKTRA_ERROR_CODE_CONFLICTING_STATE_NAME "Conflict"
#define ELEKTRA_ERROR_CODE_VALIDATION_SYNTACTIC "C03100"
#define ELEKTRA_ERROR_CODE_VALIDATION_SYNTACTIC_NAME "Validation Syntactic"
#define ELEKTRA_ERROR_CODE_VALIDATION_SEMANTIC "C03200"
#define ELEKTRA_ERROR_CODE_VALIDATION_SEMANTIC_NAME "Validation Semantic"

static void nextWarning (Key * key, char * buffer)
{
	const Key * meta = keyGetMeta (key, "warnings");
	const char * last = meta == NULL ? "" : keyString (meta);
	if (last[0] == '\0' || last[1] == '\0')
	{
		strncpy (buffer, "warnings/#0", 12);
		keySetMeta (key, "warnings", "#0");
	}
	else
	{
		strncpy (buffer, "warnings/#", 11);
		size_t len = strlen (last);
		size_t cur = len - 1;
		char * buf = buffer + strlen ("warnings/");
		strncpy (buf, last, len + 1);

		buf[cur] = last[cur];
		++buf[cur];
		while (buf[cur] > '9')
		{
			buf[cur] = '0';
			if (last[cur - 1] == '_' || last[cur - 1] == '#')
			{
				strncpy (&buf[cur + 2], &last[cur], strlen (&last[cur]));
				buf[cur] = '_';
				buf[cur + 1] = '1';
				buf[cur + 2] = '0';
				buf[len + 2] = '\0';
				break;
			}
			--cur;
			buf[cur] = last[cur];
			++buf[cur];
		}
		keySetMeta (key, "warnings", buf);
	}
}

static void addWarning (Key * key, const char * code, const char * name, const char * file, const char * line, const char * module,
			const char * reasonFmt, va_list va)
{
	if (key == NULL)
	{
		return;
	}

	char buffer[ELEKTRA_MAX_ARRAY_SIZE + sizeof ("warnings/")];
	nextWarning (key, buffer);

	size_t len = strlen (buffer);
	if (len > sizeof ("warnings/#_99"))
	{ /* wrap arround after 100 warnings */
		keySetMeta (key, "warnings", "#0");
		strncpy (buffer, "warnings/#0", 12);
		len = strlen (buffer);
	}
	char * bufferEnd = buffer + len;

	keySetMeta (key, buffer, "number description  module file line mountpoint configfile reason");
	strncpy (bufferEnd, "/number", 8);
	keySetMeta (key, buffer, code);
	strncpy (bufferEnd, "/description", 13);
	keySetMeta (key, buffer, name);
	strncpy (bufferEnd, "/module", 8);
	keySetMeta (key, buffer, module);
	strncpy (bufferEnd, "/file", 6);
	keySetMeta (key, buffer, file);
	strncpy (bufferEnd, "/line", 6);
	keySetMeta (key, buffer, line);
	strncpy (bufferEnd, "/mountpoint", 12);
	keySetMeta (key, buffer, keyName (key));
	strncpy (bufferEnd, "/configfile", 12);
	keySetMeta (key, buffer, keyString (key));
	strncpy (bufferEnd, "/reason", 8);
	char * reason = elektraVFormat (reasonFmt, va);
	keySetMeta (key, buffer, reason);
	elektraFree (reason);
}

static void setError (Key * key, const char * code, const char * name, const char * file, const char * line, const char * module,
		      const char * reasonFmt, va_list va)
{
	if (key == NULL)
	{
		return;
	}

	if (keyGetMeta (key, "error"))
	{
		addWarning (key, code, name, file, line, module, reasonFmt, va);
	}
	else
	{
		keySetMeta (key, "error", "number description  module file line mountpoint configfile reason");
		keySetMeta (key, "error/number", code);
		keySetMeta (key, "error/description", name);
		keySetMeta (key, "error/module", module);
		keySetMeta (key, "error/file", file);
		keySetMeta (key, "error/line", line);
		keySetMeta (key, "error/mountpoint", keyName (key));
		keySetMeta (key, "error/configfile", keyString (key));
		char * reason = elektraVFormat (reasonFmt, va);
		keySetMeta (key, "error/reason", reason);
		elektraFree (reason);
	}
}

#define DEFINE_ERROR_AND_WARNING(cname)                                                                                                    \
	const char * ELEKTRA_ERROR_##cname = ELEKTRA_ERROR_CODE_##cname;                                                                   \
	const char * ELEKTRA_ERROR_##cname##_NAME = ELEKTRA_ERROR_CODE_##cname##_NAME;                                                     \
	const char * ELEKTRA_WARNING_##cname = ELEKTRA_ERROR_CODE_##cname;                                                                 \
	const char * ELEKTRA_WARNING_##cname##_NAME = ELEKTRA_ERROR_CODE_##cname##_NAME;                                                   \
                                                                                                                                           \
	void elektraSetError##cname (Key * key, const char * file, const char * line, const char * module, const char * reason, ...)       \
	{                                                                                                                                  \
		va_list va;                                                                                                                \
		va_start (va, reason);                                                                                                     \
		setError (key, ELEKTRA_ERROR_##cname, ELEKTRA_ERROR_##cname##_NAME, file, line, module, reason, va);                       \
		va_end (va);                                                                                                               \
	}                                                                                                                                  \
                                                                                                                                           \
	void elektraAddWarning##cname (Key * key, const char * file, const char * line, const char * module, const char * reason, ...)     \
	{                                                                                                                                  \
		va_list va;                                                                                                                \
		va_start (va, reason);                                                                                                     \
		addWarning (key, ELEKTRA_WARNING_##cname, ELEKTRA_WARNING_##cname##_NAME, file, line, module, reason, va);                 \
		va_end (va);                                                                                                               \
	}

DEFINE_ERROR_AND_WARNING (RESOURCE)
DEFINE_ERROR_AND_WARNING (OUT_OF_MEMORY)
DEFINE_ERROR_AND_WARNING (INSTALLATION)
DEFINE_ERROR_AND_WARNING (INTERNAL)
DEFINE_ERROR_AND_WARNING (INTERFACE)
DEFINE_ERROR_AND_WARNING (PLUGIN_MISBEHAVIOR)
DEFINE_ERROR_AND_WARNING (CONFLICTING_STATE)
DEFINE_ERROR_AND_WARNING (VALIDATION_SYNTACTIC)
DEFINE_ERROR_AND_WARNING (VALIDATION_SEMANTIC)

KeySet * elektraErrorSpecification (void)
{
	return ksNew (30, keyNew ("system/elektra/modules/error/specification", KEY_VALUE, "the specification of all error codes", KEY_END),
		      keyNew ("system/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_RESOURCE, KEY_END),
		      keyNew ("system/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_RESOURCE "/description", KEY_VALUE,
			      ELEKTRA_ERROR_CODE_RESOURCE_NAME, KEY_END),
		      keyNew ("system/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_OUT_OF_MEMORY, KEY_END),
		      keyNew ("system/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_OUT_OF_MEMORY "/description", KEY_VALUE,
			      ELEKTRA_ERROR_CODE_OUT_OF_MEMORY_NAME, KEY_END),
		      keyNew ("system/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_INSTALLATION, KEY_END),
		      keyNew ("system/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_INSTALLATION "/description", KEY_VALUE,
			      ELEKTRA_ERROR_CODE_INSTALLATION_NAME, KEY_END),
		      keyNew ("system/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_INTERNAL, KEY_END),
		      keyNew ("system/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_INTERNAL "/description", KEY_VALUE,
			      ELEKTRA_ERROR_CODE_INTERNAL_NAME, KEY_END),
		      keyNew ("system/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_INTERFACE, KEY_END),
		      keyNew ("system/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_INTERFACE "/description", KEY_VALUE,
			      ELEKTRA_ERROR_CODE_INTERFACE_NAME, KEY_END),
		      keyNew ("system/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_PLUGIN_MISBEHAVIOR, KEY_END),
		      keyNew ("system/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_PLUGIN_MISBEHAVIOR "/description", KEY_VALUE,
			      ELEKTRA_ERROR_CODE_PLUGIN_MISBEHAVIOR_NAME, KEY_END),
		      keyNew ("system/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_CONFLICTING_STATE, KEY_END),
		      keyNew ("system/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_CONFLICTING_STATE "/description", KEY_VALUE,
			      ELEKTRA_ERROR_CODE_CONFLICTING_STATE_NAME, KEY_END),
		      keyNew ("system/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_VALIDATION_SYNTACTIC, KEY_END),
		      keyNew ("system/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_VALIDATION_SYNTACTIC "/description",
			      KEY_VALUE, ELEKTRA_ERROR_CODE_VALIDATION_SYNTACTIC_NAME, KEY_END),
		      keyNew ("system/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_VALIDATION_SEMANTIC, KEY_END),
		      keyNew ("system/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_VALIDATION_SEMANTIC "/description",
			      KEY_VALUE, ELEKTRA_ERROR_CODE_VALIDATION_SEMANTIC_NAME, KEY_END),
		      KS_END);
}

void elektraTriggerWarnings (const char * nr, Key * parentKey, const char * message)
{
	if (strcmp (nr, ELEKTRA_WARNING_RESOURCE) == 0)
	{
		ELEKTRA_ADD_RESOURCE_WARNING (parentKey, message);
		return;
	}
	if (strcmp (nr, ELEKTRA_WARNING_OUT_OF_MEMORY) == 0)
	{
		ELEKTRA_ADD_OUT_OF_MEMORY_WARNING (parentKey, message);
		return;
	}
	if (strcmp (nr, ELEKTRA_WARNING_INSTALLATION) == 0)
	{
		ELEKTRA_ADD_INSTALLATION_WARNING (parentKey, message);
		return;
	}
	if (strcmp (nr, ELEKTRA_WARNING_INTERNAL) == 0)
	{
		ELEKTRA_ADD_INTERNAL_WARNING (parentKey, message);
		return;
	}
	if (strcmp (nr, ELEKTRA_WARNING_INTERFACE) == 0)
	{
		ELEKTRA_ADD_INTERFACE_WARNING (parentKey, message);
		return;
	}
	if (strcmp (nr, ELEKTRA_WARNING_PLUGIN_MISBEHAVIOR) == 0)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (parentKey, message);
		return;
	}
	if (strcmp (nr, ELEKTRA_WARNING_CONFLICTING_STATE) == 0)
	{
		ELEKTRA_ADD_CONFLICTING_STATE_WARNING (parentKey, message);
		return;
	}
	if (strcmp (nr, ELEKTRA_WARNING_VALIDATION_SYNTACTIC) == 0)
	{
		ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNING (parentKey, message);
		return;
	}
	if (strcmp (nr, ELEKTRA_WARNING_VALIDATION_SEMANTIC) == 0)
	{
		ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNING (parentKey, message);
		return;
	}
	ELEKTRA_ADD_INTERNAL_WARNINGF (parentKey, "Unkown warning code %s", nr);
}

void elektraTriggerError (const char * nr, Key * parentKey, const char * message)
{
	if (strcmp (nr, ELEKTRA_ERROR_RESOURCE) == 0)
	{
		ELEKTRA_SET_RESOURCE_ERROR (parentKey, message);
		return;
	}
	if (strcmp (nr, ELEKTRA_ERROR_OUT_OF_MEMORY) == 0)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey, message);
		return;
	}
	if (strcmp (nr, ELEKTRA_ERROR_INSTALLATION) == 0)
	{
		ELEKTRA_SET_INSTALLATION_ERROR (parentKey, message);
		return;
	}
	if (strcmp (nr, ELEKTRA_ERROR_INTERNAL) == 0)
	{
		ELEKTRA_SET_INTERNAL_ERROR (parentKey, message);
		return;
	}
	if (strcmp (nr, ELEKTRA_ERROR_INTERFACE) == 0)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, message);
		return;
	}
	if (strcmp (nr, ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR) == 0)
	{
		ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERROR (parentKey, message);
		return;
	}
	if (strcmp (nr, ELEKTRA_ERROR_CONFLICTING_STATE) == 0)
	{
		ELEKTRA_SET_CONFLICTING_STATE_ERROR (parentKey, message);
		return;
	}
	if (strcmp (nr, ELEKTRA_ERROR_VALIDATION_SYNTACTIC) == 0)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (parentKey, message);
		return;
	}
	if (strcmp (nr, ELEKTRA_ERROR_VALIDATION_SEMANTIC) == 0)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (parentKey, message);
		return;
	}
	ELEKTRA_SET_INTERNAL_ERRORF (parentKey, "Unkown error code %s", nr);
}
