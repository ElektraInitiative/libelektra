/**
 * @file
 *
 * @brief Used for writing the error/warning information into a key to be used for emitting messages to the user
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

static void addWarning (Key * key, const char * code, const char * name, const char * file, const char * line, const char * module,
			const char * reasonFmt, va_list va)
{
	if (key == NULL)
	{
		return;
	}

	char buffer[25] = "warnings/#00";
	buffer[12] = '\0';
	const Key * meta = keyGetMeta (key, "warnings");
	if (meta)
	{
		buffer[10] = keyString (meta)[0];
		buffer[11] = keyString (meta)[1];
		buffer[11]++;
		if (buffer[11] > '9')
		{
			buffer[11] = '0';
			buffer[10]++;
			if (buffer[10] > '9') buffer[10] = '0';
		}
		keySetMeta (key, "warnings", &buffer[10]);
	}
	else
		keySetMeta (key, "warnings", "00");

	keySetMeta (key, buffer, "number description  module file line mountpoint configfile reason");
	strcat (buffer, "/number");
	keySetMeta (key, buffer, code);
	buffer[12] = '\0';
	strcat (buffer, "/description");
	keySetMeta (key, buffer, name);
	buffer[12] = '\0';
	strcat (buffer, "/module");
	keySetMeta (key, buffer, module);
	buffer[12] = '\0';
	strcat (buffer, "/file");
	keySetMeta (key, buffer, file);
	buffer[12] = '\0';
	strcat (buffer, "/line");
	keySetMeta (key, buffer, line);
	buffer[12] = '\0';
	strcat (buffer, "/mountpoint");
	keySetMeta (key, buffer, keyName (key));
	buffer[12] = '\0';
	strcat (buffer, "/configfile");
	keySetMeta (key, buffer, keyString (key));
	buffer[12] = '\0';
	strcat (buffer, "/reason");
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

static void setValidationError (Key * key, Key * wrongKey, const char * code, const char * name, const char * file, const char * line,
				const char * module, const char * reasonFmt, va_list va)
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
		char * keyText = elektraFormat ("Key %s with value %s does not fulfill: ", keyName (wrongKey), keyValue (wrongKey));
		char * reason = elektraVFormat (reasonFmt, va);
		keySetMeta (key, "error/reason", strncat (keyText, reason, strlen (reason)));
		elektraFree (reason);
		elektraFree (keyText);
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
// DEFINE_ERROR_AND_WARNING (VALIDATION_SEMANTIC)

const char * ELEKTRA_ERROR_VALIDATION_SEMANTIC = ELEKTRA_ERROR_CODE_VALIDATION_SEMANTIC;
const char * ELEKTRA_ERROR_VALIDATION_SEMANTIC_NAME = ELEKTRA_ERROR_CODE_VALIDATION_SEMANTIC_NAME;
const char * ELEKTRA_WARNING_VALIDATION_SEMANTIC = ELEKTRA_ERROR_CODE_VALIDATION_SEMANTIC;
const char * ELEKTRA_WARNING_VALIDATION_SEMANTIC_NAME = ELEKTRA_ERROR_CODE_VALIDATION_SEMANTIC_NAME;

void elektraSetErrorVALIDATION_SEMANTIC (Key * parentKey, Key * wrongKey, const char * file, const char * line, const char * module,
					 const char * reason, ...)
{
	va_list va;
	va_start (va, reason);
	setValidationError (parentKey, wrongKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, ELEKTRA_ERROR_VALIDATION_SEMANTIC_NAME, file, line,
			    module, reason, va);
	va_end (va);
}

void elektraAddWarningVALIDATION_SEMANTIC (Key * parentKey, const char * file, const char * line, const char * module, const char * reason,
					   ...)
{
	va_list va;
	va_start (va, reason);
	addWarning (parentKey, ELEKTRA_WARNING_VALIDATION_SEMANTIC, ELEKTRA_WARNING_VALIDATION_SEMANTIC_NAME, file, line, module, reason,
		    va);
	va_end (va);
}

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
		ELEKTRA_ADD_OUT_OF_MEMORY_WARNING (parentKey);
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
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
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
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (parentKey, parentKey, message);
		return;
	}
	ELEKTRA_SET_INTERNAL_ERRORF (parentKey, "Unkown error code %s", nr);
}
