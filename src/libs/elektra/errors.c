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

	char buffer[64] = "warnings/#0\0\0\0";
	const Key * meta = keyGetMeta (key, "warnings");
	const char * old = meta == NULL ? NULL : keyString (meta);
	char * end = &buffer[11];
	if (old && strcmp (old, "#_99") < 0)
	{
		int i = old[1] == '_' ? ((old[2] - '0') * 10 + (old[3] - '0')) : (old[1] - '0');
		i = (i + 1) % 100;

		if (i < 10)
		{
			buffer[10] = '0' + i;
			end = &buffer[11];
		}
		else
		{
			buffer[10] = '_';
			buffer[11] = '0' + (i / 10);
			buffer[12] = '0' + (i % 10);
			end = &buffer[13];
		}
	}
	keySetMeta (key, "warnings", &buffer[9]);

	keySetMeta (key, buffer, "number description  module file line mountpoint configfile reason");
	strcpy (end, "/number");
	keySetMeta (key, buffer, code);
	strcpy (end, "/description");
	keySetMeta (key, buffer, name);
	strcpy (end, "/module");
	keySetMeta (key, buffer, module);
	strcpy (end, "/file");
	keySetMeta (key, buffer, file);
	strcpy (end, "/line");
	keySetMeta (key, buffer, line);
	strcpy (end, "/mountpoint");
	keySetMeta (key, buffer, keyName (key));
	strcpy (end, "/configfile");
	keySetMeta (key, buffer, keyString (key));
	strcpy (end, "/reason");
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
	const char * const ELEKTRA_ERROR_##cname = ELEKTRA_ERROR_CODE_##cname;                                                             \
	const char * const ELEKTRA_WARNING_##cname = ELEKTRA_ERROR_CODE_##cname;                                                           \
                                                                                                                                           \
	void elektraSetError##cname (Key * key, const char * file, const char * line, const char * module, const char * reason, ...)       \
	{                                                                                                                                  \
		va_list va;                                                                                                                \
		va_start (va, reason);                                                                                                     \
		setError (key, ELEKTRA_ERROR_CODE_##cname, ELEKTRA_ERROR_CODE_##cname##_NAME, file, line, module, reason, va);             \
		va_end (va);                                                                                                               \
	}                                                                                                                                  \
                                                                                                                                           \
	void elektraAddWarning##cname (Key * key, const char * file, const char * line, const char * module, const char * reason, ...)     \
	{                                                                                                                                  \
		va_list va;                                                                                                                \
		va_start (va, reason);                                                                                                     \
		addWarning (key, ELEKTRA_ERROR_CODE_##cname, ELEKTRA_ERROR_CODE_##cname##_NAME, file, line, module, reason, va);           \
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

#define ERROR_SPEC_KEYS(cname)                                                                                                             \
	keyNew ("system:/elektra/modules/error/specification", KEY_VALUE, "the specification of all error codes", KEY_END),                \
		keyNew ("system:/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_##cname, KEY_END),                               \
		keyNew ("system:/elektra/modules/error/specification/" ELEKTRA_ERROR_CODE_##cname "/description", KEY_VALUE,               \
			ELEKTRA_ERROR_CODE_##cname##_NAME, KEY_END)

KeySet * elektraErrorSpecification (void)
{
	return ksNew (30,
		      // clang-format off
		      ERROR_SPEC_KEYS (RESOURCE),
		      ERROR_SPEC_KEYS (OUT_OF_MEMORY),
		      ERROR_SPEC_KEYS (INSTALLATION),
		      ERROR_SPEC_KEYS (INTERNAL),
		      ERROR_SPEC_KEYS (INTERFACE),
		      ERROR_SPEC_KEYS (PLUGIN_MISBEHAVIOR),
		      ERROR_SPEC_KEYS (CONFLICTING_STATE),
		      ERROR_SPEC_KEYS (VALIDATION_SYNTACTIC),
		      ERROR_SPEC_KEYS (VALIDATION_SEMANTIC),
		      // clang-format on
		      KS_END);
}

#define MAYBE_TRIGGER_WARNING(cname, code, pk, msg)                                                                                        \
	do                                                                                                                                 \
	{                                                                                                                                  \
		if (strcmp (code, ELEKTRA_ERROR_CODE_##cname) == 0)                                                                        \
		{                                                                                                                          \
			ELEKTRA_ADD_##cname##_WARNING (pk, msg);                                                                           \
			return;                                                                                                            \
		}                                                                                                                          \
	} while (0)

void elektraTriggerWarnings (const char * nr, Key * parentKey, const char * message)
{
	// OOM error is different, so check that one first
	if (strcmp (nr, ELEKTRA_ERROR_CODE_OUT_OF_MEMORY) == 0)
	{
		ELEKTRA_ADD_OUT_OF_MEMORY_WARNING (parentKey);
		return;
	}

	MAYBE_TRIGGER_WARNING (RESOURCE, nr, parentKey, message);
	MAYBE_TRIGGER_WARNING (INSTALLATION, nr, parentKey, message);
	MAYBE_TRIGGER_WARNING (INTERNAL, nr, parentKey, message);
	MAYBE_TRIGGER_WARNING (INTERFACE, nr, parentKey, message);
	MAYBE_TRIGGER_WARNING (PLUGIN_MISBEHAVIOR, nr, parentKey, message);
	MAYBE_TRIGGER_WARNING (CONFLICTING_STATE, nr, parentKey, message);
	MAYBE_TRIGGER_WARNING (VALIDATION_SYNTACTIC, nr, parentKey, message);
	MAYBE_TRIGGER_WARNING (VALIDATION_SEMANTIC, nr, parentKey, message);
	ELEKTRA_ADD_INTERNAL_WARNINGF (parentKey, "Unkown warning code %s", nr);
}

#define MAYBE_TRIGGER_ERROR(cname, code, pk, msg)                                                                                          \
	do                                                                                                                                 \
	{                                                                                                                                  \
		if (strcmp (code, ELEKTRA_ERROR_CODE_##cname) == 0)                                                                        \
		{                                                                                                                          \
			ELEKTRA_SET_##cname##_ERROR (pk, msg);                                                                             \
			return;                                                                                                            \
		}                                                                                                                          \
	} while (0)

void elektraTriggerError (const char * nr, Key * parentKey, const char * message)
{
	// OOM error is different, so check that one first
	if (strcmp (nr, ELEKTRA_ERROR_CODE_OUT_OF_MEMORY) == 0)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		return;
	}

	MAYBE_TRIGGER_ERROR (RESOURCE, nr, parentKey, message);
	MAYBE_TRIGGER_ERROR (INSTALLATION, nr, parentKey, message);
	MAYBE_TRIGGER_ERROR (INTERNAL, nr, parentKey, message);
	MAYBE_TRIGGER_ERROR (INTERFACE, nr, parentKey, message);
	MAYBE_TRIGGER_ERROR (PLUGIN_MISBEHAVIOR, nr, parentKey, message);
	MAYBE_TRIGGER_ERROR (CONFLICTING_STATE, nr, parentKey, message);
	MAYBE_TRIGGER_ERROR (VALIDATION_SYNTACTIC, nr, parentKey, message);
	MAYBE_TRIGGER_ERROR (VALIDATION_SEMANTIC, nr, parentKey, message);
	ELEKTRA_SET_INTERNAL_ERRORF (parentKey, "Unkown error code %s", nr);
}
