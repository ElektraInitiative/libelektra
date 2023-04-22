/**
 * @file
 *
 * @brief Provides all macros and definitions which are used for emitting error or warnings
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef KDBERRORS_H
#define KDBERRORS_H

#include <kdb.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <kdbmacros.h>

#ifdef __cplusplus
extern "C" {
using Key = ckdb::Key;
using KeySet = ckdb::KeySet;
#endif

#ifndef ELEKTRA_MODULE_NAME
#define ELEKTRA_MODULE_NAME kdb
#endif

#define ELEKTRA_SET_RESOURCE_ERROR(key, reason)                                                                                            \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Error %s: %s", ELEKTRA_ERROR_RESOURCE, reason);                                                          \
		elektraSetErrorRESOURCE (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), reason);    \
	} while (0)
#define ELEKTRA_SET_RESOURCE_ERRORF(key, reason, ...)                                                                                      \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Error %s: " reason, ELEKTRA_ERROR_RESOURCE, __VA_ARGS__);                                                \
		elektraSetErrorRESOURCE (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), reason,     \
					 __VA_ARGS__);                                                                                     \
	} while (0)
#define ELEKTRA_ADD_RESOURCE_WARNING(key, reason)                                                                                          \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Warning %s: %s", ELEKTRA_WARNING_RESOURCE, reason);                                                      \
		elektraAddWarningRESOURCE (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), reason);  \
	} while (0)
#define ELEKTRA_ADD_RESOURCE_WARNINGF(key, reason, ...)                                                                                    \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Warning %s: " reason, ELEKTRA_WARNING_RESOURCE, __VA_ARGS__);                                            \
		elektraAddWarningRESOURCE (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), reason,   \
					   __VA_ARGS__);                                                                                   \
	} while (0)

#define ELEKTRA_SET_OUT_OF_MEMORY_ERROR(key)                                                                                               \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Error %s", ELEKTRA_ERROR_OUT_OF_MEMORY);                                                                 \
		elektraSetErrorOUT_OF_MEMORY (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME),        \
					      "Memory allocation failed");                                                                 \
	} while (0)

#define ELEKTRA_ADD_OUT_OF_MEMORY_WARNING(key)                                                                                             \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Warning %s: %s", ELEKTRA_WARNING_OUT_OF_MEMORY, "Memory allocation failed");                             \
		elektraAddWarningOUT_OF_MEMORY (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME),      \
						"Memory allocation failed");                                                               \
	} while (0)

#define ELEKTRA_SET_INSTALLATION_ERROR(key, reason)                                                                                        \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Error %s: %s", ELEKTRA_ERROR_INSTALLATION, reason);                                                      \
		elektraSetErrorINSTALLATION (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME),         \
					     reason);                                                                                      \
	} while (0)
#define ELEKTRA_SET_INSTALLATION_ERRORF(key, reason, ...)                                                                                  \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Error %s: " reason, ELEKTRA_ERROR_INSTALLATION, __VA_ARGS__);                                            \
		elektraSetErrorINSTALLATION (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), reason, \
					     __VA_ARGS__);                                                                                 \
	} while (0)
#define ELEKTRA_ADD_INSTALLATION_WARNING(key, reason)                                                                                      \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Warning %s: %s", ELEKTRA_WARNING_INSTALLATION, reason);                                                  \
		elektraAddWarningINSTALLATION (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME),       \
					       reason);                                                                                    \
	} while (0)
#define ELEKTRA_ADD_INSTALLATION_WARNINGF(key, reason, ...)                                                                                \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Warning %s: " reason, ELEKTRA_WARNING_INSTALLATION, __VA_ARGS__);                                        \
		elektraAddWarningINSTALLATION (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME),       \
					       reason, __VA_ARGS__);                                                                       \
	} while (0)

#define ELEKTRA_SET_INTERNAL_ERROR(key, reason)                                                                                            \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Error %s: %s", ELEKTRA_ERROR_INTERNAL, reason);                                                          \
		elektraSetErrorINTERNAL (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), reason);    \
	} while (0)
#define ELEKTRA_SET_INTERNAL_ERRORF(key, reason, ...)                                                                                      \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Error %s: " reason, ELEKTRA_ERROR_INTERNAL, __VA_ARGS__);                                                \
		elektraSetErrorINTERNAL (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), reason,     \
					 __VA_ARGS__);                                                                                     \
	} while (0)
#define ELEKTRA_ADD_INTERNAL_WARNING(key, reason)                                                                                          \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Warning %s: %s", ELEKTRA_WARNING_INTERNAL, reason);                                                      \
		elektraAddWarningINTERNAL (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), reason);  \
	} while (0)
#define ELEKTRA_ADD_INTERNAL_WARNINGF(key, reason, ...)                                                                                    \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Warning %s: " reason, ELEKTRA_WARNING_INTERNAL, __VA_ARGS__);                                            \
		elektraAddWarningINTERNAL (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), reason,   \
					   __VA_ARGS__);                                                                                   \
	} while (0)

#define ELEKTRA_SET_INTERFACE_ERROR(key, reason)                                                                                           \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Error %s: %s", ELEKTRA_ERROR_INTERFACE, reason);                                                         \
		elektraSetErrorINTERFACE (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), reason);   \
	} while (0)
#define ELEKTRA_SET_INTERFACE_ERRORF(key, reason, ...)                                                                                     \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Error %s: " reason, ELEKTRA_ERROR_INTERFACE, __VA_ARGS__);                                               \
		elektraSetErrorINTERFACE (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), reason,    \
					  __VA_ARGS__);                                                                                    \
	} while (0)
#define ELEKTRA_ADD_INTERFACE_WARNING(key, reason)                                                                                         \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Warning %s: %s", ELEKTRA_WARNING_INTERFACE, reason);                                                     \
		elektraAddWarningINTERFACE (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), reason); \
	} while (0)
#define ELEKTRA_ADD_INTERFACE_WARNINGF(key, reason, ...)                                                                                   \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Warning %s: " reason, ELEKTRA_WARNING_INTERFACE, __VA_ARGS__);                                           \
		elektraAddWarningINTERFACE (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), reason,  \
					    __VA_ARGS__);                                                                                  \
	} while (0)

#define ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERROR(key, reason)                                                                                  \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Error %s: %s", ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR, reason);                                                \
		elektraSetErrorPLUGIN_MISBEHAVIOR (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME),   \
						   reason);                                                                                \
	} while (0)
#define ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF(key, reason, ...)                                                                            \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Error %s: " reason, ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR, __VA_ARGS__);                                      \
		elektraSetErrorPLUGIN_MISBEHAVIOR (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME),   \
						   reason, __VA_ARGS__);                                                                   \
	} while (0)
#define ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING(key, reason)                                                                                \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Warning %s: %s", ELEKTRA_WARNING_PLUGIN_MISBEHAVIOR, reason);                                            \
		elektraAddWarningPLUGIN_MISBEHAVIOR (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), \
						     reason);                                                                              \
	} while (0)
#define ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF(key, reason, ...)                                                                          \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Warning %s: " reason, ELEKTRA_WARNING_PLUGIN_MISBEHAVIOR, __VA_ARGS__);                                  \
		elektraAddWarningPLUGIN_MISBEHAVIOR (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), \
						     reason, __VA_ARGS__);                                                                 \
	} while (0)

#define ELEKTRA_SET_CONFLICTING_STATE_ERROR(key, reason)                                                                                   \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Error %s: %s", ELEKTRA_ERROR_CONFLICTING_STATE, reason);                                                 \
		elektraSetErrorCONFLICTING_STATE (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME),    \
						  reason);                                                                                 \
	} while (0)
#define ELEKTRA_SET_CONFLICTING_STATE_ERRORF(key, reason, ...)                                                                             \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Error %s: " reason, ELEKTRA_ERROR_CONFLICTING_STATE, __VA_ARGS__);                                       \
		elektraSetErrorCONFLICTING_STATE (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME),    \
						  reason, __VA_ARGS__);                                                                    \
	} while (0)
#define ELEKTRA_ADD_CONFLICTING_STATE_WARNING(key, reason)                                                                                 \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Warning %s: %s", ELEKTRA_WARNING_CONFLICTING_STATE, reason);                                             \
		elektraAddWarningCONFLICTING_STATE (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME),  \
						    reason);                                                                               \
	} while (0)
#define ELEKTRA_ADD_CONFLICTING_STATE_WARNINGF(key, reason, ...)                                                                           \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Warning %s: " reason, ELEKTRA_WARNING_CONFLICTING_STATE, __VA_ARGS__);                                   \
		elektraAddWarningCONFLICTING_STATE (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME),  \
						    reason, __VA_ARGS__);                                                                  \
	} while (0)

#define ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR(key, reason)                                                                                \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Error %s: %s", ELEKTRA_ERROR_VALIDATION_SYNTACTIC, reason);                                              \
		elektraSetErrorVALIDATION_SYNTACTIC (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), \
						     reason);                                                                              \
	} while (0)
#define ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF(key, reason, ...)                                                                          \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Error %s: " reason, ELEKTRA_ERROR_VALIDATION_SYNTACTIC, __VA_ARGS__);                                    \
		elektraSetErrorVALIDATION_SYNTACTIC (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), \
						     reason, __VA_ARGS__);                                                                 \
	} while (0)
#define ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNING(key, reason)                                                                              \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Warning %s: %s", ELEKTRA_WARNING_VALIDATION_SYNTACTIC, reason);                                          \
		elektraAddWarningVALIDATION_SYNTACTIC (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__),                                        \
						       ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), reason);                                   \
	} while (0)
#define ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF(key, reason, ...)                                                                        \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Warning %s: " reason, ELEKTRA_WARNING_VALIDATION_SYNTACTIC, __VA_ARGS__);                                \
		elektraAddWarningVALIDATION_SYNTACTIC (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__),                                        \
						       ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), reason, __VA_ARGS__);                      \
	} while (0)

#define ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR(key, reason)                                                                                 \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Error %s: %s", ELEKTRA_ERROR_VALIDATION_SEMANTIC, reason);                                               \
		elektraSetErrorVALIDATION_SEMANTIC (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME),  \
						    reason);                                                                               \
	} while (0)
#define ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF(key, reason, ...)                                                                           \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Error %s: " reason, ELEKTRA_ERROR_VALIDATION_SEMANTIC, __VA_ARGS__);                                     \
		elektraSetErrorVALIDATION_SEMANTIC (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME),  \
						    reason, __VA_ARGS__);                                                                  \
	} while (0)
#define ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNING(key, reason)                                                                               \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Warning %s: %s", ELEKTRA_WARNING_VALIDATION_SEMANTIC, reason);                                           \
		elektraAddWarningVALIDATION_SEMANTIC (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__),                                         \
						      ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), reason);                                    \
	} while (0)
#define ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNINGF(key, reason, ...)                                                                         \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_LOG ("Add Warning %s: " reason, ELEKTRA_WARNING_VALIDATION_SEMANTIC, __VA_ARGS__);                                 \
		elektraAddWarningVALIDATION_SEMANTIC (key, __FILE__, ELEKTRA_STRINGIFY (__LINE__),                                         \
						      ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME), reason, __VA_ARGS__);                       \
	} while (0)

#define DECLARE_ERROR_CODE(cname)                                                                                                          \
	extern const char * const ELEKTRA_ERROR_##cname;                                                                                   \
	extern const char * const ELEKTRA_WARNING_##cname;                                                                                 \
                                                                                                                                           \
	void elektraSetError##cname (Key * key, const char * file, const char * line, const char * module, const char * reason, ...);      \
	void elektraAddWarning##cname (Key * key, const char * file, const char * line, const char * module, const char * reason, ...);

DECLARE_ERROR_CODE (RESOURCE)
DECLARE_ERROR_CODE (OUT_OF_MEMORY)
DECLARE_ERROR_CODE (INSTALLATION)
DECLARE_ERROR_CODE (INTERNAL)
DECLARE_ERROR_CODE (INTERFACE)
DECLARE_ERROR_CODE (PLUGIN_MISBEHAVIOR)
DECLARE_ERROR_CODE (CONFLICTING_STATE)
DECLARE_ERROR_CODE (VALIDATION_SYNTACTIC)
DECLARE_ERROR_CODE (VALIDATION_SEMANTIC)

#undef DECLARE_ERROR_CODE

KeySet * elektraErrorSpecification (void);

void elektraTriggerWarnings (const char * nr, Key * parentKey, const char * message);
void elektraTriggerError (const char * nr, Key * parentKey, const char * message);

#ifdef __cplusplus
}
#endif

#endif
