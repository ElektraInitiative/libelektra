
/**
 * @file
 *
 * @brief Helper macros to report common kinds of errors in plugins.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_MACROS_PLUGIN_ERRORS_H
#define ELEKTRA_MACROS_PLUGIN_ERRORS_H

#define ELEKTRA_SET_ERROR_GET(parentKey)                                                                                                   \
	do                                                                                                                                 \
	{                                                                                                                                  \
		if (errno == EACCES)                                                                                                       \
			ELEKTRA_SET_RESOURCE_ERRORF (                                                                                      \
				parentKey,                                                                                                 \
				"Insufficient permissions to open configuration file %s for reading. Reason: %s. You might want "          \
				"to retry as root or change access using chmod.",                                                          \
				keyString (parentKey), strerror (errno));                                                                  \
		else                                                                                                                       \
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not open configuration file %s for reading. Reason: %s",            \
						     keyString (parentKey), strerror (errno));                                             \
	} while (0)

#define ELEKTRA_SET_ERROR_SET(parentKey)                                                                                                   \
	do                                                                                                                                 \
	{                                                                                                                                  \
		if (errno == EACCES)                                                                                                       \
			ELEKTRA_SET_RESOURCE_ERRORF (                                                                                      \
				parentKey,                                                                                                 \
				"Insufficient permissions to open configuration file %s for writing. You might want to retry as "          \
				"root. Reason: %s",                                                                                        \
				keyString (parentKey), strerror (errno));                                                                  \
		else                                                                                                                       \
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not open file %s for writing. Reason: %s", keyString (parentKey),   \
						     strerror (errno));                                                                    \
	} while (0)

/**
 * @brief Sets error if info != returned
 *
 * @param info how the info is now (freshly received)
 * @param returned how the info passed from user is
 * @param error key to set error to
 *
 * @return with -1 on error
 */
#define ELEKTRA_SET_ERROR_READ_ONLY(info, returned, error)                                                                                 \
	do                                                                                                                                 \
	{                                                                                                                                  \
		Key * k;                                                                                                                   \
		elektraCursor it = 0;                                                                                                      \
		for (; it < ksGetSize (returned); ++it)                                                                                    \
		{                                                                                                                          \
			k = ksAtCursor (returned, it);                                                                                     \
			Key * c = ksAtCursor (info, it);                                                                                   \
			if (!c)                                                                                                            \
			{                                                                                                                  \
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (error,                                                             \
									"Read only plugin, adding the key %s (value %s) is not allowed",   \
									keyName (k), keyString (k));                                       \
				ksDel (info);                                                                                              \
				return -1;                                                                                                 \
			}                                                                                                                  \
			if (strcmp (keyName (k), keyName (c)) || strcmp (keyString (k), keyString (c)))                                    \
			{                                                                                                                  \
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (error,                                                             \
									"Read only plugin, changing the key %s (expected %s) "             \
									"with value "                                                      \
									"'%s' (expected '%s') is not allowed",                             \
									keyName (k), keyName (c), keyString (k), keyString (c));           \
				ksDel (info);                                                                                              \
				return -1;                                                                                                 \
			}                                                                                                                  \
		}                                                                                                                          \
		if ((k = ksAtCursor (info, it)) != 0)                                                                                      \
		{                                                                                                                          \
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (error, "Read only plugin, removing the key %s (value %s) is not allowed",  \
								keyName (k), keyString (k));                                               \
			ksDel (info);                                                                                                      \
			return -1;                                                                                                         \
		}                                                                                                                          \
		ksDel (info);                                                                                                              \
	} while (0)

#endif // ELEKTRA_MACROS_PLUGIN_ERRORS_H
