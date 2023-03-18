/**
 * @file
 *
 * @brief Macros by Elektra.
 *
 * Macros start with ELEKTRA_ and are uppercase.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#ifndef KDBMACROS_H
#define KDBMACROS_H

/** Surround a value with double quotes */
#define ELEKTRA_QUOTE(x) #x
/** Surround a **macro value** with double quotes */
#define ELEKTRA_STRINGIFY(x) ELEKTRA_QUOTE (x)

/** Concat two values */
#define ELEKTRA_CONCAT2(X, Y) X##Y
/** Concat two **macro values** */
#define ELEKTRA_CONCAT(X, Y) ELEKTRA_CONCAT2 (X, Y)

#if defined(__APPLE__)
#define ELEKTRA_STAT_SECONDS(status) status.st_mtime
#define ELEKTRA_STAT_NANO_SECONDS(status) status.st_mtimespec.tv_nsec
#elif defined(_WIN32)
#define ELEKTRA_STAT_SECONDS(status) status.st_mtime
#define ELEKTRA_STAT_NANO_SECONDS(status) 0
#else
#define ELEKTRA_STAT_SECONDS(status) status.st_mtim.tv_sec
#define ELEKTRA_STAT_NANO_SECONDS(status) status.st_mtim.tv_nsec
#endif

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

#ifdef __GNUC__
#define ELEKTRA_ATTRIBUTE_FORMAT(archetype, stringIndex, firstToCheck) __attribute__ ((format (archetype, stringIndex, firstToCheck)))
#else
#define ELEKTRA_ATTRIBUTE_FORMAT(archetype, stringIndex, firstToCheck)
#endif

#ifdef __GNUC__
#define ELEKTRA_ATTRIBUTE_NO_RETURN __attribute__ ((noreturn))
#else
#define ELEKTRA_ATTRIBUTE_NO_RETURN
#endif

#ifdef __GNUC__
/** Declares a parameter as unused. */
#define ELEKTRA_UNUSED __attribute__ ((unused))
#else
#define ELEKTRA_UNUSED
#endif

#ifdef __GNUC__
/** Declares a switch fallthrough case. */
#define ELEKTRA_FALLTHROUGH __attribute__ ((fallthrough))
#else
#define ELEKTRA_FALLTHROUGH
#endif

#ifdef __GNUC__
/** Declares an API as deprecated. */
#define ELEKTRA_DEPRECATED __attribute__ ((deprecated))
#else
#define ELEKTRA_DEPRECATED
#endif

/**
 * Helper macro to create a versioned name of a symbol.
 *
 * @param sym  unversioned name of the symbol
 * @param impl version suffix
 */
#define ELEKTRA_SYMVER(sym, impl) sym##_##impl

/**
 * Declares another version of a symbol using the `.symver` assembler pseudo command
 *
 * @param ver  the version name as declared versions.def
 * @param sym  the unversioned name of the symbol
 * @param impl the version suffix to use for this version
 */
#define ELEKTRA_SYMVER_DECLARE(ver, sym, impl) ELEKTRA_SYMVER_COMMAND (ELEKTRA_STRINGIFY (ELEKTRA_SYMVER (sym, impl)), #sym "@" ver)

#endif
