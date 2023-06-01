/**
 * @file
 *
 * @brief Some common functions in use for testing framework.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef TESTS_H
#define TESTS_H

#include <elektra/macros/utils.h>
#include <internal/config.h>

#include <internal/plugin/struct.h>
#include <internal/utility/alloc.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#define BUFFER_LENGTH 4096
#define ELEKTRA_TEST_ROOT "/tests/ckdb/"

#ifdef __cplusplus
extern "C" {
#endif

extern int nbError;
extern int nbTest;

extern uid_t nbUid;
extern gid_t nbGid;

extern char * tempHome;
extern int tempHomeLen;

extern char * tmpfilename;

int init (int argc, char ** argv);

#ifndef __cplusplus

#define print_result(name)                                                                                                                 \
	{                                                                                                                                  \
		printf ("\n" name " Results: %d Test%s done — %d error%s.\n", nbTest, nbTest == 1 ? "" : "s", nbError,                     \
			nbError == 1 ? "" : "s");                                                                                          \
	}

#define warn_if_fail(expression, message)                                                                                                  \
	{                                                                                                                                  \
		nbTest++;                                                                                                                  \
		if (!(expression))                                                                                                         \
		{                                                                                                                          \
			printf ("%s:%d: warn in %s: %s\n", __FILE__, __LINE__, __func__, message);                                         \
		}                                                                                                                          \
	}

#define yield_error(message)                                                                                                               \
	{                                                                                                                                  \
		nbError++;                                                                                                                 \
		printf ("%s:%d: error in %s: %s\n", __FILE__, __LINE__, __func__, message);                                                \
	}

#define yield_error_fmt(message_fmt, ...)                                                                                                  \
	{                                                                                                                                  \
		nbError++;                                                                                                                 \
		printf ("%s:%d: error in %s: " message_fmt "\n", __FILE__, __LINE__, __func__, __VA_ARGS__);                               \
	}

#define succeed_if(expression, message)                                                                                                    \
	{                                                                                                                                  \
		nbTest++;                                                                                                                  \
		if (!(expression))                                                                                                         \
		{                                                                                                                          \
			yield_error (message);                                                                                             \
		}                                                                                                                          \
	}

#define succeed_if_fmt(expression, message_fmt, ...)                                                                                       \
	{                                                                                                                                  \
		nbTest++;                                                                                                                  \
		if (!(expression))                                                                                                         \
		{                                                                                                                          \
			yield_error_fmt (message_fmt, __VA_ARGS__);                                                                        \
		}                                                                                                                          \
	}

#define exit_if_fail(expression, message)                                                                                                  \
	{                                                                                                                                  \
		nbTest++;                                                                                                                  \
		if (!(expression))                                                                                                         \
		{                                                                                                                          \
			printf ("%s:%d: fatal in %s: %s\n", __FILE__, __LINE__, __func__, message);                                        \
			exit (1);                                                                                                          \
		}                                                                                                                          \
	}

#define compare_key_name(pk1, pk2)                                                                                                         \
	{                                                                                                                                  \
		Key * nmmk1 = pk1;                                                                                                         \
		Key * nmmk2 = pk2;                                                                                                         \
		nbTest++;                                                                                                                  \
		if (strcmp (keyName (nmmk1), keyName (nmmk2)) != 0)                                                                        \
		{                                                                                                                          \
			char errorMsg[BUFFER_LENGTH];                                                                                      \
                                                                                                                                           \
			strcpy (errorMsg, "key name \"");                                                                                  \
			strcat (errorMsg, keyName (nmmk1));                                                                                \
			strcat (errorMsg, "\" is not equal to \"");                                                                        \
			strcat (errorMsg, keyName (nmmk2));                                                                                \
			strcat (errorMsg, "\"");                                                                                           \
                                                                                                                                           \
			yield_error (errorMsg);                                                                                            \
		}                                                                                                                          \
	}

#define compare_key_value(pk1, pk2)                                                                                                        \
	{                                                                                                                                  \
		Key * vmmk1 = pk1;                                                                                                         \
		Key * vmmk2 = pk2;                                                                                                         \
		nbTest++;                                                                                                                  \
		int vmmk1binary = keyIsBinary (vmmk1);                                                                                     \
		int vmmk2binary = keyIsBinary (vmmk2);                                                                                     \
		if (vmmk1binary && vmmk2binary)                                                                                            \
		{                                                                                                                          \
			compare_key_binary (vmmk1, vmmk2);                                                                                 \
		}                                                                                                                          \
		else if (!vmmk1binary && !vmmk2binary)                                                                                     \
		{                                                                                                                          \
			compare_key_string (vmmk1, vmmk2);                                                                                 \
		}                                                                                                                          \
		else                                                                                                                       \
		{                                                                                                                          \
			char errorMsg[BUFFER_LENGTH];                                                                                      \
                                                                                                                                           \
			strcpy (errorMsg, "key \"");                                                                                       \
			strcat (errorMsg, keyName (vmmk1binary ? vmmk1 : vmmk2));                                                          \
			strcat (errorMsg, "\" is binary, but key \"");                                                                     \
			strcat (errorMsg, keyName (vmmk1binary ? vmmk2 : vmmk1));                                                          \
			strcat (errorMsg, "\" is not");                                                                                    \
                                                                                                                                           \
			yield_error (errorMsg);                                                                                            \
		}                                                                                                                          \
	}

#define compare_key_string(pk1, pk2)                                                                                                       \
	{                                                                                                                                  \
		Key * smmk1 = pk1;                                                                                                         \
		Key * smmk2 = pk2;                                                                                                         \
		nbTest++;                                                                                                                  \
		if (strcmp (keyString (smmk1), keyString (smmk2)) != 0)                                                                    \
		{                                                                                                                          \
			char errorMsg[BUFFER_LENGTH];                                                                                      \
                                                                                                                                           \
			strcpy (errorMsg, "key value \"");                                                                                 \
			strcat (errorMsg, keyString (smmk1));                                                                              \
			strcat (errorMsg, "\" is not equal to \"");                                                                        \
			strcat (errorMsg, keyString (smmk2));                                                                              \
			strcat (errorMsg, "\"");                                                                                           \
                                                                                                                                           \
			yield_error (errorMsg);                                                                                            \
		}                                                                                                                          \
	}

#define compare_key_binary(pk1, pk2)                                                                                                       \
	{                                                                                                                                  \
		Key * bmmk1 = pk1;                                                                                                         \
		Key * bmmk2 = pk2;                                                                                                         \
		nbTest++;                                                                                                                  \
		size_t bmmk1size = keyGetValueSize (pk1);                                                                                  \
		size_t bmmk2size = keyGetValueSize (pk2);                                                                                  \
		if (bmmk1size != bmmk2size)                                                                                                \
		{                                                                                                                          \
			char errorMsg[BUFFER_LENGTH];                                                                                      \
                                                                                                                                           \
			strcpy (errorMsg, "key value sizes of \"");                                                                        \
			strcat (errorMsg, keyName (bmmk1));                                                                                \
			strcat (errorMsg, "\" and \"");                                                                                    \
			strcat (errorMsg, keyName (bmmk2));                                                                                \
			strcat (errorMsg, "\" don't match");                                                                               \
                                                                                                                                           \
			yield_error (errorMsg);                                                                                            \
		}                                                                                                                          \
		else if (bmmk1size != 0)                                                                                                   \
		{                                                                                                                          \
			void * bmmk1buf = elektraMalloc (bmmk1size);                                                                       \
			void * bmmk2buf = elektraMalloc (bmmk2size);                                                                       \
                                                                                                                                           \
			keyGetBinary (bmmk1, bmmk1buf, bmmk1size);                                                                         \
			keyGetBinary (bmmk2, bmmk2buf, bmmk2size);                                                                         \
                                                                                                                                           \
			if (memcmp (bmmk1buf, bmmk2buf, bmmk1size) != 0)                                                                   \
			{                                                                                                                  \
				char errorMsg[BUFFER_LENGTH];                                                                              \
                                                                                                                                           \
				strcpy (errorMsg, "binary values of key \"");                                                              \
				strcat (errorMsg, keyName (bmmk1));                                                                        \
				strcat (errorMsg, "\" and \"");                                                                            \
				strcat (errorMsg, keyName (bmmk2));                                                                        \
				strcat (errorMsg, "\" don't match");                                                                       \
                                                                                                                                           \
				yield_error (errorMsg);                                                                                    \
			}                                                                                                                  \
			elektraFree (bmmk1buf);                                                                                            \
			elektraFree (bmmk2buf);                                                                                            \
		}                                                                                                                          \
	}

// GCC diagnostic not allowed inside functions
// does not work with 4.4
// 4.6 and 4.7 as in debian work with it, that are:
// 4.6.3
// 4.7.2
//(https://gcc.gnu.org/bugzilla/show_bug.cgi?id=52116)
#if defined(__GNUC__) && (__GNUC__ > 4 || (__GNUC__ == 4 && (__GNUC_MINOR__ > 6 || (__GNUC_MINOR__ == 6 && __GNUC_PATCHLEVEL__ > 2))) ||   \
			  (__GNUC__ == 4 && (__GNUC_MINOR__ > 7 || (__GNUC_MINOR__ == 7 && __GNUC_PATCHLEVEL__ > 1))))
#define ELEKTRA_PRAGMA(x) _Pragma (ELEKTRA_PRAGMA_STR (x))
#elif defined(__clang_major__) && (__clang_major__ > 3 || (__clang_major__ == 3 && (__clang_minor__ >= 3)))
// might also be supported in earlier versions of clang, but no documentation was found
#define ELEKTRA_PRAGMA(x) _Pragma (ELEKTRA_PRAGMA_STR (x))
#else
#define ELEKTRA_PRAGMA(x)
#endif
#define ELEKTRA_PRAGMA_STR(x) #x
#define ELEKTRA_DIAG_STORE ELEKTRA_PRAGMA (GCC diagnostic push)
#define ELEKTRA_DIAG_OFF_STR(x) ELEKTRA_PRAGMA (GCC diagnostic ignored x)
#define ELEKTRA_DIAG_OFF(x) ELEKTRA_DIAG_OFF_STR (ELEKTRA_PRAGMA_STR (x))
#define ELEKTRA_DIAG_RESTORE ELEKTRA_PRAGMA (GCC diagnostic pop)

#define succeed_if_same_string(ps1, ps2)                                                                                                   \
	{                                                                                                                                  \
		nbTest++;                                                                                                                  \
		const char * s1 = ps1;                                                                                                     \
		const char * s2 = ps2;                                                                                                     \
		ELEKTRA_DIAG_STORE                                                                                                         \
		ELEKTRA_DIAG_OFF (-Waddress)                                                                                               \
		if (!s1)                                                                                                                   \
		{                                                                                                                          \
			yield_error ("left hand side is null pointer")                                                                     \
		}                                                                                                                          \
		else if (!s2)                                                                                                              \
		{                                                                                                                          \
			yield_error ("right hand side is null pointer")                                                                    \
		}                                                                                                                          \
		else                                                                                                                       \
		{                                                                                                                          \
			succeed_if_fmt (strcmp (s1, s2) == 0, "string \"%s\" is not equal to \"%s\"\n\tcompared: %s and %s", s1, s2, #ps1, \
					#ps2)                                                                                              \
		}                                                                                                                          \
		ELEKTRA_DIAG_RESTORE                                                                                                       \
	}

// only works with types convertible to int
#define succeed_if_same_int(s1, s2)                                                                                                        \
	{                                                                                                                                  \
		nbTest++;                                                                                                                  \
		int s1 = ps1;                                                                                                              \
		int s2 = ps2;                                                                                                              \
		if (s1 != s2)                                                                                                              \
		{                                                                                                                          \
			char errorMsg[BUFFER_LENGTH];                                                                                      \
                                                                                                                                           \
			snprintf (errorMsg, BUFFER_LENGTH, "int %d is not equal to %d", s1, s2);                                           \
                                                                                                                                           \
			yield_error (errorMsg);                                                                                            \
		}                                                                                                                          \
	}


/**
 * Checks if two keys are equal.
 *
 * Warning: messes up internal cursor
 *
 */
#define compare_key(pk1, pk2)                                                                                                              \
	do                                                                                                                                 \
	{                                                                                                                                  \
		nbTest++;                                                                                                                  \
		Key * mmk1 = (Key *) pk1;                                                                                                  \
		Key * mmk2 = (Key *) pk2;                                                                                                  \
		if (mmk1 == mmk2)                                                                                                          \
		{                                                                                                                          \
			break; /* same pointer */                                                                                          \
		}                                                                                                                          \
		else if (mmk1 == NULL)                                                                                                     \
		{                                                                                                                          \
			yield_error ("first key is null, but second is not")                                                               \
		}                                                                                                                          \
		else if (mmk2 == NULL)                                                                                                     \
		{                                                                                                                          \
			yield_error ("second key is null, but first is not")                                                               \
		}                                                                                                                          \
		else if (mmk1 != mmk2)                                                                                                     \
		{                                                                                                                          \
			if (mmk1 == NULL)                                                                                                  \
			{                                                                                                                  \
				printf ("%s:%d: error in %s: First key is NULL (but second isn't)\n", __FILE__, __LINE__, __func__);       \
				break;                                                                                                     \
			}                                                                                                                  \
                                                                                                                                           \
			if (mmk2 == NULL)                                                                                                  \
			{                                                                                                                  \
				printf ("%s:%d: error in %s: Second key is NULL (but first isn't)\n", __FILE__, __LINE__, __func__);       \
				break;                                                                                                     \
			}                                                                                                                  \
			compare_key_name (mmk1, mmk2);                                                                                     \
			compare_key_value (mmk1, mmk2);                                                                                    \
                                                                                                                                           \
			KeySet * metaKeys1 = keyMeta (mmk1);                                                                               \
			KeySet * metaKeys2 = keyMeta (mmk2);                                                                               \
			ssize_t itMeta = 0;                                                                                                \
			for (; itMeta < ksGetSize (metaKeys1); ++itMeta)                                                                   \
			{                                                                                                                  \
				const Key * const meta = ksAtCursor (metaKeys1, itMeta);                                                   \
				const Key * const metaCmp = ksAtCursor (metaKeys2, itMeta);                                                \
				if (metaCmp == 0)                                                                                          \
				{                                                                                                          \
					nbError++;                                                                                         \
					printf ("%s:%d: error in %s: Compare key \"%s\" with \"%s\" failed, did not find corresponding "   \
						"metakey %s (k1 > k2)\n",                                                                  \
						__FILE__, __LINE__, __func__, ELEKTRA_QUOTE (mmk1), ELEKTRA_QUOTE (mmk2), keyName (meta)); \
					break;                                                                                             \
				}                                                                                                          \
				if (strcmp (keyName (meta), keyName (metaCmp)) != 0)                                                       \
				{                                                                                                          \
					nbError++;                                                                                         \
					printf ("%s:%d: error in %s: Name of meta key \"%s\" ≠ \"%s\"\n", __FILE__, __LINE__, __func__,    \
						keyName (meta), keyName (metaCmp));                                                        \
					break;                                                                                             \
				}                                                                                                          \
				if (strcmp (keyString (meta), keyString (metaCmp)) != 0)                                                   \
				{                                                                                                          \
					nbError++;                                                                                         \
					printf ("%s:%d: error in %s: Comparison of the keys with name \"%s\" failed. The value of the "    \
						"metakey \"%s\" is not equal: \"%s\" ≠ \"%s\"\n",                                          \
						__FILE__, __LINE__, __func__, keyName (mmk1), keyName (meta), keyString (meta),            \
						keyString (metaCmp));                                                                      \
					break;                                                                                             \
				}                                                                                                          \
			}                                                                                                                  \
                                                                                                                                           \
			const Key * const metaCmp = ksAtCursor (metaKeys2, itMeta);                                                        \
			if (metaCmp != 0)                                                                                                  \
			{                                                                                                                  \
				nbError++;                                                                                                 \
				printf ("%s:%d: error in %s: Compare key \"%s\" with \"%s\" failed, too many metakeys found (k1 < k2)\n",  \
					__FILE__, __LINE__, __func__, ELEKTRA_QUOTE (mmk1), ELEKTRA_QUOTE (mmk2));                         \
			}                                                                                                                  \
		}                                                                                                                          \
	} while (0)


/**Compare two keysets.
 *
 * Compare if two keysets contain the same keys.
 * @retval 0 on success
 * */
#define compare_keyset(pks1, pks2)                                                                                                         \
	{                                                                                                                                  \
		nbTest++;                                                                                                                  \
		KeySet * mmks1 = (KeySet *) pks1;                                                                                          \
		KeySet * mmks2 = (KeySet *) pks2;                                                                                          \
		int bothEmpty = ksGetSize (mmks1) == 0 && ksGetSize (mmks1) == ksGetSize (mmks2);                                          \
		if (mmks1 != mmks2 && !bothEmpty)                                                                                          \
		{                                                                                                                          \
			if (ksGetSize (mmks1) == 0) yield_error ("real size of " ELEKTRA_QUOTE (mmks1) " was 0");                          \
			if (ksGetSize (mmks2) == 0) yield_error ("real size of " ELEKTRA_QUOTE (mmks2) " was 0");                          \
                                                                                                                                           \
			if (ksGetSize (mmks1) != ksGetSize (mmks2))                                                                        \
			{                                                                                                                  \
				nbError++;                                                                                                 \
				printf ("%s:%d: error in %s: Compare keyset failed, size of keysets are not equal with size(%s): %d, "     \
					"size(%s): %d\n",                                                                                  \
					__FILE__, __LINE__, __func__, ELEKTRA_QUOTE (mmks1), (int) ksGetSize (mmks1),                      \
					ELEKTRA_QUOTE (mmks2), (int) ksGetSize (mmks2));                                                   \
				printf ("mmks1:\n");                                                                                       \
				output_keyset (mmks1);                                                                                     \
				printf ("mmks2:\n");                                                                                       \
				output_keyset (mmks2);                                                                                     \
			}                                                                                                                  \
			else                                                                                                               \
			{                                                                                                                  \
				for (elektraCursor it = 0; it < ksGetSize (mmks1); ++it)                                                   \
				{                                                                                                          \
					Key * cmmk1 = ksAtCursor (mmks1, it);                                                              \
					Key * cmmk2 = ksAtCursor (mmks2, it);                                                              \
					if (!cmmk2)                                                                                        \
					{                                                                                                  \
						yield_error ("Compare keyset " ELEKTRA_QUOTE (mmks1) " with " ELEKTRA_QUOTE (              \
							mmks2) " failed, did not find corresponding key") break;                           \
					}                                                                                                  \
                                                                                                                                           \
					compare_key (cmmk1, cmmk2);                                                                        \
				}                                                                                                          \
			}                                                                                                                  \
		}                                                                                                                          \
	}

#endif // __cplusplus

int compare_files (const char * filename);
int compare_line_files (const char * filename, const char * genfilename);
int compare_regex_to_line_files (const char * filename, const char * genfilename);

char * srcdir_file (const char * fileName);
char * bindir_file (const char * fileName);
const char * elektraFilename (void);
void elektraUnlink (const char * filename);

Key * create_root_key (const char * backendName);
KeySet * create_conf (const char * filename);

void output_meta (Key * k);
void output_key (Key * ks);
void output_keyset (KeySet * ks);

int output_warnings (Key * errorKey);
int output_error (Key * errorKey);

void clean_temp_home (void);

#ifdef __cplusplus
} // end extern "C"
#endif

#endif
