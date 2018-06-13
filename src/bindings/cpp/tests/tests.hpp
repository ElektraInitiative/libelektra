/**
 * @file
 *
 * @brief Some common functions in use for testing framework
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDB_TESTS_HPP
#define KDB_TESTS_HPP

#include <kdb.hpp>
#include <key.hpp>
#include <keyset.hpp>

#include <cstdlib>
#include <cstring>
#include <exception>
#include <iostream>
#include <string>

#include <gtest/gtest.h>

using namespace std;
using namespace kdb;

#define BUFFER_LENGTH 4096

#define succeed_if(x, y)                                                                                                                   \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ASSERT_TRUE (x) << y;                                                                                                      \
	} while (0)
#define exit_if_fail(x, y)                                                                                                                 \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ASSERT_TRUE (x) << y;                                                                                                      \
	} while (0)

#define yield_error(message)                                                                                                               \
	{                                                                                                                                  \
		printf ("%s:%d: error in %s: %s\n", __FILE__, __LINE__, __func__, message);                                                \
	}

#define compare_key_name(pk1, pk2)                                                                                                         \
	{                                                                                                                                  \
		ckdb::Key * nmmk1 = pk1;                                                                                                   \
		ckdb::Key * nmmk2 = pk2;                                                                                                   \
		if (strcmp (keyName (nmmk1), keyName (nmmk2)))                                                                             \
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

#define compare_key_string(pk1, pk2)                                                                                                       \
	{                                                                                                                                  \
		ckdb::Key * smmk1 = pk1;                                                                                                   \
		ckdb::Key * smmk2 = pk2;                                                                                                   \
		if (strcmp (keyString (smmk1), keyString (smmk2)))                                                                         \
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

/**
 * Checks if two keys are equal.
 *
 * Warning: messes up internal cursor
 *
 */
#define compare_key(pk1, pk2)                                                                                                              \
	{                                                                                                                                  \
		ckdb::Key * mmk1 = static_cast<ckdb::Key *> (pk1);                                                                         \
		ckdb::Key * mmk2 = static_cast<ckdb::Key *> (pk2);                                                                         \
		if (mmk1 != mmk2)                                                                                                          \
		{                                                                                                                          \
			compare_key_name (mmk1, mmk2);                                                                                     \
                                                                                                                                           \
			compare_key_string (mmk1, mmk2);                                                                                   \
                                                                                                                                           \
			const ckdb::Key * meta;                                                                                            \
			keyRewindMeta (mmk1);                                                                                              \
			keyRewindMeta (mmk2);                                                                                              \
			while ((meta = keyNextMeta (mmk1)) != 0)                                                                           \
			{                                                                                                                  \
				const ckdb::Key * const metaCmp = keyNextMeta (mmk2);                                                      \
				if (metaCmp == 0)                                                                                          \
				{                                                                                                          \
					printf ("%s:%d: error in %s: Compare key \"%s\" with \"%s\" failed, did not find corresponding "   \
						"metakey %s (k1 > k2)\n",                                                                  \
						__FILE__, __LINE__, __func__, ELEKTRA_QUOTE (mmk1), ELEKTRA_QUOTE (mmk2), keyName (meta)); \
					break;                                                                                             \
				}                                                                                                          \
			}                                                                                                                  \
                                                                                                                                           \
			const ckdb::Key * const metaCmp = keyNextMeta (mmk2);                                                              \
			if (metaCmp != 0)                                                                                                  \
			{                                                                                                                  \
				printf ("%s:%d: error in %s: Compare key \"%s\" with \"%s\" failed, too many metakeys found (k1 < k2)\n",  \
					__FILE__, __LINE__, __func__, ELEKTRA_QUOTE (mmk1), ELEKTRA_QUOTE (mmk2));                         \
			}                                                                                                                  \
		}                                                                                                                          \
	}

#endif
