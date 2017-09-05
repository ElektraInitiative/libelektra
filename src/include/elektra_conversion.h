/**
 * @file
 *
 * @brief Elektra conversion.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_CONVERSION_H
#define ELEKTRA_CONVERSION_H

#include <stdlib.h>
#include "kdbhelper.h"

#define KDB_STRING_TO_STRING(value) value

#define KDB_STRING_TO_BOOLEAN(string) (kdb_boolean_t) !strcmp (string, "1")
#define KDB_STRING_TO_CHAR(string) (kdb_char_t) string[0]
#define KDB_STRING_TO_OCTET(string) (kdb_octet_t) strtoul (string, NULL, 10)
#define KDB_STRING_TO_SHORT(string) (kdb_short_t) strtoul (string, NULL, 10)
#define KDB_STRING_TO_UNSIGNED_SHORT(string) (kdb_unsigned_short_t) strtoul (string, NULL, 10)
#define KDB_STRING_TO_LONG(string) (kdb_long_t) strtoul (string, NULL, 10)
#define KDB_STRING_TO_UNSIGNED_LONG(string) (kdb_unsigned_long_t) strtoul (string, NULL, 10)
#define KDB_STRING_TO_LONG_LONG(string) ELEKTRA_LONG_LONG_S (string, NULL, 10)
#define KDB_STRING_TO_UNSIGNED_LONG_LONG(string) ELEKTRA_UNSIGNED_LONG_LONG_S (string, NULL, 10)
#define KDB_STRING_TO_FLOAT(string) strtof (string, NULL)
#define KDB_STRING_TO_DOUBLE(string) strtod (string, NULL)
#define KDB_STRING_TO_LONG_DOUBLE(string) strtold (string, NULL)
#define KDB_STRING_TO_ENUM(string) atoi (string)

#define KDB_BOOLEAN_TO_STRING(value) ((value ? "1" : "0"))
#define KDB_CHAR_TO_STRING(value) elektraFormat ("%c", value)
#define KDB_OCTET_TO_STRING(value) elektraFormat ("%d", value)
#define KDB_SHORT_TO_STRING(value) elektraFormat ("%d", value)
#define KDB_UNSIGNED_SHORT_TO_STRING(value) elektraFormat ("%d", value)
#define KDB_LONG_TO_STRING(value) elektraFormat (ELEKTRA_LONG_F, value)
#define KDB_UNSIGNED_LONG_TO_STRING(value) elektraFormat (ELEKTRA_UNSIGNED_LONG_F, value)
#define KDB_LONG_LONG_TO_STRING(value) elektraFormat (ELEKTRA_LONG_LONG_F, value)
#define KDB_UNSIGNED_LONG_LONG_TO_STRING(value) elektraFormat (ELEKTRA_UNSIGNED_LONG_LONG_F, value)
#define KDB_FLOAT_TO_STRING(value) elektraFormat ("%f", value)
#define KDB_DOUBLE_TO_STRING(value) elektraFormat ("%f", value)
#define KDB_LONG_DOUBLE_TO_STRING(value) elektraFormat ("%Lf", value)
#define KDB_ENUM_TO_STRING(string) elektraFormat ("%d", value)

#endif // ELEKTRA_CONVERSION_H
