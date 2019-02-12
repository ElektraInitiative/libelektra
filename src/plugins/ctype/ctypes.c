/**
 * @file
 *
 * @brief Only a destructor
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "ctypes.h"

#include <string.h>

#include <kdbhelper.h>

#include <elektra/conversion.h>

#define CHECK_TYPE(key, var, toValue)                                                                                                      \
	{                                                                                                                                  \
		if (strlen (keyString (key)) == 0 || toValue (key, &var) != 1)                                                             \
		{                                                                                                                          \
			return false;                                                                                                      \
		}                                                                                                                          \
	}

#define CHECK_TYPE_REVERSIBLE(key, var, toString)                                                                                          \
	{                                                                                                                                  \
		char * string = toString (var);                                                                                            \
		if (strcmp (keyString (key), string) != 0)                                                                                 \
		{                                                                                                                          \
			elektraFree (string);                                                                                              \
			return false;                                                                                                      \
		}                                                                                                                          \
		elektraFree (string);                                                                                                      \
	}

bool elektraCTypeCheckAny (const Key * key ELEKTRA_UNUSED)
{
	return true;
}

bool elektraCTypeCheckEmpty (const Key * key)
{
	return strlen (keyString (key)) == 0;
}

bool elektraCTypeCheckChar (const Key * key)
{
	return strlen (keyString (key)) == 1;
}

bool elektraCTypeCheckString (const Key * key)
{
	return strlen (keyString (key)) != 0;
}

bool elektraCTypeCheckBoolean (const Key * key)
{
	const char * value = keyString (key);
	return (value[0] == '1' || value[0] == '0') && value[1] == '\0';
}

bool elektraCTypeCheckFloat (const Key * key)
{
	kdb_float_t value;
	CHECK_TYPE (key, value, elektraKeyToFloat)
	return true;
}

bool elektraCTypeCheckDouble (const Key * key)
{
	kdb_double_t value;
	CHECK_TYPE (key, value, elektraKeyToDouble)
	return true;
}

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE
bool elektraCTypeCheckLongDouble (const Key * key)
{
	kdb_long_double_t value;
	CHECK_TYPE (key, value, elektraKeyToLongDouble)
	return true;
}

#endif

bool elektraCTypeCheckShort (const Key * key)
{
	kdb_short_t value;
	CHECK_TYPE (key, value, elektraKeyToShort)
	CHECK_TYPE_REVERSIBLE (key, value, elektraShortToString);
	return true;
}

bool elektraCTypeCheckLong (const Key * key)
{
	kdb_long_t value;
	CHECK_TYPE (key, value, elektraKeyToLong)
	CHECK_TYPE_REVERSIBLE (key, value, elektraLongToString);
	return true;
}

bool elektraCTypeCheckLongLong (const Key * key)
{
	kdb_long_long_t value;
	CHECK_TYPE (key, value, elektraKeyToLongLong)
	CHECK_TYPE_REVERSIBLE (key, value, elektraLongLongToString);
	return true;
}

bool elektraCTypeCheckUnsignedShort (const Key * key)
{
	kdb_unsigned_short_t value;
	CHECK_TYPE (key, value, elektraKeyToUnsignedShort)
	CHECK_TYPE_REVERSIBLE (key, value, elektraUnsignedShortToString);
	return true;
}

bool elektraCTypeCheckUnsignedLong (const Key * key)
{
	kdb_unsigned_long_t value;
	CHECK_TYPE (key, value, elektraKeyToUnsignedLong)
	CHECK_TYPE_REVERSIBLE (key, value, elektraUnsignedLongToString);
	return true;
}

bool elektraCTypeCheckUnsignedLongLong (const Key * key)
{
	kdb_unsigned_long_long_t value;
	CHECK_TYPE (key, value, elektraKeyToUnsignedLongLong)
	CHECK_TYPE_REVERSIBLE (key, value, elektraUnsignedLongLongToString);
	return true;
}
