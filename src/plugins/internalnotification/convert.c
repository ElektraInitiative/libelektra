/**
 * @file
 *
 * @brief Source for internalnotification plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "internalnotification.h"

#include <errno.h>     // errno
#include <kdblogger.h> // ELEKTRA_LOG macros
#include <stdlib.h>    // strto* functions

void elektraInternalnotificationConvertInt (Key * key, void * context)
{
	int * variable = (int *)context;

	// Convert string value to long
	char * end;
	errno = 0;
	long int value = strtol (keyString (key), &end, 10);
	// Update variable if conversion was successful and did not exceed integer range
	if (*end == 0 && errno == 0 && value <= INT_MAX && value >= INT_MIN)
	{
		*(variable) = value;
	}
	else
	{
		ELEKTRA_LOG_WARNING ("conversion failed! keyString=\"%s\" *end=%c, errno=%d, value=%ld", keyString (key), *end, errno,
				     value);
	}
}

void elektraInternalnotificationConvertLong (Key * key, void * context)
{
	long * variable = (long *)context;

	// Convert string value to long
	char * end;
	errno = 0;
	long value = strtol (keyString (key), &end, 10);
	// Update variable if conversion was successful and did not exceed integer range
	if (*end == 0 && errno == 0)
	{
		*(variable) = value;
	}
	else
	{
		ELEKTRA_LOG_WARNING ("conversion failed! keyString=\"%s\" *end=%c, errno=%d, value=%ld", keyString (key), *end, errno,
				     value);
	}
}

void elektraInternalnotificationConvertUnsignedLong (Key * key, void * context)
{
	unsigned long * variable = (unsigned long *)context;

	// Convert string value to unsigned long
	char * end;
	errno = 0;
	unsigned long value = strtoul (keyString (key), &end, 10);
	// Update variable if conversion was successful and did not exceed integer range
	if (*end == 0 && errno == 0)
	{
		*(variable) = value;
	}
	else
	{
		ELEKTRA_LOG_WARNING ("conversion failed! keyString=\"%s\" *end=%c, errno=%d, value=%ld", keyString (key), *end, errno,
				     value);
	}
}

void elektraInternalnotificationConvertFloat (Key * key, void * context)
{
	float * variable = (float *)context;

	// Convert string value to long
	char * end;
	errno = 0;
	float value = strtof (keyString (key), &end);
	// Update variable if conversion was successful
	if (*end == 0 && errno == 0)
	{
		*(variable) = value;
	}
	else
	{
		ELEKTRA_LOG_WARNING ("conversion failed! keyString=\"%s\" *end=%c, errno=%d, value=%f", keyString (key), *end, errno,
				     value);
	}
}

void elektraInternalnotificationConvertDouble (Key * key, void * context)
{
	double * variable = (double *)context;

	// Convert string value to long
	char * end;
	errno = 0;
	double value = strtod (keyString (key), &end);
	// Update variable if conversion was successful
	if (*end == 0 && errno == 0)
	{
		*(variable) = value;
	}
	else
	{
		ELEKTRA_LOG_WARNING ("conversion failed! keyString=\"%s\" *end=%c, errno=%d, value=%f", keyString (key), *end, errno,
				     value);
	}
}
