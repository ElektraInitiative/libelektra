#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <stdio.h>
#include <kdbhelper.h>

#include "floathelper.h"

char * elektraFtoA(char *buffer, ssize_t bufSize, double val)
{
	if(buffer == NULL)
	{
		bufSize = MAX_CHARS_DOUBLE;
		buffer = elektraMalloc(bufSize);
	}
	snprintf(buffer, bufSize, "%g", val);

	struct lconv *locale;
	locale = localeconv();
	char sysSep = (locale->decimal_point)[0];

	if(sysSep == ELEKTRA_DEFAULT_DECIMAL_POINT)
	{
		return buffer;
	}
	else
	{
		char *sepPtr = strchr(buffer, sysSep);
		if(sepPtr == NULL)
		{
			return buffer;
		}
		else
		{
			*sepPtr = ELEKTRA_DEFAULT_DECIMAL_POINT;
			return buffer;
		}
	}
}

double elektraEFtoF(const char *string)
{
	char *buffer = malloc(elektraStrLen(string));
	strcpy(buffer, string);
	char *sepPtr = strchr(buffer, ELEKTRA_DEFAULT_DECIMAL_POINT);
	if(sepPtr == NULL)
	{
		free(buffer);
		return atof(string);
	}
	else
	{
		struct lconv *locale;
		locale = localeconv();
		char sysSep = (locale->decimal_point)[0];
		*sepPtr = sysSep;
		double retval = atof(buffer);
		free(buffer);
		return retval;
	}
}
