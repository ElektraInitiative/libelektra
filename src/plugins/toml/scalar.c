#include "scalar.h"

Scalar * createScalar (ScalarType type, char * scalarString, size_t line)
{
	Scalar * scalar = malloc (sizeof (Scalar));
	scalar->type = type;
	scalar->str = scalarString;
	scalar->line = line;
	return scalar;
}

Scalar * createScalarDup (ScalarType type, const char * scalarString, size_t line)
{
	Scalar * scalar = malloc (sizeof (Scalar));
	scalar->type = type;
	if (scalarString != NULL)
	{
		scalar->str = strdup (scalarString);
	}
	else
	{
		scalar->str = NULL;
	}
	scalar->line = line;
	return scalar;
}
