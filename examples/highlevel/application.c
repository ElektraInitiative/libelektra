/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/elektra.h>

#include <stdio.h>
#include <stdlib.h>

static void onFatalError (ElektraError * error)
{
	fprintf (stderr, "ERROR: %s\n", elektraErrorDescription (error));
	exit (EXIT_FAILURE);
}

int main (int argc, char ** argv)
{
	ElektraError * error = NULL;
	Elektra * elektra = elektraOpen ("/sw/example/highlevel/#0/current", NULL, &error);
	if (elektra == NULL)
	{
		fprintf (stderr, "An error occured while opening elektra: %s", elektraErrorDescription (error));
		elektraErrorReset (&error);
		return EXIT_FAILURE;
	}

	elektraFatalErrorHandler (elektra, onFatalError);

	const char * mystring = elektraGetString (elektra, "mystring");
	const int myint = elektraGetLong (elektra, "myint");
	const double mydouble = elektraGetDouble (elektra, "mydouble");

	const size_t size = elektraArraySize (elektra, "myfloatarray");
	float * myfloatarray = calloc (size, sizeof (float));

	for (int i = 0; i < size; ++i)
	{
		myfloatarray[i] = elektraGetFloatArrayElement (elektra, "myfloatarray", i);
	}

	const bool print = elektraGetBoolean (elektra, "print");

	printf ("successfully read configuration\n");

	if (print)
	{
		printf ("mystring: %s\nmyint: %d\nmydouble: %f\nsizeof(myfloatarray): %ld", mystring, myint, mydouble, size);
		for (int i = 0; i < size; ++i)
		{
			printf ("\nmyfloatarray[%d]: %f", i, myfloatarray[i]);
		}
		printf ("\n");
	}

	free (myfloatarray);

	elektraClose (elektra);

	return EXIT_SUCCESS;
}
