/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <genelektra.h>

#include <stdio.h>
#include <stdlib.h>

static void onFatalError (ElektraError * error)
{
	fprintf (stderr, "ERROR: %s\n", elektraErrorDescription (error));
	elektraErrorReset (&error);
	exit (EXIT_FAILURE);
}

void printTree (Tree * tree, int level)
{
	if (tree == NULL)
	{
		printf ("%*s%*s(empty)\n", level, "", level, "");
		return;
	}

	printf ("%*s%*s%s\n", level, "", level, "", tree->text);
	for (kdb_long_long_t i = 0; i < tree->childCount; ++i)
	{
		printTree (tree->children[i], level + 1);
	}
}

int main (int argc, const char ** argv)
{
	specloadCheck (argc, argv);

	ElektraError * error = NULL;
	Elektra * elektra = NULL;
	int rc = loadConfiguration (&elektra, &error);

	if (rc == -1)
	{
		fprintf (stderr, "An error occurred while opening elektra: %s", elektraErrorDescription (error));
		elektraErrorReset (&error);
		return EXIT_FAILURE;
	}

	if (rc == 1)
	{
		// help mode
		printHelpMessage (elektra, NULL, NULL);
		elektraClose (elektra);
		return EXIT_SUCCESS;
	}

	elektraFatalErrorHandler (elektra, onFatalError);

	Tree * tree = elektraGet (elektra, ELEKTRA_TAG_ROOT);

	printTree (tree, 0);

	ELEKTRA_STRUCT_FREE (StructTree) (&tree);
	elektraClose (elektra);

	return EXIT_SUCCESS;
}
