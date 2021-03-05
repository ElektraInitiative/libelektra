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
#include <string.h>

static void onFatalError (ElektraError * error)
{
	fprintf (stderr, "ERROR: %s\n", elektraErrorDescription (error));
	elektraErrorReset (&error);
	exit (EXIT_FAILURE);
}

void showMenu (Menu * menu)
{
	printf ("%s:\n\n", menu->name);
	if (menu->command[0] != '\0')
	{
		printf ("$ %s\n\n", menu->command);
		printf ("  [0] Run command\n");
	}
	else if (menu->childCount == 0)
	{
		printf ("(menu empty)\n");
		return;
	}

	for (kdb_long_long_t i = 0; i < menu->childCount; ++i)
	{
		printf ("  [" ELEKTRA_LONG_LONG_F "] %s\n", i + 1, menu->children[i]->name);
	}

	char * end;
	char buf[20];
	int selection = -1;
	int minSelection = menu->command[0] != '\0' ? 0 : 1;

	do
	{
		printf ("\nPlease select what to do (Ctrl-D = quit): ");
		if (!fgets (buf, sizeof (buf), stdin))
		{
			break;
		}

		// remove \n
		buf[strlen (buf) - 1] = 0;

		selection = strtol (buf, &end, 10);
	} while (end != buf + strlen (buf) || selection > menu->childCount || selection < minSelection);

	printf ("\n\n");

	if (selection == 0)
	{
		system (menu->command);
	}
	else if (selection > 0)
	{
		showMenu (menu->children[selection - 1]);
	}
}

extern const char * const * environ;

int main (int argc, const char ** argv)
{
	exitForSpecload (argc, argv);

	ElektraError * error = NULL;
	Elektra * elektra = NULL;
	int rc = loadConfiguration (&elektra, argc, argv, environ, &error);

	if (rc == -1)
	{
		fprintf (stderr, "An error occurred while opening Elektra: %s", elektraErrorDescription (error));
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

	Menu * menu = elektraGet (elektra, ELEKTRA_TAG_MAIN);

	if (menu == NULL)
	{
		printf ("no menu found\n");
	}
	else
	{
		showMenu (menu);
	}

	ELEKTRA_STRUCT_FREE (StructMenu) (&menu);
	elektraClose (elektra);

	return EXIT_SUCCESS;
}
