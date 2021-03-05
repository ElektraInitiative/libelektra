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

void printFormatConf (const FormatConf * conf)
{
	printf ("[%s]\n", conf->filePattern);
	printf ("indent_style = %s\n", ELEKTRA_TO_STRING (EnumIndentStyle) (conf->indentStyle));
	if (conf->indentSize == 0)
	{
		printf ("indent_size = tab\n");
	}
	else
	{
		printf ("indent_size = " ELEKTRA_UNSIGNED_LONG_F "\n", conf->indentSize);
	}
	if (conf->tabWidth > 0)
	{
		printf ("tab_width = " ELEKTRA_UNSIGNED_LONG_F "\n", conf->tabWidth);
	}
	if (conf->eol != LINE_END_NATIVE)
	{
		printf ("end_of_line = %s\n", ELEKTRA_TO_STRING (EnumLineEnd) (conf->eol));
	}
	printf ("charset = %s\n", ELEKTRA_TO_STRING (EnumCharset) (conf->charset));
	printf ("trim_trailing_whitespace = %s\n", conf->trim ? "true" : "false");
	printf ("insert_final_newline = %s\n", conf->eofNewline ? "true" : "false");
	if (conf->maxLineLength == 0)
	{
		printf ("max_line_length = off\n");
	}
	else
	{
		printf ("max_line_length = " ELEKTRA_UNSIGNED_LONG_F "\n", conf->maxLineLength);
	}
}

extern const char * const * environ;

int main (int argc, const char * const * argv)
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

	kdb_boolean_t isRootFile = elektraGet (elektra, ELEKTRA_TAG_ROOT);

	printf ("root = %s\n", isRootFile ? "true" : "false");

	kdb_long_long_t formatCount = elektraSize (elektra, ELEKTRA_TAG_FORMAT);

	for (kdb_long_long_t i = 0; i < formatCount; ++i)
	{
		FormatConf conf;
		elektraFillStructV (elektra, &conf, ELEKTRA_TAG_FORMAT, i);
		printFormatConf (&conf);
	}

	elektraClose (elektra);

	return EXIT_SUCCESS;
}
