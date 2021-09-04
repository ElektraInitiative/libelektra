/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define UNUSED __attribute__ ((unused))

#if defined(_WIN32)
#define FOLDER_DELIMITER '\\'
#define PLUGIN_PATH "src\\plugins\\"
#else
#define FOLDER_DELIMITER '/'
#define PLUGIN_PATH "src/plugins/"
#endif

/* The CMAKE_CACHE_FILENAME will be parsed to get the value of CMAKE_CACHE_VARNAME
 * which should be the absolute path the the source directory.
 * Each line of CMAKE_CACHE_FILENAME must fit in the CmakecacheFileReadBuffer.
 */
#define CMAKE_CACHE_FILE_READ_BUFFER 1024
#define CMAKE_CACHE_FILENAME "CMakeCache.txt"
#define CMAKE_CACHE_VARNAME "Elektra_SOURCE_DIR"
#define TEMP_FILENAME "temp"
#define INVALIDLINK_MESS "%s:%i:0 warning: invalid link '%s'\n"
#define HTTPLINK_FORMAT "%s:%i:0| %s\n"
#define HTTPLINK_FILENAME "external-links.txt"

// Link Blacklist: do not convert links with the following starting and ending
// ignore http, ftp, @ref and #anchor
const char * const ignoreTargetStart[] = { "#", "@", "http", "ftp", "" };

// Links with this endings will be transformed to links to the source.
// If not Blacklisted.
const char * const linksToSrc[] = { ".h", ".c", ".cpp", ".hpp", ".cmake", ".ini", "" };

// both need to be terminated with an empty string

// helpers
static void printTarget (FILE * output, char * target, size_t targetSize, char * inputFilename, int indexofElektraRoot, bool isMarkdown,
			 int lineCount);
static void printConvertedPath (FILE * output, char * path);
static int getIndexofElektraRoot (char * cmakeCacheFilename);
static void exitError (FILE * f1, FILE * f2, const char * mes);

/* These structs represent the transitions from the state machines.
 * Means if char c is read and the state machine is in state s
 * the next state is t[c][s].
 */
struct transitionLink
{
	int t[10][7];
};
struct transitionTitle
{
	int t[10][4];
};

/* This transition array represents the regex for a link, namely:
 *
 * not(\!)\[.*\][' '\n\t]*\(.*\)
 *
 * 0 is the initial state and 5 the goal state.
 * If the trap will be reached the input will be
 * set back. This transition array matches the
 * regex multiple times, which is correct due to the
 * fact that links can not be nested.
 */

const int linkStart = 0;
static inline int linkPossible (int old, int new)
{
	return (old == 0 || old == 5) && new == 2;
}
static inline int linkFound (int old, int new)
{
	return old == 4 && new == 5;
}
static inline int linkName (int old, int new)
{
	return ((old == 2 || old == 3) && (new == 2 || new == 3)) || (old == 3 && new == 4);
}
static inline int linkTarget (int old, int new)
{
	return old == 4 && new == 4;
}
static inline int linkTrap (int old UNUSED, int new)
{
	return new == 6;
}
static inline int linkNolink (int old UNUSED, int new)
{
	return new == 0 || new == 1;
}

struct transitionLink genLinkTransitionTable (void)
{
	struct transitionLink out = { {
		// char\state    0  1  2  3  4  5  6(trap)
		// clang-format off
		/* ! */			{1, 1, 2, 6, 4, 0, 0},
		/* [ */			{2, 0, 2, 6, 4, 2, 0},
		/* ] */			{0, 0, 3, 6, 4, 0, 0},
		/* blank */		{0, 0, 2, 3, 4, 0, 0},
		/* \n */		{0, 0, 2, 3, 4, 0, 0},
		/* ( */			{0, 0, 2, 4, 4, 0, 0},
		/* ) */			{0, 0, 2, 6, 5, 0, 0},
		/* # */			{0, 0, 2, 6, 4, 0, 0},
		/* other*/		{0, 0, 2, 6, 4, 0, 0},
		// clang-format on
	} };
	return out;
}

/* The next transition array matches the first title,
 * with the following regex:
 *
 * ^' '*#+.*$
 *
 * TITLE_START defines the start state and TITLE_ISGOAL ()
 * checks whether a title has matched.
 */

const int titleStart = 0;
static inline int titleIsGoal (int old, int new)
{
	return old == 3 && new == 0;
}

struct transitionTitle genTitleTransitionTable (void)
{
	struct transitionTitle out = { {
		// char\state    0  1  2  3
		// clang-format off
		/* ! */			{1, 1, 1, 3},
		/* [ */			{1, 1, 1, 3},
		/* ] */			{1, 1, 1, 3},
		/* blank */		{2, 1, 2, 3},
		/* \n */		{0, 0, 1, 0},
		/* ( */			{1, 1, 1, 3},
		/* ) */			{1, 1, 1, 3},
		/* # */			{3, 1, 3, 3},
		/* other*/		{1, 1, 1, 3},
		// clang-format on
	} };
	return out;
}

// Maps a given char to an int, used for the transition table
static int resolveChar (int c)
{
	if (c != '\n' && isblank (c)) return 3;
	switch (c)
	{
	case '!':
		return 0;
	case '[':
		return 1;
	case ']':
		return 2;
	case '\n':
		return 4;
	case '(':
		return 5;
	case ')':
		return 6;
	case '#':
		return 7;
	default:
		return 8; // all other
	}
}

/* This function prints out the input to the output, while printing the title
 * regex is applied. If a title is found a header ("{ #header }") will
 * be printed out additionally to the output and at the end true will
 * be returned. If no title is found false will be returned.
 */
static bool convertTitle (FILE * input, FILE * output, char * filenameInElektra)
{
	int state = titleStart;
	int newstate;
	int c;
	bool titleFound = false;
	struct transitionTitle transitions = genTitleTransitionTable ();
	while ((c = fgetc (input)) != EOF)
	{
		newstate = transitions.t[resolveChar (c)][state];

		if (!titleFound && titleIsGoal (state, newstate))
		{
			titleFound = true;
			// print Header
			fprintf (output, " {#");
			printConvertedPath (output, filenameInElektra);
			fprintf (output, "}");
		}

		fprintf (output, "%c", c);

		state = newstate;
	}
	return titleFound;
}

/* This procedure reads from the input and writes to the output, while
 * processing the file it converts all not Blacklisted links.
 */
static void convertLinks (FILE * input, FILE * output, char * inputFilename, int indexofElektraRoot)
{
	FILE * httplinks = fopen (HTTPLINK_FILENAME, "a");
	if (!httplinks)
	{
		fprintf (stderr, "WARNING http link file %s could not be opened\n", HTTPLINK_FILENAME);
	}
	int lineCount = 1;
	int c;
	fpos_t pos;
	int state = linkStart;
	int newstate;
	int sCount = 0;
	int index = 0;
	unsigned int len = 0;
	/* index and len help to convert a link if found.
	 *
	 *  |--index--||-len-|
	 * [link name](foo/bar)
	 */
	struct transitionLink transitions = genLinkTransitionTable ();
	while ((c = fgetc (input)) != EOF)
	{
		newstate = transitions.t[resolveChar (c)][state];
		if (c == '\n')
		{
			++lineCount;
		}
		if (linkPossible (state, newstate))
		{
			// first [, possible link
			// position is saved for setting back
			if (fgetpos (input, &pos)) exitError (input, NULL, "fgetpos");

			index = 0;
			len = 0;
		}
		else if (linkFound (state, newstate))
		{
			// set back and convert link if not blacklisted
			if (fsetpos (input, &pos)) exitError (input, NULL, "fsetpos");

			fprintf (output, "["); // first char got lost
			// print link name
			while (index > 0)
			{
				fprintf (output, "%c", fgetc (input));
				--index;
			}
			// extract target
			size_t targetSize = len * sizeof (char) + 1;
			char target[targetSize];
			if (fread (&target[0], sizeof (char), len, input) != len) exitError (input, NULL, "fread");

			target[len] = '\0';

			// check target
			bool targetOK = true;
			bool isMarkdown = true;
			// start
			for (int i = 0; strcmp (ignoreTargetStart[i], "") != 0; ++i)
			{
				if (strncmp (ignoreTargetStart[i], target, strlen (ignoreTargetStart[i])) == 0)
				{
					targetOK = false;
					if ((!strcmp (ignoreTargetStart[i], "ftp") || !strcmp (ignoreTargetStart[i], "http")) && httplinks)
						fprintf (httplinks, HTTPLINK_FORMAT, &inputFilename[indexofElektraRoot], lineCount, target);
					break;
				}
			}
			// end
			for (int i = 0; strcmp (linksToSrc[i], "") != 0; ++i)
			{
				if (len < strlen (linksToSrc[i])) continue;

				int j = len - strlen (linksToSrc[i]);
				if (strncmp (linksToSrc[i], &target[j], strlen (linksToSrc[i])) == 0)
				{
					isMarkdown = false;
					break;
				}
			}
			// print target
			if (targetOK)
			{
				printTarget (output, target, targetSize, inputFilename, indexofElektraRoot, isMarkdown, lineCount);
			}
			else
				fprintf (output, "%s", target);
			fprintf (output, "%c", fgetc (input)); // print ")"
		}
		else if (linkName (state, newstate))
		{
			++index;
		}
		else if (linkTarget (state, newstate))
			++len;
		else if (linkTrap (state, newstate))
		{
			// trap, reset
			if (fsetpos (input, &pos)) exitError (input, NULL, "fsetpos");

			fprintf (output, "["); // first char got lost
			state = linkStart;
			continue;
		}
		else if (c == '`')
		{
			sCount++;
			fprintf (output, "%c", c);
		}
		else if (sCount == 3)
		{
			sCount++;
			if (c != 's')
			{
				fprintf (output, "%c", c);
				sCount = 0;
			}
		}
		else if (sCount == 4)
		{
			sCount++;
			if (c != 'h')
			{
				sCount = 0;
				fprintf (output, "s%c", c);
			}
		}
		else if (sCount == 5)
		{
			sCount = 0;
			if (c != '\n')
				fprintf (output, "sh%c", c);
			else
				fprintf (output, "\n"); // swallow sh (from ```sh)
		}
		else if (linkNolink (state, newstate))
		{
			// print all other content
			fprintf (output, "%c", c);
			sCount = 0;
		}
		state = newstate;
	}
	if (httplinks)
	{
		fclose (httplinks);
	}
}

int main (int argc, char * argv[])
{
	if (argc < 2 || argc > 3)
	{
		fprintf (stderr, "Invalid number of arguments.\n");
		fprintf (stderr, "Usage: %s [<cmake-cache-file>] <input-file>\n", argv[0]);
		return EXIT_FAILURE;
	}

	char inputFilename[strlen (argv[argc - 1]) + 1];
	strcpy (inputFilename, argv[argc - 1]);
	int indexofElektraRoot = -1;
	if (argc == 3)
	{
		char cmakeCacheFilename[strlen (argv[1]) + 1];
		strcpy (cmakeCacheFilename, argv[1]);
		indexofElektraRoot = getIndexofElektraRoot (cmakeCacheFilename);
	}
	else
	{
		indexofElektraRoot = getIndexofElektraRoot (NULL);
	}

	if (0 > indexofElektraRoot) return EXIT_FAILURE;

	// 1st pass
	FILE * input = fopen (inputFilename, "r");
	if (!input)
	{
		fprintf (stderr, "fopen Error: file %s not found\n", inputFilename);
		return EXIT_FAILURE;
	}
	FILE * output = fopen (TEMP_FILENAME, "w+");
	if (!output)
	{
		fclose (input);
		fprintf (stderr, "fopen Error: tempfile %s not found\n", TEMP_FILENAME);
		return EXIT_FAILURE;
	}

	// save start of each file
	fpos_t startInput;
	fpos_t startTempFile;
	if (fgetpos (input, &startInput)) exitError (input, output, "fgetpos");
	if (fgetpos (output, &startTempFile)) exitError (input, output, "fgetpos");

	// detect plugins and give appropriate title
	if (!strncmp (PLUGIN_PATH, &inputFilename[indexofElektraRoot], strlen (PLUGIN_PATH)))
	{
		char * title = &inputFilename[indexofElektraRoot] + strlen (PLUGIN_PATH);
		// ignore src/plugins/README.md
		if (strcmp (title, "README.md"))
		{
			printf (" # Plugin: ");
			while ((*title) != FOLDER_DELIMITER)
			{
				printf ("%c", *title);
				++title;
			}
			printf (" #\n");
		}
	}

	if (!convertTitle (input, output, &inputFilename[indexofElektraRoot]))
	{
		/* No title found in file, therefore generate one and
		 * print it out.
		 */
		// reset input file
		if (fsetpos (input, &startInput)) exitError (input, output, "fsetpos");

		fclose (output);
		output = stdout;
		// Generate Title
		char * title = strrchr (inputFilename, FOLDER_DELIMITER);
		if (title == NULL) exitError (input, NULL, "parsed file path invalid");

		// print Title + Header
		fprintf (output, "# %s # {#", &title[1]);
		printConvertedPath (output, &inputFilename[indexofElektraRoot]);
		fprintf (output, "}\n");
		// 2nd pass (see README.md)
		convertLinks (input, output, inputFilename, indexofElektraRoot);
		fclose (input);
	}
	else
	{
		fclose (input);
		// reset temp file
		if (fsetpos (output, &startTempFile)) exitError (output, NULL, "fsetpos");

		// 2nd pass (see README.md)
		convertLinks (output, stdout, inputFilename, indexofElektraRoot);
		fclose (output);
	}
	remove (TEMP_FILENAME);

	return EXIT_SUCCESS;
}

static void printTarget (FILE * output, char * target, size_t targetSize, char * inputFilename, int indexofElektraRoot, bool isMarkdown,
			 int lineCount)
{
	char * backupTarget = target;
	char pathToLink[strlen (inputFilename) + strlen (target) + 11 + 1];
	// pathToLink cannot be longer than both stings + "_README.md" + terminating \0
	strcpy (pathToLink, inputFilename);
	// distinguish between relative and absolute targets
	if (target[0] != '/')
	{
		// determine how many times "../" is used in the target
		int folderBack = 0;
		while (!strncmp ("../", target, 3))
		{
			++folderBack;
			target = target + 3;
		}
		// reduce pathToLink folderBack times
		char * lastFolderDelimiter = strrchr (pathToLink, FOLDER_DELIMITER);
		if (lastFolderDelimiter != NULL)
		{
			while (folderBack > 0)
			{
				*lastFolderDelimiter = '\0';
				--folderBack;
				lastFolderDelimiter = strrchr (pathToLink, FOLDER_DELIMITER);
				if (lastFolderDelimiter == NULL)
				{
					fprintf (stderr, INVALIDLINK_MESS, inputFilename, lineCount, backupTarget);
					return;
				}
			}
			strcpy (++lastFolderDelimiter, target);
		}
	}
	else
	{
		++target; // remove starting /
		strcpy (&pathToLink[indexofElektraRoot], target);
	}

	// check if dir and validate link
	struct stat st;
	if (stat (pathToLink, &st) < 0)
	{
		fprintf (stderr, INVALIDLINK_MESS, inputFilename, lineCount, backupTarget);
		return;
	}
	if (S_ISDIR (st.st_mode))
	{
		if (backupTarget[targetSize - 1] == FOLDER_DELIMITER)
		{
			strcpy (&pathToLink[strlen (pathToLink)], "README.md");
		}
		else
		{
			strcpy (&pathToLink[strlen (pathToLink)], "_README.md");
		}
	}
	if (isMarkdown)
	{
		fprintf (output, "@ref ");
		printConvertedPath (output, &pathToLink[indexofElektraRoot]);
	}
	else
	{
		fprintf (output, "%s", pathToLink);
	}
}

/* This helper converts and prints a given path, all slashes or backslashes
 * and points will be replaced by underscore.
 */
static void printConvertedPath (FILE * output, char * path)
{
	char * workPath = path;
	while (*workPath != '\0')
	{
		switch (*workPath)
		{
		case '.':
		case '/':
		case '\\':
			fprintf (output, "_");
			break;
		default:
			fprintf (output, "%c", *workPath);
		}
		++workPath;
	}
}

/* Detect the index of the absolute path (in the elektra root folder) of
 * the parsed file, by parsing the CMAKE_CACHE_FILENAME file and getting
 * the length of the CMAKE_CACHE_VARNAME value, which contains the source path.
 */
static int getIndexofElektraRoot (char * cmakeCacheFilename)
{
	FILE * input = NULL;
	if (!cmakeCacheFilename)
	{
		cmakeCacheFilename = CMAKE_CACHE_FILENAME;
	}
	input = fopen (cmakeCacheFilename, "r");
	if (!input)
	{
		fprintf (stderr, "fopen Error: CmakeCacheFile %s not found\n", cmakeCacheFilename);
		return -1;
	}
	char line[CMAKE_CACHE_FILE_READ_BUFFER + 1];
	int lenCmakecacheVar = strlen (CMAKE_CACHE_VARNAME);
	bool foundCmakecacheVar = false;
	while (fgets (line, CMAKE_CACHE_FILE_READ_BUFFER, input))
	{
		if (strncmp (CMAKE_CACHE_VARNAME, line, lenCmakecacheVar) == 0)
		{
			foundCmakecacheVar = true;
			break;
		}
	}
	fclose (input);
	if (!foundCmakecacheVar)
	{
		fprintf (stderr, "%s parse Error: Variable %s not found\n", CMAKE_CACHE_FILENAME, CMAKE_CACHE_VARNAME);
		return -1;
	}
	char * CmakecacheVarValue = line;
	while (*CmakecacheVarValue && *CmakecacheVarValue != '=')
	{
		++CmakecacheVarValue;
	}
	++CmakecacheVarValue;
	return strlen (CmakecacheVarValue);
}

static void exitError (FILE * f1, FILE * f2, const char * mes)
{
	if (f1)
	{
		fclose (f1);
	}

	if (f2)
	{
		fclose (f2);
	}

	remove (TEMP_FILENAME);
	fprintf (stderr, "%s Error\n", mes);
	exit (EXIT_FAILURE);
}
