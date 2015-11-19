#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>
#include <string.h>

#define UNUSED __attribute__ ((unused))

#if defined(WIN32)
#define FOLDER_DELIMITER '\\'
#else
#define FOLDER_DELIMITER '/'
#endif

/* The CMAKE_CACHE_FILENAME will be parsed to get the value of CMAKE_CACHE_VARNAME
 * which should be the absolute path the the source directory.
 * Each line of CMAKE_CACHE_FILENAME must fit in the CmakecacheFileReadBuffer.
 */
const int CmakecacheFileReadBuffer = 1024;
#define CMAKE_CACHE_FILENAME "CMakeCache.txt"
#define CMAKE_CACHE_VARNAME "Elektra_SOURCE_DIR"
#define TEMP_FILENAME "temp"

// Link Blacklist: do not convert links with the following starting and ending
const char * const ignoreTargetEnd [] = { ".h", ".c" , ".cpp" , ".hpp" , ""};
// ignore http, @ref and #anchor
const char * const ignoreTargetStart [] = { "#", "@" , "http" , ""};
// both need to be terminated with an empty string

// helpers
static void printTarget(FILE * output, char * target, char * filenameInElektra);
static void printConvertedPath (FILE * output, char * path);
static char * getPathInElektraRoot (char * inputFilename, char * cmakeCacheFilename);
static void exitError (FILE * f1, FILE * f2, const char * mes);

/* These structs represent the transitions from the state machines.
 * Means if char c is read and the state machine is in state s
 * the next state is t[c][s].
 */
struct transitionLink { int t[10][7]; };
struct transitionTitle { int t[10][4]; };

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
	return ((old == 2 || old == 3) && (new == 2  || new == 3))
								|| (old == 3 && new == 4);
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

struct transitionLink genLinkTransitionTable() {
    struct transitionLink out = {
        {//char\state    0  1  2  3  4  5  6(trap)
		/* ! */			{1, 1, 2, 6, 4, 0, 0},
		/* [ */			{2, 0, 2, 6, 4, 2, 0},
		/* ] */			{0, 0, 3, 6, 4, 0, 0},
		/* blank */		{0, 0, 2, 3, 4, 0, 0},
		/* \n */		{0, 0, 2, 3, 4, 0, 0},
		/* ( */			{0, 0, 2, 4, 4, 0, 0},
		/* ) */			{0, 0, 2, 6, 5, 0, 0},
		/* # */			{0, 0, 2, 6, 4, 0, 0},
		/* other*/		{0, 0, 2, 6, 4, 0, 0},
        }
    };
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

struct transitionTitle genTitleTransitionTable() {
    struct transitionTitle out = {
        {//char\state    0  1  2  3
		/* ! */			{1, 1, 1, 3},
		/* [ */			{1, 1, 1, 3},
		/* ] */			{1, 1, 1, 3},
		/* blank */		{2, 1, 2, 3},
		/* \n */		{0, 0, 1, 0},
		/* ( */			{1, 1, 1, 3},
		/* ) */			{1, 1, 1, 3},
		/* # */			{3, 1, 3, 3},
		/* other*/		{1, 1, 1, 3},
        }
    };
    return out;
}

// Maps a given char to a int, used for the transition table
static int resolveChar (char c)
{
	if (c != '\n' && isblank (c)) return 3;
	switch (c)
	{
		case '!': return 0;
		case '[': return 1;
		case ']': return 2;
		case '\n': return 4;
		case '(': return 5;
		case ')': return 6;
		case '#': return 7;
		default: return 8; // all other
	}
}

/* This function prints out the input to the output, while printing the title
 * regex is applied. If a title is found a header ("{ #header }") will
 * be printed out additionally to the output and at the end true will
 * be returned. If no title is found false will be returned.
 */
static bool convertTitle (FILE * input, FILE *  output, char * filenameInElektra)
{
	int state = titleStart;
	int newstate;
	char c;
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
static void convertLinks (FILE * input, FILE * output, char * filenameInElektra)
{
	char c;
	fpos_t pos;
	int state = linkStart;
	int newstate;
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
		if (linkPossible (state, newstate))
		{
			// first [, possible link
			// position is saved for setting back
			if (fgetpos (input, &pos))
				exitError (input, NULL, "fgetpos");

			index = 0;
			len = 0;
		}
		else if (linkFound (state, newstate))
		{
			//set back and convert link if not blacklisted
			if (fsetpos (input, &pos))
				exitError (input, NULL, "fsetpos");

			fprintf (output, "["); //first char got lost
			//print link name
			while (index > 0)
			{
				fprintf (output, "%c", fgetc (input));
				--index;
			}
			//extract target
			char target [len * sizeof (char) + 1];
			if (fread (&target[0], sizeof (char), len, input) != len)
				exitError (input, NULL, "fread");

			target[len] = '\0';

			//check target
			bool targetOK = true;
			//start
			for (int i = 0;strcmp (ignoreTargetStart[i], "") != 0;++i)
			{
				if (strncmp (ignoreTargetStart[i], target,
					strlen (ignoreTargetStart[i])) == 0)
				{
					targetOK = false;
					break;
				}
			}
			//end
			for (int i = 0;strcmp (ignoreTargetEnd[i], "") != 0;++i)
			{
				if(len < strlen (ignoreTargetEnd[i]))
					continue;

				int j = len - strlen (ignoreTargetEnd[i]);
				if (strncmp (ignoreTargetEnd[i], &target[j],
					strlen (ignoreTargetEnd[i])) == 0)
				{
					targetOK = false;
					break;
				}
			}
			//print target
			if (targetOK)
			{
				printTarget (output,target, filenameInElektra);
			}
			else fprintf (output, "%s", target);
			fprintf (output, "%c", fgetc (input)); // print ")"
		}
		else if (linkName (state, newstate))
		{
			++index;
		}
		else if (linkTarget (state, newstate)) ++len;
		else if (linkTrap (state, newstate))
		{
			//trap, reset
			if (fsetpos (input, &pos))
				exitError (input, NULL, "fsetpos");

			fprintf (output, "["); //first char got lost
			state = linkStart;
			continue;
		}
		else if(linkNolink (state, newstate))
		{
			// print all other content
			fprintf (output, "%c", c);
		}
		state = newstate;
	}
}

int main (int argc, char *argv[])
{
	if (argc < 2 || argc > 3)
	{
		fprintf (stderr, "Argument Error: expected format <filter> [<cmake-cache-file>] <input-file>\n");
		return EXIT_FAILURE;
	}

	char inputFilename [strlen (argv[argc - 1]) + 1];
	strcpy (inputFilename, argv[argc - 1]);
	char * filenameInElektra = NULL;
	if (argc == 3)
	{
		char cmakeCacheFilename [strlen (argv[1]) + 1];
		strcpy (cmakeCacheFilename, argv[1]);
		filenameInElektra = getPathInElektraRoot (inputFilename, cmakeCacheFilename);
	} else {
		filenameInElektra = getPathInElektraRoot (inputFilename, NULL);
	}

	if (!filenameInElektra)
		return EXIT_FAILURE;

	//1st pass
	FILE * input = fopen (inputFilename, "r");
	if (!input)
	{
		fprintf (stderr ,"fopen Error: file %s not found\n", inputFilename);
		return EXIT_FAILURE;
	}
	FILE * output = fopen (TEMP_FILENAME, "w+");
	if (!output)
	{
		fclose (input);
		fprintf (stderr ,"fopen Error: tempfile %s not found\n", TEMP_FILENAME);
		return EXIT_FAILURE;
	}

	//save start of each file
	fpos_t startInput;
	fpos_t startTempFile;
	if (fgetpos (input, &startInput))
		exitError (input, output, "fgetpos");
	if (fgetpos (output, &startTempFile))
		exitError (input, output, "fgetpos");

	if(!convertTitle (input, output, filenameInElektra))
	{
		/* No title found in file, therefore generate one and
		 * print it out.
		 */
		//reset input file
		if (fsetpos (input, &startInput))
			exitError (input, output, "fsetpos");

		fclose (output);
		output = stdout;
		//Generate Title
		char * title = strrchr (inputFilename, FOLDER_DELIMITER);
		if (title == NULL)
			exitError (input, NULL, "parsed file path invalid");

		// print Title + Header
		fprintf (output, "# %s # {#", &title[1]);
		printConvertedPath (output, filenameInElektra);
		fprintf (output, "}\n");
		//2nd pass (see README.md)
		convertLinks (input, output, filenameInElektra);
		fclose (input);
	} else {
		fclose (input);
		//reset temp file
		if (fsetpos (output, &startTempFile))
			exitError (output, NULL, "fsetpos");

		//2nd pass (see README.md)
		convertLinks (output, stdout, filenameInElektra);
		fclose (output);
	}
	remove (TEMP_FILENAME);

	return EXIT_SUCCESS;
}

static void printTarget(FILE * output, char * target, char * filenameInElektra)
{
	fprintf (output, "@ref ");
	// distinguish between relative and absolute targets
	if (target[0] != '/')
	{
		//copy filenameInElektra and extend it with FOLDER_DELIMITER at start.
		char copyFilenameData[strlen (filenameInElektra) + 2];
		strcpy (&copyFilenameData[1], filenameInElektra);
		copyFilenameData[0] = FOLDER_DELIMITER;
		char * copyFilename = copyFilenameData;
		// determine how many times "../" is used in the target
		int folderBack = 0;
		while (!strncmp ("../", target, 3))
		{
			++folderBack;
			target = target + 3;
		}
		// go folderBack times upwards in filenameInElektra
		char * lastFolderDelimiter = strrchr (&copyFilenameData[1], FOLDER_DELIMITER);
		if (lastFolderDelimiter != NULL)
		{
			*lastFolderDelimiter = '\0';
			// not in root so target needs to be extended with path
			while (folderBack>0)
			{
				--folderBack;
				lastFolderDelimiter = strrchr (copyFilename, FOLDER_DELIMITER);
				if (lastFolderDelimiter == NULL)
				{
					fprintf (output, "INVALID LINK");
					return;
				}
				*lastFolderDelimiter = '\0';
			}
			// if copyFilename was not reduced print it
			if (copyFilename[0] != '\0')
			{
				printConvertedPath (output, &copyFilename[1]);
				fprintf (output, "_");
			}
		}
	} else {
		++target; // remove starting /
	}
	printConvertedPath (output, target);
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
			case '\\': fprintf (output, "_");
						break;
			default: fprintf (output, "%c", *workPath);
		}
		++workPath;
	}
	if (*(--workPath) == '/') fprintf (output, "README_md");
}

/* Detect the absolute path (in the elektra root folder) of the parsed
 * file, by parsing the CMAKE_CACHE_FILENAME file and getting the CMAKE_CACHE_VARNAME,
 * which contains the source path.
 */
static char * getPathInElektraRoot (char * inputFilename, char * cmakeCacheFilename)
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
		return NULL;
	}
	char line [CmakecacheFileReadBuffer];
	int lenCmakecacheVar = strlen (CMAKE_CACHE_VARNAME);
	bool foundCmakecacheVar = false;
	while (fgets (line, CmakecacheFileReadBuffer, input))
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
		fprintf (stderr, "%s parse Error: Variable %s not found\n",
							CMAKE_CACHE_FILENAME, CMAKE_CACHE_VARNAME);
		return NULL;
	}
	char * CmakecacheVarValue = line;
	while (*CmakecacheVarValue && *CmakecacheVarValue != FOLDER_DELIMITER) ++CmakecacheVarValue;
	//calculate filename in elektra root
	char * out = inputFilename + strlen (CmakecacheVarValue);
	return out;
}

static void exitError (FILE * f1, FILE * f2, const char * mes)
{
	if (f1)
		fclose (f1);

	if (f2)
		fclose (f2);

	remove (TEMP_FILENAME);
	fprintf (stderr ,"%s Error\n", mes);
	exit (EXIT_FAILURE);
}
