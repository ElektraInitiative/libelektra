#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>
#include <string.h>

#if defined(WIN32)
#define FOLDER_DELIMITER '\\'
#else
#define FOLDER_DELIMITER '/'
#endif

/* Slashes or backslashes to the elektra root.
 * Depends on the place of the executable, example:
 * /foo/bar/elektra/build/bin/executable
 * E:\\foo\bar\elektra\build\bin\executable
 * In both cases the there are 3 slashes or backslashes
 */
#define FOLDER_DELIMITER_TO_ELEKTRA_ROOT 3
#define TEMP_FILENAME "temp"

// Link Blacklist: do not convert links with the following starting and ending
char * ignoreTargetEnd [] = { ".h", ".c" , ".cpp" , ".hpp" , "\0"};
// ignore http, @ref and #anchor
char * ignoreTargetStart [] = { "#", "@" , "http" , "\0"};
// both need to be null terminated

// helpers
void printTarget(FILE * output, char * target, char * filenameInElektra);
void printConvertedPath (FILE * output ,char * path);
char * getPathInElektraRoot (char * executablename, char * filename);
void exitError (FILE * f1, FILE * f2, char * mes);

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
 * regex multiple times, witch is correct due to the
 * fact that links can not be nested.
 */

#define LINK_START 0
#define LINK_POSSIBLE(old, new) ((old == 0 || old == 5) && new == 2)
#define LINK_FOUND(old, new) (state == 4 && newstate == 5)
#define LINK_NAME(old, new) (((old == 2 || old == 3) && (new == 2  || new == 3)) \
								|| (old == 3 && new == 4))
#define LINK_TARGET(old, new) (state == 4 && newstate == 4)
#define LINK_TRAP(old, new) (new == 6)
#define LINK_NOLINK(old, new) (newstate == 0 || newstate == 1)

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

#define TITLE_START 0
#define TITLE_ISGOAL(old, new) (old == 3 && new == 0)

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
int resolveChar (char c)
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
bool convertTitle (FILE * input, FILE *  output, char * filenameInElektra)
{
	int state = TITLE_START;
	int newstate;
	char c;
	bool titleFound = false;
	struct transitionTitle transitions = genTitleTransitionTable ();
	while ((c = fgetc (input)) != EOF)
	{
		newstate = transitions.t[resolveChar (c)][state];

		if (!titleFound && TITLE_ISGOAL (state, newstate))
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
void convertLinks (FILE * input, FILE * output, char * filenameInElektra)
{
	char c;
	fpos_t pos;
	int state = LINK_START;
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
		if (LINK_POSSIBLE (state, newstate))
		{
			// first [, possible link
			// position is saved for setting back
			if (fgetpos (input, &pos))
				exitError (input, NULL, "fgetpos");

			index = 0;
			len = 0;
		}
		if (LINK_FOUND (state, newstate))
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
			for (int i = 0;strcmp (ignoreTargetStart[i], "\0") != 0;++i)
			{
				if (strncmp (ignoreTargetStart[i], target,
					strlen (ignoreTargetStart[i])) == 0)
				{
					targetOK = false;
					break;
				}
			}
			//end
			for (int i = 0;strcmp (ignoreTargetEnd[i], "\0") != 0;++i)
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
		if (LINK_NAME (state, newstate))
		{
			++index;
		}
		if (LINK_TARGET (state, newstate)) ++len;
		if (LINK_TRAP (state, newstate))
		{
			//trap, reset
			if (fsetpos (input, &pos))
				exitError (input, NULL, "fsetpos");

			fprintf (output, "["); //first char got lost
			state = LINK_START;
			continue;
		}
		if(LINK_NOLINK (state, newstate))
		{
			// print all other content
			fprintf (output, "%c", c);
		}
		state = newstate;
	}
}

int main (int argc, char *argv[])
{
	if (argc < 2)
	{
		fprintf (stderr, "args error\n");
		return EXIT_FAILURE;
	}

	char executablename[strlen (argv[0]) + 1];
	char filename[strlen (argv[1]) + 1];

	strcpy (executablename, argv[0]);
	strcpy (filename, argv[1]);

	char * filenameInElektra = getPathInElektraRoot (executablename, filename);

	//1st pass
	FILE * input = fopen (filename, "r");
	if (!input)
	{
		fprintf (stderr ,"fopen Error\n");
		return EXIT_FAILURE;
	}
	FILE * output = fopen (TEMP_FILENAME, "w+");
	if (!output)
	{
		fclose (input);
		fprintf (stderr ,"fopen Error\n");
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
		char * title = strrchr (filename, FOLDER_DELIMITER);
		if (title == NULL)
			exitError (input, NULL, "parsed file path invalid");

		// print Title + Header
		fprintf (output, "# %s # {#", &title[1]);
		printConvertedPath (output, filenameInElektra);
		fprintf (output, "}\n");
		//2nd pass
		convertLinks (input, output, filenameInElektra);
		fclose (input);
	} else
	{
		fclose (input);
		//reset temp file
		if (fsetpos (output, &startTempFile))
			exitError (output, NULL, "fsetpos");

		//2nd pass
		convertLinks (output, stdout, filenameInElektra);
		fclose (output);
	}
	remove (TEMP_FILENAME);

	return EXIT_SUCCESS;
}

void printTarget(FILE * output, char * target, char * filenameInElektra)
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
	}else
	{
		++target; // remove starting /
	}
	printConvertedPath (output, target);
}

/* This helper converts and prints a given path, all slashes or backslashes
 * and points will be replaced by underscore.
 */
void printConvertedPath (FILE * output, char * path)
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
 * file, by counting the slashes or backslashes from the executable
 * back to the Filesystem root, taking in to account the FOLDER_DELIMITER_TO_ELEKTRA_ROOT
 * constant. This count will then be used to determine the beginning of the
 * elektra root. The returned char pointer points to the beginning of
 * the the parsed file path in the Elektra root folder.
 * Example:
 * /foo/bar/test/elektra_src/build/bin/exe
 * /foo/bar/test/elektra_src/doc/help.md
 * with the FOLDER_DELIMITER_TO_ELEKTRA_ROOT set to 3 will return:
 * doc/help.md
 */
char * getPathInElektraRoot (char * executablename, char * filename)
{
	int countDeliToFsRoot = -(FOLDER_DELIMITER_TO_ELEKTRA_ROOT-1);
	for (int i = strlen (executablename);i >= 0;--i)
	{
		if(executablename[i] == FOLDER_DELIMITER) ++countDeliToFsRoot;
	}
	int fileDeliCount = 0;
	char * out = filename;
	while (countDeliToFsRoot > fileDeliCount)
	{
		if (*out == FOLDER_DELIMITER) ++fileDeliCount;
		++out;
	}
	return out;
}

void exitError (FILE * f1, FILE * f2, char * mes)
{
	if (f1)
		fclose (f1);

	if (f2)
		fclose (f2);

	remove (TEMP_FILENAME);
	fprintf (stderr ,"%s Error\n", mes);
	exit (EXIT_FAILURE);
}
