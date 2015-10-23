#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>
#include <string.h>

#if defined(WIN32)
#define FSEP '\\'
#else
#define FSEP '/'
#endif

/* Slashes or backslashes to the elektra root.
 * Depends on the place of the executable, example:
 * /foo/bar/elektra/build/bin/executable
 * E:\\foo\bar\elektra\build\bin\executable
 * In both cases the FSEP_UNTIL_ROOT = 3
 */
#define FSEP_UNTIL_ROOT 3
#define TEMP_FILENAME "temp"

// Link Blacklist: do not convert links with the following starting and ending
char * ignoreTargetEnd [] = { ".h", ".c" , ".cpp" , ".hpp" , "\0"};
// ignore http, @ref and #anchor
char * ignoreTargetStart [] = { "#", "@" , "http" , "\0"};
// both need to be null terminated

void printHeader(FILE * output, char * filename, int FileDeliToRoot);
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
bool convertTitle (FILE * input, FILE *  output, char * filename, int FileDeliToRoot)
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
			printHeader (output, filename, FileDeliToRoot);
		}

		fprintf (output, "%c", c);

		state = newstate;
	}
	return titleFound;
}

/* This procedure reads from the input and writes to the output, while
 * processing the file it converts all not Blacklisted links.
 */
void convertLinks (FILE * input, FILE * output)
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
				fprintf (output, "@ref ");
				for (unsigned int i = 0;i < len;++i)
				{
					switch (target[i])
					{
						case '.':
						case FSEP: fprintf (output, "_");
									break;
						default: fprintf (output, "%c", target[i]);
					}
				}
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

	/* Detect the project folder depth by counting the slashes or
	 * backslashes to the root of the path.
	 */
	int cFileDeliToRoot = -(FSEP_UNTIL_ROOT-1);
	for (int i = strlen (argv[0]);i >= 0;--i)
	{
		if(argv[0][i] == FSEP) ++cFileDeliToRoot;
	}

	//1st pass
	FILE * input = fopen (argv[1], "r");
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

	if(!convertTitle (input, output, argv[1], cFileDeliToRoot))
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
		char * title = strrchr (argv[1], FSEP);
		if (!title)
			exitError (input, NULL, "strrchr");

		fprintf (output, "# %s #", &title[1]);
		printHeader (output, argv[1], cFileDeliToRoot);
		fprintf (output, "\n");
		//2nd pass
		convertLinks (input, output);
		fclose (input);
	} else
	{
		fclose (input);
		//reset temp file
		if (fsetpos (output, &startTempFile))
			exitError (output, NULL, "fsetpos");

		//2nd pass
		convertLinks (output, stdout);
		fclose (output);
	}
	remove (TEMP_FILENAME);

	return EXIT_SUCCESS;
}

/* Prints a header with the information given in filename, the
 * elektraroot is determined with the number of slashes or backslashes
 * from the system root to the elektra root folder.
 * Folder separator and dots will be replaced with underscores. Example:
 * /foo/bar/elektraroot/doc/help/help.md
 * is translated to:
 * doc_help_help_md
 */
void printHeader(FILE * output, char * filename, int FileDeliToRoot)
{
	fprintf (output, " {#");
	int FileDeliCount = 0;
	for (size_t i = 0; i < strlen (filename);++i)
	{
		if (FileDeliToRoot <= FileDeliCount)
		{
			switch (filename[i])
			{
				case '.':
				case FSEP: fprintf (output, "_");
							break;
				default: fprintf (output, "%c", filename[i]);
			}
		}
		if (filename[i] == FSEP) ++FileDeliCount;
	}
	fprintf (output, "}");
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
