#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>
#include <string.h>

#define FSEP '/'
#define FSEP_UNTIL_ROOT 3
#define TEMP_OUTPUTFILENAME "temp"

// do not convert links with the following starting and ending
// needs to be null terminated
char * ignoreTargetEnd [] = { ".h", ".c" , ".cpp" , ".hpp" , "\0"};
// ignore http, @ref and #anchor
char * ignoreTargetStart [] = { "#", "@" , "http" , "\0"};

//t[char][state]
struct transitionLink { int t[10][7]; };
struct transitionTitle { int t[10][7]; };

//QUEST wtf??  stack?
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
		/* [a-z0-9] */	{0, 0, 2, 6, 4, 0, 0},
		/* other*/		{0, 0, 2, 6, 4, 0, 0},
        }
    };
    return out;
}

struct transitionTitle genTitleTransitionTable() {
    struct transitionTitle out = {
        {//char\state    0  1  2  3  4  5  6(trap)
		/* ! */			{1, 1, 7, 7, 7, 5, 0},
		/* [ */			{1, 1, 7, 7, 7, 5, 0},
		/* ] */			{1, 1, 7, 7, 7, 5, 0},
		/* blank */		{2, 1, 2, 4, 4, 5, 0},
		/* \n */		{0, 0, 7, 7, 7, 0, 0},
		/* ( */			{1, 1, 7, 7, 7, 5, 0},
		/* ) */			{1, 1, 7, 7, 7, 5, 0},
		/* # */			{1, 1, 3, 3, 7, 5, 0},
		/* [a-z0-9] */	{1, 1, 7, 7, 5, 5, 0},
		/* other*/		{1, 1, 7, 7, 7, 5, 0},
        }
    };
    return out;
}

// Maps a given char to a int, used for the transition table
int resolveChar (char c)
{
	if (isalnum (c)) return 8;
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
		default: return 9; // all other
	}
}


int main (int argc, char *argv[])
{
	if (argc < 2)
	{
		fprintf (stderr, "args error\n");
		return EXIT_FAILURE;
	}

	// detect the project folder depth
	int cFileDeliToRoot = -(FSEP_UNTIL_ROOT-1);
	for (int i = strlen (argv[0]);i >= 0;--i)
	{
		if(argv[0][i] == FSEP) ++cFileDeliToRoot;
	}
	//~ fprintf (stderr, "%i\n", cFileDeliToRoot);

	FILE * input = fopen (argv[1], "r");
	FILE * output = fopen (TEMP_OUTPUTFILENAME, "w");

	char c;
	fpos_t pos;
	int state = 0;
	int newstate;
	int index = 0;
	unsigned int len = 0;
	/*  |--index--||-len-|
	 * [link name](foo/bar)
	 */
	struct transitionLink transitions = genLinkTransitionTable ();
	while ((c = fgetc (input)) != EOF)
	{
		newstate = transitions.t[resolveChar (c)][state];
		if ((state == 0 || state == 5)  && newstate == 2)
		{
			// first [, possible link
			fgetpos (input, &pos);
			index = 0;
			len = 0;
		}
		if (state == 4 && newstate == 5)
		{
			// found link
			fprintf (output, "link  i=%i  l=%i\n",index,len);
			fsetpos (input, &pos);
			fprintf (output, "["); //first char got lost
			//print link name
			while (index > 0)
			{
				fprintf (output, "%c", fgetc (input));
				--index;
			}
			//extract target
			char target [len * sizeof (char) + 1];
			fread (&target[0], sizeof (char), len, input);
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
				for (unsigned int i = 0;i < len;++i)
				{
					switch (target[i])
					{
						case FSEP: fprintf (output, "_");
									break;
						case '.': i = len; // abort
									break;
						default: fprintf (output, "%c", target[i]);
					}
				}
			}
			else fprintf (output, "%s",target);
			fprintf (output, "%c", fgetc (input)); // print ")"
		}
		if (((state == 2 || state == 3) && (newstate == 2  || newstate == 3))
			|| (state == 3 && newstate == 4)	)
		{
			++index;
		}
		if (state == 4 && newstate == 4) ++len;
		if (newstate == 6)
		{
			//trap, reset
			fsetpos (input, &pos);
			fprintf (output, "["); //first char got lost
			state = 0;
			continue;
		}
		if(newstate == 0 || newstate == 1)
		{
			// print all other then links
			fprintf (output, "%c", c);
		}
		state = newstate;
	}
	fclose (input);
	fclose (output);

	//2nd pass
	input = fopen (TEMP_OUTPUTFILENAME, "r");
	fpos_t startPos;
	fgetpos (&startPos);
	state = 0;
	while ((c = fgetc (input)) != EOF)
	{
		fprintf (stdout, "%c", c);
	}









	fclose (input);
	remove (TEMP_OUTPUTFILENAME);
	return EXIT_SUCCESS;
}

