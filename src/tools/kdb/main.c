/**
* @file
*
* @brief The KDB cli tool
*
* @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
*/

#include <mount.h>

#include <command.h>
#include <kdb.h>
#include <kdbgopts.h>
#include <kdbhelper.h>
#include <kdbinvoke.h>
#include <stdio.h>
#include <stdlib.h>

#define CLI_SPEC_KEY "spec:" CLI_BASE_KEY

extern char ** environ;

command subcommands[] = {
       { "mount", addMountSpec, execMount }
};

void printWarnings (Key * errorKey)
{
       const Key * warningsKey = keyGetMeta (errorKey, "warnings");
       if (warningsKey == NULL)
       {
	       return;
       }
       const char * warnings = keyString (warningsKey);
       warnings = warnings[1] == '_' ? warnings + 2 : warnings + 1;

       int warningsCount = atoi (warnings);
       char buff[8 + 1 + 1 + 11 + 1 + 6 + 1];
       for (int i = 0; i <= warningsCount; ++i)
       {
	       snprintf (buff, sizeof buff, "warnings/#%d/reason", i);
	       const char * warning = keyString (keyGetMeta (errorKey, buff));
	       printf ("WARNING: %s\n", warning);
       }
}

int main (int argc, char ** argv)
{
       return 0;
}
