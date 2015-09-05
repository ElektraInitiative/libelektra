#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void doGetenv(const char *name)
{
	char *c = getenv(name);
	printf ("getenv(\"%s\") -> ", name);
	printf ("%s\n", c?c:"(null)");
}

int main(int argc, char** argv, char** environ)
{
	if (argc == 1)
	{
		char** env;
		for (env = environ; *env != 0; env++)
		{
			size_t len = strcspn(*env, "=");
			char name[len+1];
			strncpy(name, *env, len);
			name[len] = 0;
			doGetenv(name);
		}
	}
	else
	{
		for (int i=1; i<argc; ++i)
		{
			doGetenv(argv[i]);
		}
	}
	return 0;
}
