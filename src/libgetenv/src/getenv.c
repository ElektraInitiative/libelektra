#include <kdb.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

KDB *elektraKdb;
KeySet *elektraKs;

int main()
{
	printf ("%s\n", getenv("algorithm"));
	printf ("%s\n", getenv("limit"));
	return 0;
}

int __wrap_main(int argc, char** argv, char** env)
{
	Key *parentKey = keyNew("user/sw/app/lift", KEY_END);
	// printf ("%s - %s\n", keyName(parentKey), keyString(parentKey));
	elektraKdb = kdbOpen(parentKey);
	elektraKs = ksNew(20, KS_END);
	//TODO: install SIGHUP signal handler (on request)
	//TODO: parse arguments -> spec, remove "env"
	kdbGet(elektraKdb, elektraKs, parentKey);

	Key *c;
	ksRewind(elektraKs);
	/*
	while ((c = ksNext(elektraKs)))
	{
		printf ("%s - %s\n", keyName(c), keyString(c));
	}
	*/
	int ret = __real_main(argc, argv, env);
	//TODO: save configuration (on request)
	kdbClose(elektraKdb, parentKey);
	ksDel(elektraKs);
	return ret;
}

char *elektraGetEnv(const char *name)
{
	char fullName[strlen(name)+strlen("/sw/app/lift")+2];
	strcpy(fullName, "/sw/app/lift/");
	strcat(fullName, name);
	Key *key = ksLookupByName(elektraKs, fullName, 0);
	if (!key) return 0;
	// printf ("getenv called with %s: %s\n", fullName, keyString(key));
	return (char*)keyString(key);
}

char *__wrap_getenv(const char *name)
{
	return elektraGetEnv(name);
}

char *__wrap_secure_getenv(const char *name)
{
	return elektraGetEnv(name);
}
