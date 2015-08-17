#include <kdb.h>
#include <kdbconfig.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

KDB *elektraRepo;
KeySet *elektraConfig;
Key *elektraParentKey;

volatile sig_atomic_t elektraReloadCounter;
int elektraReloadCounterNext;

void elektraReload(int signal ELEKTRA_UNUSED)
{
}

int elektraCheckReload()
{
	return 0;
}

void elektraOpen(int* argc, char** argv)
{
	elektraParentKey = keyNew("user/sw/app/lift", KEY_END);
	printf ("%s - %s\n", keyName(elektraParentKey), keyString(elektraParentKey));
	elektraRepo = kdbOpen(elektraParentKey);
	elektraConfig = ksNew(20, KS_END);
	//TODO: install SIGHUP signal handler (on request)
	//TODO: parse arguments -> spec, remove "env"
	kdbGet(elektraRepo, elektraConfig, elektraParentKey);

	for (int i=1; i<*argc; ++i)
	{
		printf ("argv[%d]: %s\n", i, argv[i]);
	}


	/*
	Key *c;
	ksRewind(elektraConfig);
	while ((c = ksNext(elektraConfig)))
	{
		printf ("%s - %s\n", keyName(c), keyString(c));
	}
	*/
}

void elektraClose()
{
	kdbClose(elektraRepo, elektraParentKey);
	ksDel(elektraConfig);
	keyDel(elektraParentKey);
}

int __real_main(int argc, char** argv, char** env);

int __wrap_main(int argc, char** argv, char** env)
{
	printf ("Main wrapped successfully\n");
	elektraOpen(&argc, argv);
	int ret = __real_main(argc, argv, env);
	//TODO: save configuration (on request)
	elektraClose();
	return ret;
}

char *elektraGetEnv(const char *name)
{
	char fullName[strlen(name)+strlen("/sw/app/lift")+2];
	strcpy(fullName, "/sw/app/lift/");
	strcat(fullName, name);
	Key *key = ksLookupByName(elektraConfig, fullName, 0);
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
