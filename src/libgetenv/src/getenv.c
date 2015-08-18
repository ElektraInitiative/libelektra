#define _GNU_SOURCE // RTLD_NEXT

#include <kdbgetenv.h>
#include <kdbconfig.h>

#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

Key *elektraParentKey;
KeySet *elektraConfig;
KDB *elektraRepo;

volatile sig_atomic_t elektraReloadCounter;
int elektraReloadCounterNext;

void elektraReload(int signal ELEKTRA_UNUSED)
{
}

int elektraCheckReload()
{
	return 0;
}

void elektraClose()
{
	if (!elektraRepo) return; // already closed

	kdbClose(elektraRepo, elektraParentKey);
	ksDel(elektraConfig);
	keyDel(elektraParentKey);
	elektraRepo = 0;
}

void elektraOpen(int* argc, char** argv)
{
	if (elektraRepo) elektraClose(); // already opened

	elektraParentKey = keyNew("user/sw/app/lift", KEY_END);
	elektraConfig = ksNew(20, KS_END);

	if (argc && argv)
	{
		ksAppendKey(elektraConfig, keyNew("/sw/app/lift/HOME", KEY_VALUE, "/home/markus", KEY_END));
		// TODO: parse argc, argv
		for (int i=0; i<*argc; ++i)
		{
			printf ("argv[%d]: %s\n", i, argv[i]);
		}
	}

	printf ("%s - %s\n", keyName(elektraParentKey), keyString(elektraParentKey));
	elektraRepo = kdbOpen(elektraParentKey);
	//TODO: install SIGHUP signal handler (on request)
	//TODO: parse arguments -> spec, remove "env"
	kdbGet(elektraRepo, elektraConfig, elektraParentKey);


	/*
	Key *c;
	ksRewind(elektraConfig);
	while ((c = ksNext(elektraConfig)))
	{
		printf ("%s - %s\n", keyName(c), keyString(c));
	}
	*/
}

int __real_main(int argc, char** argv, char** env);

typedef int (*fcn)(int *(main) (int, char * *, char * *), int argc, char ** argv, void (*init) (void), void (*fini) (void), void (*rtld_fini) (void), void (* stack_end));
int __libc_start_main(int *(main) (int, char * *, char * *), int argc, char ** argv, void (*init) (void), void (*fini) (void), void (*rtld_fini) (void), void (* stack_end))
{
	static union {void*d; fcn f;} start;
	if (!start.d) start.d = dlsym(RTLD_NEXT, "__libc_start_main");

	printf ("Main wrapped successfully\n");
	elektraOpen(&argc, argv);
	int ret = (*start.f)(main, argc, argv, init, fini, rtld_fini, stack_end);
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
	if (!key)
	{
		printf ("getenv with %s: <nothing>\n", fullName);
		return 0;
	}
	printf ("getenv with %s: %s\n", fullName, keyString(key));
	return (char*)keyString(key);
}

char *getenv(const char *name)
{
	char *ret = elektraGetEnv(name);
	return ret;
}

char *secure_getenv(const char *name)
{
	char * ret = elektraGetEnv(name);
	return ret;
}
