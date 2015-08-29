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

const char *elektraHelpText =
"This application is elektrified using libelektragetenv.\n"
"This is a LD_PRELOAD technique to elektrify applications\n"
"that use getenv().\n"
"\n"
"\n"
"Instead of searching in the environment (environ), getenv() will use\n"
"ksLookup() to lookup configuration.\n"
"Two different ksLookup() will be done:\n"
" - first an application-specific lookup using /sw/<app-name>/current\n"
"   where <app-name> is different for every application.\n"
" - second a fallback lookup using /sw/env/current\n"
"   which is the same for every application\n"
"\n"
"OPTIONS\n"
"\n"
" --elektra-help             .. show this text\n"
" --elektra-app-name=key     .. the application name to be used instead of argv[0]\n"
" --elektra-app-profile=key  .. the application profile to be used instead of current\n"
" --elektra-env-profile=key  .. the environment profile to be used instead of current\n"
" --elektra-proc:key=value   .. set a key/value below root to be preferred\n"
"                               (in proc-namespace)\n"
"\n"
"Note that keys can contain / to form hierarchies.\n"
"Every option starting with --elektra- will be discarded from argv\n"
"before the application's main function is started.\n"
"\n"
"\n"
// "INTERNAL OPTIONS\n"
// "\n"
// "Some options can be used to change the behaviour of the library itself.\n"
// "\n"
// "see spec/getenv/current\n"
// "\n"
// "\n"
"EXAMPLES\n"
"\n"
"> elektrify-getenv man man --elektra-proc:MANWIDTH=40\n"
"\n"
"Will use MANWIDTH 40 for this invocation of man man.\n"
"This feature is handy, if an option is only available\n"
"by environment, but not by command-line arguments,\n"
"because sometimes environment variables are not trivial\n"
"to set (e.g. in Makefiles)-\n"
"\n"
"\n"
"> kdb set user/sw/man/current/MANOPT --regex\n"
"\n"
"Will permanently and user-wide change MANOPT to include --regex, so that -K\n"
"and similar options automatically prefer regular expressions.\n"
"This feature is handy to change the default behaviour of\n"
"applications (either system, user or directory-wide).\n"
"\n"
"\n"
"> kdb set system/sw/env/current/HTTP_PROXY http://proxy.hogege.com:8000/\n"
"\n"
"Will permanently and system-wide change the proxy for all applications\n"
"that honor HTTP_PROXY, e.g. w3m.\n"
"\n"
"\n";

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
	const char *appName = "/sw/app/current";

	// will be used to append configuration
	elektraConfig = ksNew(20, KS_END);

	if (argc && argv)
	{
		const char *argPrefix = "--elektra";
		size_t argPrefixSize = sizeof(argPrefix);

		const char *rootPath = "/sw/app/lift/";
		size_t rootPathSize = sizeof(rootPath);

		ksAppendKey(elektraConfig, keyNew("HOME", KEY_VALUE, "/home/markus", KEY_END));
		// TODO: parse argc, argv
		for (int i=0; i<*argc; ++i)
		{
			if (!strncmp(argv[i], argPrefix, argPrefixSize))
			{
				char *kv = argv[i]+argPrefixSize;
				if (kv[0] == ':')
				{
					char *v = strchr(kv, '=');
					char *keyname = elektraFormat("%s/%s", rootPath, kv);
					ksAppendKey(elektraConfig, keyNew(keyname, KEY_VALUE, "", KEY_END));
					free(keyname);
				}
				printf ("kv: %s\n", kv);
			}
			printf ("argv[%d]: %s\n", i, argv[i]);
		}
	}
	*/

	elektraParentKey = keyNew("user/sw/app/lift", KEY_END);
	printf ("%s - %s\n", keyName(elektraParentKey), keyString(elektraParentKey));
	elektraRepo = kdbOpen(elektraParentKey);
	//TODO: install SIGHUP signal handler (on request)
	//TODO: parse arguments -> spec, remove "env"
	kdbGet(elektraRepo, elektraConfig, elektraParentKey);
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
