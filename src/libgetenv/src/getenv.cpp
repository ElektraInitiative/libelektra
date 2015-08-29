/**
 * \file
 *
 * \brief Source for the getenv library
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef _GNU_SOURCE
# define _GNU_SOURCE // RTLD_NEXT
#endif

#include <kdbgetenv.h>
#include <kdbconfig.h>

#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#include <string>
#include <sstream>
#include <iostream>

using namespace std;
using namespace ckdb;

namespace ckdb {

Key *elektraParentKey;
KeySet *elektraConfig;
KDB *elektraRepo;

const char *appName = "/sw/app/current";
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
" --elektra:key=value        .. set a key/value below root to be preferred\n"
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
"> elektrify-getenv man man --elektra:MANWIDTH=40\n"
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

static pthread_mutex_t elektraGetEnvMutex = PTHREAD_MUTEX_INITIALIZER;

void printVersion()
{
	cout << "Elektra getenv is active" << std::endl;
	Key *k = keyNew("system/elektra/version", KEY_END);
	KDB *kdb = kdbOpen(k);
	KeySet *c = ksNew(20, KS_END);
	kdbGet (kdb, c, k);
	kdbClose(kdb, k);
	keyDel(k);
	Key *kdb_version = ksLookupByName(c, "system/elektra/version/constants/KDB_VERSION", 0);
	if (!kdb_version)
	{
		cerr << "Could not lookup KDB_VERSION key" << endl;
	}
	else
	{
		cout << "KDB_VERSION: " << keyString(kdb_version) << endl;
	}
	cout << "KDB_GETENV_VERSION: " << KDB_GETENV_VERSION << endl;
	ksDel(c);
}

void addProc(string kv)
{
	cout << "kv is: " << kv << endl;
	stringstream ss(kv);
	string k, v;
	getline(ss, k, '=');
	getline(ss, v);
	cout << "k is " << k << " and v is " << v << endl;

	string fullName = "proc/";
	fullName += k;
	ksAppendKey(elektraConfig, keyNew(fullName.c_str(), KEY_VALUE, v.c_str(), KEY_END));
}

void parseArgs(int* argc, char** argv)
{
	string prefix = "--elektra";

	string rootPath = "/sw/app/lift/";

	ksAppendKey(elektraConfig, keyNew("HOME", KEY_VALUE, "/home/markus", KEY_END));
	// TODO: parse argc, argv
	for (int i=0; i<*argc; ++i)
	{
		std::string argument = argv[i];
		cout << "Process argument " << argument << std::endl;
		if (argument.size() < prefix.size())
		{
			cout << "Skip argument " << argument << std::endl;
			continue;
		}
		if (argument.substr(0, prefix.size()) == prefix)
		{
			string kv = argument.substr(prefix.size());
			cout << "Handling kv: " << kv << endl;
			if (kv == "-help")
			{
				cout << elektraHelpText << endl;
				exit (0);
			}
			else if (kv == "-version")
			{
				printVersion();
				exit(0);
			}
			else if (kv[0] == ':')
			{
				kv = kv.substr(1); // skip :
				addProc(kv);
			}
		}
		printf ("argv[%d]: %s\n", i, argv[i]);
	}
}

void elektraCloseHelper()
{
	if (!elektraRepo) return; // already closed

	kdbClose(elektraRepo, elektraParentKey);
	ksDel(elektraConfig);
	keyDel(elektraParentKey);
	elektraRepo = 0;
}

extern "C" void elektraOpen(int* argc, char** argv)
{
	pthread_mutex_lock(&elektraGetEnvMutex);
	if (elektraRepo) elektraCloseHelper(); // already opened

	elektraParentKey = keyNew("user/sw/app/lift", KEY_END);
	elektraConfig = ksNew(20, KS_END);

	if (argc && argv)
	{
		parseArgs(argc, argv);
	}

	printf ("%s - %s\n", keyName(elektraParentKey), keyString(elektraParentKey));
	elektraRepo = kdbOpen(elektraParentKey);
	//TODO: install SIGHUP signal handler (on request)
	//TODO: parse arguments -> spec, remove "env"
	kdbGet(elektraRepo, elektraConfig, elektraParentKey);

	Key *c;
	ksRewind(elektraConfig);
	while ((c = ksNext(elektraConfig)))
	{}

	elektraParentKey = keyNew("user/sw/app/lift", KEY_END);
	printf ("%s - %s\n", keyName(elektraParentKey), keyString(elektraParentKey));
	elektraRepo = kdbOpen(elektraParentKey);
	//TODO: install SIGHUP signal handler (on request)
	//TODO: parse arguments -> spec, remove "env"
	kdbGet(elektraRepo, elektraConfig, elektraParentKey);
	pthread_mutex_unlock(&elektraGetEnvMutex);
}

extern "C" void elektraClose()
{
	pthread_mutex_lock(&elektraGetEnvMutex);
	elektraCloseHelper();
	pthread_mutex_unlock(&elektraGetEnvMutex);
}

extern "C" int __real_main(int argc, char** argv, char** env);

typedef int (*fcn)(int *(main) (int, char * *, char * *), int argc, char ** argv, void (*init) (void), void (*fini) (void), void (*rtld_fini) (void), void (* stack_end));
extern "C" int __libc_start_main(int *(main) (int, char * *, char * *), int argc, char ** argv, void (*init) (void), void (*fini) (void), void (*rtld_fini) (void), void (* stack_end))
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

typedef char *(* gfcn)(const char *);

/**
 * @brief Uses Elektra to get from environment.
 *
 * @param name to be looked up in the environment.
 *
 * @return the value found for that key
 * @see getenv
 * @see secure_getenv
 */
extern "C" char *elektraGetEnv(const char *name, gfcn getenv)
{
	std::string fullName = "proc/";
	fullName += name;
	Key *key = ksLookupByName(elektraConfig, fullName.c_str(), 0);
	if (key)
	{
		cout << "getenv found " << fullName << endl;
		return (char*)keyString(key);
	}

	fullName = "/sw/app/lift/";
	fullName += name;
	key = ksLookupByName(elektraConfig, fullName.c_str(), 0);
	if (!key)
	{
		cout << "getenv with " << fullName << ": " << keyString(key) << endl;
		return (char*)keyString(key);
	}
	cout << "getenv with " << fullName << " returned nothing" << endl;

	char *ret = (*getenv)(name);
	cout << "fallback to getenv returned " << ret << endl;
	return ret;
}

extern "C" char *getenv(const char *name) // throw ()
{
	pthread_mutex_lock(&elektraGetEnvMutex);
	static union {void*d; gfcn f;} sym;
	if (!sym.d) sym.d = dlsym(RTLD_NEXT, "getenv");

	char *ret = elektraGetEnv(name, sym.f);
	pthread_mutex_unlock(&elektraGetEnvMutex);
	return ret;
}

extern "C" char *secure_getenv(const char *name) // throw ()
{
	pthread_mutex_lock(&elektraGetEnvMutex);
	static union {void*d; gfcn f;} sym;
	if (!sym.d) sym.d = dlsym(RTLD_NEXT, "secure_getenv");

	char * ret = elektraGetEnv(name, sym.f);
	pthread_mutex_unlock(&elektraGetEnvMutex);
	return ret;
}

}
