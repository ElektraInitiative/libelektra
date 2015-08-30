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

#include <kdbcontext.hpp>

#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <libgen.h>

#include <string>
#include <sstream>
#include <iostream>

using namespace std;
using namespace ckdb;

namespace ckdb {

Key *elektraParentKey;
KeySet *elektraConfig;
KDB *elektraRepo;
bool elektraDebug;

namespace {

class KeyValueLayer : public kdb::Layer
{
public:
	KeyValueLayer(std::string const & key, std::string const & value_) :
		m_key(key), m_value(value_) {}
	std::string id() const { return m_key; }
	std::string operator()() const { return m_value; }
private:
	std::string m_key;
	std::string m_value;
};

}

class GetEnvContext : public kdb::Context
{
public:
	void addLayer(std::string layername, std::string layervalue)
	{
		std::shared_ptr<kdb::Layer> layer = make_shared<KeyValueLayer>(layername, layervalue);
		activateLayer(layer);
	}
} elektraEnvContext;

const char *appName = "/sw/app/current";
const char *elektraHelpText =
"This application is elektrified using libelektragetenv.\n"
"This is a LD_PRELOAD technique to elektrify applications\n"
"that use getenv().\n"
"\n"
"\n"
"Additional to searching in the environment (environ), getenv() will use\n"
"ksLookup() to lookup configuration.\n"
"\n"
"Different lookups will be done:\n"
" 1.) Commandline parameters will be preferred\n"
" 2.) an application-specific lookup using /sw/env/<name>\n"
"     where <name> is different for every application.\n"
" 3.) a convenience fallback lookup using /env\n"
"     which is the same for every application\n"
"     (easies usage of already established environment-standards. In applications with a specification this should not be used. Here we make an exception, because the administrator would have to write the specification)\n"
" 4.) the environment will be requested\n"
"\n"
"\n"
"OPTIONS\n"
"\n"
" --elektra-help             .. show this text\n"
" --elektra-name=key         .. the application name to be used instead of\n"
"                               the executable's basename\n"
" --elektra:key=value        .. set a key/value below root to be preferred\n"
"                               (in proc-namespace)\n"
"\n"
"Note that keys can contain / to form hierarchies.\n"
"Every option starting with --elektra will be discarded from argv\n"
"before the application's main function is started.\n"
"\n"
"\n"
"\n"
"KDB\n"
"\n"
"/sw/env/<name>/%profile%/<key>\n"
"/env/<key>\n"
"  will be used preferable. The spec(ification) entries are:\n"
"  - context .. do not use the key itself, but do a contextual lookup of the name, honoring layers , given in the metadata\n"
"  - no_elektra .. disable Elektra functionality for that key and only use environment\n"
"  - no_env .. disable environment fallback\n"
"\n"
"/sw/env/<name>/layers\n"
"  Specification of all layers to be activated\n"
"  Note that the profile layer??\n"
"\n"
"\n"
"\n"
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
"> kdb set user/sw/env/man/MANOPT -- --regex\n"
"\n"
"Will permanently and user-wide change MANOPT to include --regex, so that -K\n"
"and similar options automatically prefer regular expressions.\n"
"This feature is handy to change the default behaviour of\n"
"applications (either system, user or directory-wide).\n"
"\n"
"\n"
"> kdb set system/env/HTTP_PROXY http://proxy.hogege.com:8000/\n"
"\n"
"Will permanently and system-wide change the proxy for all applications\n"
"that honor HTTP_PROXY, e.g. w3m.\n"
"\n"
"\n";

pthread_mutex_t elektraGetEnvMutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;

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
	if (elektraDebug) cout << "kv is: " << kv << endl;
	stringstream ss(kv);
	string k, v;
	getline(ss, k, '=');
	getline(ss, v);
	if (elektraDebug) cout << "k is " << k << " and v is " << v << endl;

	string fullName = "proc/";
	fullName += k;
	ksAppendKey(elektraConfig, keyNew(fullName.c_str(), KEY_VALUE, v.c_str(), KEY_END));
}

void giveName(string name)
{
	std::string fullName = "/sw/env/";
	char * n = strdup(name.c_str());
	fullName += basename(n);
	free(n);
	if (elektraDebug) std::cout << "give name " << fullName << std::endl;
	keySetName(elektraParentKey, fullName.c_str());
}

void parseArgs(int* argc, char** argv)
{
	const string prefix = "--elektra";
	const string prefixName = "-name=";
	if (elektraDebug) std::cout << "Parsing args " << *argc << endl;

	giveName(argv[0]);

	int length = *argc;
	for (int i=1; i<length; ++i)
	{
		if (elektraDebug) printf ("argv[%d]: %s\n", i, argv[i]);

		std::string argument = argv[i];
		if (elektraDebug) cout << "Process argument " << argument << std::endl;
		if (argument.size() < prefix.size())
		{
			if (elektraDebug) cout << "Skip argument " << argument << std::endl;
		}
		else if (argument.substr(0, prefix.size()) == prefix)
		{
			string kv = argument.substr(prefix.size());
			if (elektraDebug) cout << "Handling kv: " << kv << endl;
			if (kv == "-help")
			{
				cout << elektraHelpText << endl;
				exit(0);
			}
			else if (kv.substr(0, prefixName.size()) == prefixName)
			{
				giveName(kv.substr(prefixName.size()));
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
			else continue;
			// we consume a parameter
			argv[i] = 0;
		}
	}
	char **oldEnd = &argv[length];
	char **newEnd = remove_if<char**>(argv, oldEnd, [](char *c) {return c==0;});
	*newEnd = 0;
	const size_t toSubtract = oldEnd-newEnd;
	*argc -= toSubtract;
}

extern "C" void elektraOpen(int* argc, char** argv)
{
	pthread_mutex_lock(&elektraGetEnvMutex);
	if (elektraRepo) elektraClose(); // already opened

	if (elektraDebug) cout << "opening elektra" << endl;
	// elektraEnvContext.addLayer("abc", "def");
	// std::cout << "evaluated: " << elektraEnvContext.evaluate("something/with/%abc%/more") << std::endl;

	elektraParentKey = keyNew("/env", KEY_END);
	elektraConfig = ksNew(20, KS_END);
	elektraRepo = kdbOpen(elektraParentKey);
	kdbGet(elektraRepo, elektraConfig, elektraParentKey);

	keySetName(elektraParentKey, "/sw/env/default");
	if (argc && argv)
	{
		parseArgs(argc, argv);
	}
	kdbGet(elektraRepo, elektraConfig, elektraParentKey);

	Key *c;
	while ((c = ksNext(elektraConfig)))
	{
		if (elektraDebug) printf ("%s - %s\n", keyName(c), keyString(c));
	}


	// reopen everything (if wrong variable names were used before)
	kdbClose(elektraRepo, elektraParentKey);
	elektraRepo = kdbOpen(elektraParentKey);
	std::string name = keyName(elektraParentKey);
	keySetName(elektraParentKey, "/env");
	kdbGet(elektraRepo, elektraConfig, elektraParentKey);
	keySetName(elektraParentKey, name.c_str());
	kdbGet(elektraRepo, elektraConfig, elektraParentKey);
	pthread_mutex_unlock(&elektraGetEnvMutex);
}

extern "C" void elektraClose()
{
	pthread_mutex_lock(&elektraGetEnvMutex);
	if (!elektraRepo) return; // already closed

	kdbClose(elektraRepo, elektraParentKey);
	ksDel(elektraConfig);
	keyDel(elektraParentKey);
	elektraRepo = 0;
	pthread_mutex_unlock(&elektraGetEnvMutex);
}

extern "C" int __real_main(int argc, char** argv, char** env);

typedef int (*fcn)(int *(main) (int, char * *, char * *), int argc, char ** argv, void (*init) (void), void (*fini) (void), void (*rtld_fini) (void), void (* stack_end));
extern "C" int __libc_start_main(int *(main) (int, char * *, char * *), int argc, char ** argv, void (*init) (void), void (*fini) (void), void (*rtld_fini) (void), void (* stack_end))
{
	static union {void*d; fcn f;} start;
	if (!start.d) start.d = dlsym(RTLD_NEXT, "__libc_start_main");

	if (elektraDebug) cout << "wrapping main" << endl;
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
extern "C" char *elektraGetEnv(const char *name, gfcn origGetenv)
{
	if (elektraDebug) cout << "elektraGetEnv(" << name << ")" ;
	if (!elektraRepo) // no open Repo (needed for bootstrapping, if inside kdbOpen() getenv is used)
	{
		char *ret = (*origGetenv)(name);
		if (elektraDebug) { if (!ret) cout << " orig getenv returned null pointer" << endl;
		else cout << " orig getenv returned ("<< strlen(ret) << ") <" << ret << ">" << endl; }
		return ret;
	}

	std::string fullName = "proc/";
	fullName += name;
	Key *key = ksLookupByName(elektraConfig, fullName.c_str(), 0);
	if (key)
	{
		if (elektraDebug) cout << " found " << fullName << endl;
		return (char*)keyString(key);
	}
	if (elektraDebug) cout << " tried " << fullName << " , " ;

	fullName = keyName(elektraParentKey);
	fullName += "/";
	fullName += name;
	key = ksLookupByName(elektraConfig, fullName.c_str(), 0);
	if (key)
	{
		if (elektraDebug) cout << " found " << fullName << ": " << keyString(key) << endl;
		return (char*)keyString(key);
	}
	if (elektraDebug) cout << " tried " << fullName << " , " ;

	fullName = "/env/";
	fullName += name;
	key = ksLookupByName(elektraConfig, fullName.c_str(), 0);
	if (key)
	{
		if (elektraDebug) cout << " found " << fullName << ": " << keyString(key) << endl;
		return (char*)keyString(key);
	}
	if (elektraDebug) cout << " tried " << fullName << " , " ;

	char *ret = (*origGetenv)(name);
	if (elektraDebug) { if (!ret) cout << " orig getenv returned null pointer" << endl;
	else cout << " orig getenv returned ("<< strlen(ret) << ") <" << ret << ">" << endl; }
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
