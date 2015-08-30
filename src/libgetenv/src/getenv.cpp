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
extern "C" {
Key *elektraParentKey;
KeySet *elektraConfig;
KDB *elektraRepo;
bool elektraDebug;
std::string elektraName;
}

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

KeySet *elektraDocu = ksNew(20,
#include "readme_elektrify-getenv.c"
	KS_END);

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
	char * n = strdup(name.c_str());
	elektraName = basename(n);
	free(n);
	if (elektraDebug) std::cout << "give name " << elektraName << std::endl;

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
				cout << keyString(ksLookupByName(elektraDocu,
					"system/elektra/modules/elektrify-getenv/infos/description",0)) << endl;
				exit(0);
			}
			else if (kv.substr(0, prefixName.size()) == prefixName)
			{
				elektraName = kv.substr(prefixName.size());
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

	if (argc && argv)
	{
		parseArgs(argc, argv);
	}

	Key *c;
	while ((c = ksNext(elektraConfig)))
	{
		if (elektraDebug) printf ("%s - %s\n", keyName(c), keyString(c));
	}


	// reopen everything (if wrong variable names were used before)
	kdbClose(elektraRepo, elektraParentKey);
	elektraRepo = kdbOpen(elektraParentKey);
	std::string name = keyName(elektraParentKey);
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
	elektraDebug = false;
	elektraName = "";
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

char *elektraGetEnvKey(std::string const& fullName, bool & finish)
{
	Key *key = ksLookupByName(elektraConfig, fullName.c_str(), 0);
	if (key)
	{
		if (elektraDebug) cout << " found " << fullName << ": " << keyString(key) << endl;
		finish = true;
		if (keyIsBinary(key)) return 0;
		return const_cast<char*>(keyString(key));
	}

	finish = false;
	if (elektraDebug) cout << " tried " << fullName << " , " ;
	return 0;
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
extern "C" char *elektraGetEnv(std::string const& name, gfcn origGetenv)
{
	if (elektraDebug) cout << "elektraGetEnv(" << name << ")" ;
	if (!elektraRepo)
	{	// no open Repo (needed for bootstrapping, if inside kdbOpen() getenv is used)
		char *ret = (*origGetenv)(name.c_str());
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

	bool finish = false;
	char * ret = 0;
	ret = elektraGetEnvKey("/env/override/"+name, finish);
	if (finish) return ret;

	ret = (*origGetenv)(name.c_str());
	if (ret)
	{
		if (elektraDebug) cout << " orig getenv returned ("<< strlen(ret) << ") <" << ret << ">" << endl;
		return ret;
	} else if (elektraDebug) cout << " orig getenv returned null pointer" << endl;

	ret = elektraGetEnvKey("/env/fallback/"+name, finish);
	if (finish) return ret;

	// found absolutely nothing..
	return 0;
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
