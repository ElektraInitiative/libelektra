/**
 * \file
 *
 * \brief Source for the getenv library
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef _GNU_SOURCE
# define _GNU_SOURCE // for RTLD_NEXT (except BSDI)
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

/* BSDI has this functionality, but its not defined */
#if !defined(RTLD_NEXT)
#define RTLD_NEXT ((void *) -1L)
#endif

using namespace std;
using namespace ckdb;

// #define LOG cerr
// #define LOG if(elektraDebug) cerr
#define LOG if(elektraDebug) elektraLog



namespace ckdb {
extern "C" {
Key *elektraParentKey;
KeySet *elektraConfig;
KDB *elektraRepo;
bool elektraDebug = true; // TODO
ofstream elektraLog("/tmp/elektra-getenv.log", fstream::app); // TODO
std::string elektraName;
std::string elektraProfile;
KeySet *elektraDocu = ksNew(20,
#include "readme_elektrify-getenv.c"
	KS_END);

typedef int (*fcn)(int *(main) (int, char * *, char * *), int argc, char ** argv, void (*init) (void), void (*fini) (void), void (*rtld_fini) (void), void (* stack_end));
typedef char *(* gfcn)(const char *);

union Start{void*d; fcn f;} start; // symbol for libc pre-main
union Sym{void*d; gfcn f;} sym, ssym; // symbols for libc (secure) getenv
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
	stringstream ss(kv);
	string k, v;
	getline(ss, k, '=');
	getline(ss, v);
	LOG << "add proc (cmdline option) " << k << " with " << v << endl;

	string fullName = "proc/";
	fullName += k;
	ksAppendKey(elektraConfig, keyNew(fullName.c_str(), KEY_VALUE, v.c_str(), KEY_END));
}

void giveName(string name)
{
	char * n = strdup(name.c_str());
	elektraName = basename(n);
	free(n);
	LOG << "give name " << elektraName << std::endl;

}

void appendEnv(const char *where, const char *env)
{
	char *v = (*sym.f)(env);
	if (v)
	{
		ksAppendKey(elektraConfig, keyNew(where, KEY_VALUE, v, KEY_END));
		LOG << "append " << v << " from " << env << " to " << where << std::endl;
	}
}


void parseArgs(int* argc, char** argv)
{
	const string prefix = "--elektra";
	const string prefixName = "-name=";
	const string prefixProfile = "-profile=";
	LOG << "Parsing args " << *argc << endl;

	giveName(argv[0]);

	int length = *argc;
	for (int i=1; i<length; ++i)
	{
		std::string argument = argv[i];
		LOG << "Process argument " << argument << std::endl;
		if (argument.size() < prefix.size())
		{
			LOG << "Skip argument " << argument << std::endl;
		}
		else if (argument.substr(0, prefix.size()) == prefix)
		{
			string kv = argument.substr(prefix.size());
			LOG << "Handling parameter: " << kv << endl;
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
			else if (kv.substr(0, prefixProfile.size()) == prefixProfile)
			{
				elektraProfile = kv.substr(prefixProfile.size());
			}
			else if (kv == "-clearenv")
			{
				ksAppendKey(elektraConfig, keyNew("proc/env/options/clearenv", KEY_END));
			}
			else if (kv == "-debug")
			{
				ksAppendKey(elektraConfig, keyNew("proc/env/options/debug", KEY_END));
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

void parseEnv()
{
	appendEnv("proc/env/options/debug", "ELEKTRA_DEBUG");
	appendEnv("proc/env/options/clearenv", "ELEKTRA_CLEARENV");
}

void addLayers()
{
	using namespace ckdb;
	elektraEnvContext.addLayer("name", elektraName);
	elektraEnvContext.addLayer("profile", elektraProfile);
	Key *c;
	ksRewind(elektraConfig);
	std::string prefix = "/env/layer/";
	while ((c = ksNext(elektraConfig)))
	{
		std::string fullName = keyName(c);
		if (fullName.substr(fullName.find('/'), prefix.size()) == prefix)
		{
			string name = fullName.substr(fullName.find_last_of('/')+1);
			string value = keyString(c);
			LOG << "Will add layer " << name << " with " << value << endl;
			elektraEnvContext.addLayer(name, value);
		}
	}
}

void applyOptions()
{
	Key *k = ksLookupByName(elektraConfig, "/env/options/debug", 0);
	if (k) elektraDebug = true;
	k = ksLookupByName(elektraConfig, "/env/options/clearenv", 0);
	if (k) clearenv();
}

extern "C" void elektraOpen(int* argc, char** argv)
{
	pthread_mutex_lock(&elektraGetEnvMutex);
	if (elektraRepo) elektraClose(); // already opened

	LOG << "opening elektra" << endl;

	elektraParentKey = keyNew("/env", KEY_END);
	elektraConfig = ksNew(20, KS_END);
	elektraRepo = kdbOpen(elektraParentKey);
	kdbGet(elektraRepo, elektraConfig, elektraParentKey);

	parseEnv();
	if (argc && argv)
	{
		parseArgs(argc, argv);
	}

	// reopen everything (if wrong variable names were used before)
	kdbClose(elektraRepo, elektraParentKey);
	elektraRepo = kdbOpen(elektraParentKey);
	std::string name = keyName(elektraParentKey);
	kdbGet(elektraRepo, elektraConfig, elektraParentKey);
	addLayers();
	applyOptions();
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

extern "C" int __libc_start_main(int *(main) (int, char * *, char * *), int argc, char ** argv, void (*init) (void), void (*fini) (void), void (*rtld_fini) (void), void (* stack_end))
{
	LOG << "wrapping main" << endl;
	start.d = dlsym(RTLD_NEXT, "__libc_start_main");
	sym.d = dlsym(RTLD_NEXT, "getenv");
	ssym.d = dlsym(RTLD_NEXT, "secure_getenv");

	elektraOpen(&argc, argv);
	int ret = (*start.f)(main, argc, argv, init, fini, rtld_fini, stack_end);
	//TODO: save configuration (on request)
	elektraClose();
	return ret;
}


Key *elektraContextEvaluation(ELEKTRA_UNUSED KeySet *ks, ELEKTRA_UNUSED Key *key, Key *found, option_t options)
{
	if (found && !strncmp(keyName(found), "spec/", 5) && options == KDB_O_CALLBACK)
	{
		const Key *meta = keyGetMeta(found, "context");
		if (meta)
		{
			string contextName = elektraEnvContext.evaluate(keyString(meta));
			LOG << ", in context: " << contextName;
			// only consider context if key actually exists, otherwise continue searching
			Key *ret = ksLookupByName(ks, contextName.c_str(), 0);
			if (ret) return ret; // use context override!
		}
		else
		{
			LOG << ", NO context";
		}
	}
	return found;
}

Key *elektraLookupWithContext(std::string name)
{
	Key *search = keyNew(name.c_str(), KEY_FUNC, elektraContextEvaluation, KEY_END);
	Key * ret = ksLookup(elektraConfig, search, 0);
	keyDel(search);
	return ret;
}

char *elektraGetEnvKey(std::string const& fullName, bool & finish)
{
	Key *key = elektraLookupWithContext(fullName);
	if (key)
	{
		LOG << " found " << fullName << ": " << keyString(key) << endl;
		finish = true;
		if (keyIsBinary(key)) return 0;
		return const_cast<char*>(keyString(key));
	}

	finish = false;
	LOG << " tried " << fullName << "," ;
	return 0;
}


/**
 * @brief Uses Elektra to get from environment.
 *
 * @param name to be looked up in the environment.
 *
 * @return the value found for that key
 * @see getenv
 * @see secure_getenv
 */
extern "C" char *elektraGetEnv(const char * cname, gfcn origGetenv)
{
	LOG << "elektraGetEnv(" << cname << ")" ;
	if (!elektraRepo)
	{	// no open Repo (needed for bootstrapping, if inside kdbOpen() getenv is used)
		char *ret = (*origGetenv)(cname);
		if (!ret) { LOG << " orig getenv returned null pointer" << endl; }
		else LOG << " orig getenv returned ("<< strlen(ret) << ") <" << ret << ">" << endl;
		return ret;
	}

	// TODO kdbGet() if needed

	std::string name = cname;
	std::string fullName = "proc/";
	fullName += name;
	Key *key = ksLookupByName(elektraConfig, fullName.c_str(), 0);
	if (key)
	{
		LOG << " found " << fullName << endl;
		return (char*)keyString(key);
	}
	LOG << " tried " << fullName << ",";

	bool finish = false;
	char * ret = 0;
	ret = elektraGetEnvKey("/env/override/"+name, finish);
	if (finish) return ret;

	ret = (*origGetenv)(name.c_str());
	if (ret)
	{
		LOG << " environ returned ("<< strlen(ret) << ") <" << ret << ">" << endl;
		return ret;
	} else LOG << " tried environ,";

	ret = elektraGetEnvKey("/env/fallback/"+name, finish);
	if (finish) return ret;

	LOG << " nothing found" << endl;
	return 0;
}

/*
// Nice trick to find next execution of elektraMalloc
// set foo to (int*)-1 to trigger it
int *foo = 0;
extern "C" void* elektraMalloc (size_t size)
{
	// LOG << "malloc " << size << endl;
	if (foo) printf("%d\n", *foo);
	return malloc (size);
}
*/

extern "C" char *getenv(const char *name) // throw ()
{
	pthread_mutex_lock(&elektraGetEnvMutex);

	if (!sym.f)
	{
		LOG << "jemalloc (firefox) bootstrap hack" << std::endl;
		return 0;
	}

	char *ret = elektraGetEnv(name, sym.f);
	pthread_mutex_unlock(&elektraGetEnvMutex);
	return ret;
}

extern "C" char *secure_getenv(const char *name) // throw ()
{
	pthread_mutex_lock(&elektraGetEnvMutex);

	if (!ssym.f)
	{
		LOG << "jemalloc (firefox) bootstrap hack" << std::endl;
		return 0;
	}

	char * ret = elektraGetEnv(name, ssym.f);
	pthread_mutex_unlock(&elektraGetEnvMutex);
	return ret;
}

}
