/**
 * \file
 *
 * \brief Source for the getenv library
 *
 * \note there are two necessary bootstrap phases:
 *
 * 1.) bootstrapping in pre-main phase when no allocation is possible
 * 2.) bootstrapping when elektra modules use getenv()
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
#include <unistd.h> // euid
#include <sys/types.h> // euid

#include <string>
#include <chrono>
#include <sstream>
#include <iostream>

/* BSDI has this functionality, but its not defined */
#if !defined(RTLD_NEXT)
#define RTLD_NEXT ((void *) -1L)
#endif

using namespace std;
using namespace ckdb;

#define LOG if(elektraLog) (*elektraLog)

namespace ckdb
{

extern "C"
{
Key *elektraParentKey;
KeySet *elektraConfig;
KDB *elektraRepo;
} // extern "C"

namespace
{

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

class GetEnvContext : public kdb::Context
{
public:
	void addLayer(std::string layername, std::string layervalue)
	{
		std::shared_ptr<kdb::Layer> layer = make_shared<KeyValueLayer>(layername, layervalue);
		activateLayer(layer);
	}
	void clearAllLayer()
	{
		kdb::Context::clearAllLayer();
	}
} elektraEnvContext;

typedef int (*fcn)(int *(main) (int, char * *, char * *), int argc, char ** argv, void (*init) (void), void (*fini) (void), void (*rtld_fini) (void), void (* stack_end));
typedef char *(* gfcn)(const char *);

union Start{void*d; fcn f;} start; // symbol for libc pre-main
union Sym{void*d; gfcn f;} sym, ssym; // symbols for libc (secure) getenv

std::chrono::milliseconds elektraReloadTimeout;
std::chrono::system_clock::time_point elektraReloadNext;
std::shared_ptr<ostream>elektraLog;
KeySet *elektraDocu = ksNew(20,
#include "readme_elektrify-getenv.c"
	KS_END);

pthread_mutex_t elektraGetEnvMutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
} // anonymous namespace



extern "C" void elektraLockMutex()
{
	pthread_mutex_lock(&elektraGetEnvMutex);
}

extern "C" void elektraUnlockMutex()
{
	pthread_mutex_unlock(&elektraGetEnvMutex);
}



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

void addOverride(string kv)
{
	stringstream ss(kv);
	string k, v;
	getline(ss, k, '=');
	getline(ss, v);
	LOG << "add override " << k << " with " << v << endl;

	string fullName = "proc/env/override/";
	fullName += k;
	ksAppendKey(elektraConfig, keyNew(fullName.c_str(), KEY_VALUE, v.c_str(), KEY_END));
}

void addOption(string kv)
{
	stringstream ss(kv);
	string k, v;
	getline(ss, k, '=');
	getline(ss, v);
	LOG << "add option " << k << " with " << v << endl;

	string fullName = "proc/env/option/";
	fullName += k;
	ksAppendKey(elektraConfig, keyNew(fullName.c_str(), KEY_VALUE, v.c_str(), KEY_END));
}

void addLayer(string kv)
{
	stringstream ss(kv);
	string k, v;
	getline(ss, k, '%');
	if (ss.get() != '=') return;
	getline(ss, v);
	LOG << "add layer " << k << " with " << v << endl;

	string fullName = "proc/env/layer/";
	fullName += k;
	ksAppendKey(elektraConfig, keyNew(fullName.c_str(), KEY_VALUE, v.c_str(), KEY_END));
}

void giveName(string name)
{
	char * n = strdup(name.c_str());
	std::string basename = ::basename(n);
	free(n);
	LOG << "give name " << name << ", basename: " << basename << std::endl;
	ksAppendKey(elektraConfig, keyNew("proc/env/layer/name", KEY_VALUE, name.c_str(), KEY_END));
	ksAppendKey(elektraConfig, keyNew("proc/env/layer/basename", KEY_VALUE, basename.c_str(), KEY_END));

}

void parseArgs(int* argc, char** argv)
{
	const string prefix = "--elektra";
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

			if (kv.empty()); // ignore but consume --elektra
			else if (kv[0] == '-')
			{
				addOption(kv.substr(1));
			}
			else if (kv[0] == ':')
			{
				addOverride(kv.substr(1));
			}
			else if (kv[0] == '%')
			{
				addLayer(kv.substr(1));
			}
			// ignore but consume all others

			// we consumed a parameter
			argv[i] = 0;
		}
	}
	char **oldEnd = &argv[length];
	char **newEnd = remove_if<char**>(argv, oldEnd, [](char *c) {return c==0;});
	*newEnd = 0;
	const size_t toSubtract = oldEnd-newEnd;
	*argc -= toSubtract;
}

void addEnvironment(string kv)
{
	std::transform(kv.begin(), kv.end(), kv.begin(), ::tolower);
	stringstream ss(kv);
	string k, v;
	getline(ss, k, '=');
	getline(ss, v);
	LOG << "add option " << k << " with " << v << endl;

	string fullName = "proc/env/option/";
	fullName += k;
	ksAppendKey(elektraConfig, keyNew(fullName.c_str(), KEY_VALUE, v.c_str(), KEY_END));
}

extern "C"
{
extern char **environ;
}

void parseEnvironment()
{
	const string prefix = "ELEKTRA_";
	char** env;
	for (env = environ; *env != 0; env++)
	{
		std::string argument = *env;
		if (argument.substr(0, prefix.size()) == prefix)
		{
			addEnvironment(argument.substr(prefix.size()));
		}
	}
}

void addLayers()
{
	using namespace ckdb;
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

void elektraSingleCleanup()
{
	// make everything really proper clean:
	ksDel(elektraDocu);
	elektraLog.reset();
}

void applyOptions()
{
	Key *k = 0;

	elektraLog.reset();
	if ((k = ksLookupByName(elektraConfig, "/env/option/debug", 0)))
	{
		if (keyGetValueSize(k) > 1)
		{
			elektraLog = make_shared<ofstream>(keyString(k), fstream::app);
		}
		else
		{
			elektraLog = shared_ptr<ostream>(&cerr, [](ostream*){});
		}
		LOG << "Elektra getenv starts logging to ";
		if (elektraLog.get() == &cerr)
		{
			LOG << "stderr";
		} else {
			LOG << keyString(k);
		}
		LOG << endl;
	}

	if ((k = ksLookupByName(elektraConfig, "/env/option/clearenv", 0)))
	{
		LOG << "clearing the environment" << endl;
		clearenv();
	}

	elektraReloadTimeout = std::chrono::milliseconds::zero();
	if ((k = ksLookupByName(elektraConfig, "/env/option/reload_timeout", 0)))
	{
		LOG << "activate reloading feature" << endl;

		// we do not care about errors, 0 is an invalid number anyway
		std::chrono::milliseconds::rep v = atoi(keyString(k));
		elektraReloadTimeout = std::chrono::milliseconds(v);
	}

	if ((k = ksLookupByName(elektraConfig, "/env/option/help", 0)))
	{
		cout << keyString(ksLookupByName(elektraDocu,
			"system/elektra/modules/elektrify-getenv/infos/description",0)) << endl;
		exit(0);
	}

	if ((k = ksLookupByName(elektraConfig, "/env/option/version", 0)))
	{
		printVersion();
		exit(0);
	}
}

extern "C" void elektraOpen(int* argc, char** argv)
{
	elektraLockMutex();
	if (elektraRepo) elektraClose(); // already opened

	LOG << "opening elektra" << endl;

	elektraParentKey = keyNew("/env", KEY_END);
	elektraConfig = ksNew(20, KS_END);
	elektraRepo = kdbOpen(elektraParentKey);
	kdbGet(elektraRepo, elektraConfig, elektraParentKey);

	parseEnvironment();
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
	elektraUnlockMutex();
}

extern "C" void elektraClose()
{
	elektraLockMutex();
	if (!elektraRepo) return; // already closed

	kdbClose(elektraRepo, elektraParentKey);
	ksDel(elektraConfig);
	keyDel(elektraParentKey);
	elektraRepo = 0;
	elektraUnlockMutex();
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
	elektraClose();
	return ret;
}


Key *elektraContextEvaluation(ELEKTRA_UNUSED KeySet *ks, ELEKTRA_UNUSED Key *key, Key *found, option_t option)
{
	if (found && !strncmp(keyName(found), "spec/", 5) && option == KDB_O_CALLBACK)
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
char *elektraGetEnv(const char * cname, gfcn origGetenv)
{
	LOG << "elektraGetEnv(" << cname << ")" ;
	if (!elektraRepo)
	{	// no open Repo (needed for bootstrapping, if inside kdbOpen() getenv is used)
		char *ret = (*origGetenv)(cname);
		if (!ret) { LOG << " orig getenv returned null pointer" << endl; }
		else LOG << " orig getenv returned ("<< strlen(ret) << ") <" << ret << ">" << endl;
		return ret;
	}

	// is reload feature enabled at all?
	if (elektraReloadTimeout > std::chrono::milliseconds::zero())
	{
		std::chrono::system_clock::time_point const now =
			std::chrono::system_clock::now();

		// are we now ready to reload?
		if (now >= elektraReloadNext)
		{
			int ret = kdbGet(elektraRepo, elektraConfig, elektraParentKey);

			// was there a change?
			if (ret == 1)
			{
				elektraEnvContext.clearAllLayer();
				addLayers();
				applyOptions();
			}
		}

		elektraReloadNext = now + elektraReloadTimeout;
	}

	std::string name = cname;
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

/**
 * @brief Search in environ, should be identical to getenv
 *
 * implementation is needed for bootstrapping in pre-main phases
 * where memory allocation hangs or crashes and thus dlsym cannot be used!
 *
 * @see getenv()
 */
char *elektraBootstrapGetEnv(const char *name)
{
	int len = strlen(name);
	if (environ == NULL || len == 0)
	{
		return 0;
	}

	char** env;
	for (env = environ; *env != 0; env++)
	{
		if (!strncmp(*env, name, len))
		{
			if ((*env)[len] == '=')
			{
				return &((*env)[len+1]);
			}
		}
	}

	return 0;
}

char *elektraBootstrapSecureGetEnv(const char *name)
{
	return (geteuid() != getuid() || getegid() != getgid()) ? NULL : elektraBootstrapGetEnv(name);
}

extern "C" char *getenv(const char *name) // throw ()
{
	if (!sym.f)
	{
		return elektraBootstrapGetEnv(name);
	}

	elektraLockMutex();
	char *ret = elektraGetEnv(name, sym.f);
	elektraUnlockMutex();
	return ret;
}

extern "C" char *secure_getenv(const char *name) // throw ()
{
	if (!ssym.f)
	{
		return elektraBootstrapSecureGetEnv(name);
	}

	elektraLockMutex();
	char * ret = elektraGetEnv(name, ssym.f);
	elektraUnlockMutex();
	return ret;
}

}
