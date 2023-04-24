/**
 * @file
 *
 * @brief Source for the getenv library
 *
 * \note there are two necessary bootstrap phases:
 *
 * 1.) bootstrapping in pre-main phase when no allocation is possible
 * 2.) bootstrapping when elektra modules use getenv()
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE // for RTLD_NEXT (except BSDI)
#endif

#include <internal/kdb/config.h>
#include <kdbgetenv.h>

#include <kdbcontext.hpp>

#include <internal/utility/old_helper.h>

#include <dlfcn.h>
#include <libgen.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h> // euid
#include <unistd.h>    // euid

#ifdef __powerpc__
#include <elf.h>
#include <link.h>
#include <sys/auxv.h>
#endif

#include <chrono>
#include <iostream>
#include <sstream>
#include <string>

/* BSDI has this functionality, but its not defined */
#if !defined(RTLD_NEXT)
#define RTLD_NEXT ((void *) -1L)
#endif

using namespace std;
using namespace ckdb;

#define LOG                                                                                                                                \
	if (elektraLog) (*elektraLog)

#define ELEKTRA_GETENV_USE_LOCKS 1

#if ELEKTRA_GETENV_USE_LOCKS
#if defined(__APPLE__) && defined(__MACH__)
#define ELEKTRA_MUTEX_INIT PTHREAD_RECURSIVE_MUTEX_INITIALIZER
#else
#define ELEKTRA_MUTEX_INIT PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
#endif
#endif

namespace ckdb
{

extern "C" {
Key * elektraParentKey;
KeySet * elektraConfig;
KDB * elektraRepo;

} // extern "C"

namespace
{

class KeyValueLayer : public kdb::Layer
{
public:
	KeyValueLayer (std::string key, std::string value_) : m_key (std::move (key)), m_value (std::move (value_))
	{
	}
	std::string id () const override
	{
		return m_key;
	}
	std::string operator() () const override
	{
		return m_value;
	}

private:
	std::string m_key;
	std::string m_value;
};

class GetEnvContext : public kdb::Context
{
public:
	void addLayer (std::string layername, std::string layervalue)
	{
		std::shared_ptr<kdb::Layer> layer = make_shared<KeyValueLayer> (layername, layervalue);
		activateLayer (layer);
	}
	void clearAllLayer ()
	{
		kdb::Context::clearAllLayer ();
	}
} elektraEnvContext;

#ifdef __powerpc__
typedef int (*fcn) (int argc, char ** argv, char ** ev, ElfW (auxv_t) * auxvec, void (*rtld_fini) (void), void * stinfo,
		    char ** stack_on_entry);
#else
typedef int (*fcn) (int *(main) (int, char **, char **), int argc, char ** argv, void (*init) (void), void (*fini) (void),
		    void (*rtld_fini) (void), void (*stack_end));
#endif
typedef char * (*gfcn) (const char *);

union Start
{
	void * d;
	fcn f;
} start; // symbol for libc pre-main
union Sym
{
	void * d;
	gfcn f;
} sym, ssym; // symbols for libc (secure) getenv

typedef pid_t (*ffcn) (void);
union Fork
{
	void * d;
	ffcn f;
} ffork; // symbols for libc fork

std::chrono::milliseconds elektraReloadTimeout;
std::chrono::system_clock::time_point elektraReloadNext;
std::shared_ptr<ostream> elektraLog;
bool elektraInGetEnv;
KeySet * elektraDocu = ksNew (20,
#include "./readme_elektrify-getenv.c"
			      KS_END);

int to_ (int c)
{
	if (c == '-') return '_';
	return c;
}

pthread_mutex_t elektraGetEnvMutex = ELEKTRA_MUTEX_INIT;


} // anonymous namespace


extern "C" void elektraLockMutex ()
{
#if ELEKTRA_GETENV_USE_LOCKS
	pthread_mutex_lock (&elektraGetEnvMutex);
#endif
}

extern "C" void elektraUnlockMutex ()
{
#if ELEKTRA_GETENV_USE_LOCKS
	pthread_mutex_unlock (&elektraGetEnvMutex);
#endif
}


void printVersion ()
{
	cout << "Elektra getenv is active" << std::endl;
	Key * k = keyNew ("system:/elektra/version", KEY_END);
	KDB * kdb = kdbOpen (NULL, k);
	KeySet * c = ksNew (20, KS_END);
	kdbGet (kdb, c, k);
	kdbClose (kdb, k);
	keyDel (k);
	Key * kdb_version = ksLookupByName (c, "system:/elektra/version/constants/KDB_VERSION", 0);
	if (!kdb_version)
	{
		cerr << "Could not lookup KDB_VERSION key" << endl;
	}
	else
	{
		cout << "KDB_VERSION: " << keyString (kdb_version) << endl;
	}
	cout << "KDB_GETENV_VERSION: " << KDB_GETENV_VERSION << endl;
	ksDel (c);
}

void addOverride (string kv)
{
	stringstream ss (kv);
	string k, v;
	getline (ss, k, '=');
	getline (ss, v);
	LOG << "add override " << k << " with " << v << endl;

	string fullName = "proc:/elektra/intercept/getenv/override/";
	fullName += k;
	ksAppendKey (elektraConfig, keyNew (fullName.c_str (), KEY_VALUE, v.c_str (), KEY_END));
}

void addOption (string kv)
{
	std::transform (kv.begin (), kv.end (), kv.begin (), to_);
	stringstream ss (kv);
	string k, v;
	getline (ss, k, '=');
	getline (ss, v);
	LOG << "add option " << k << " with " << v << endl;

	string fullName = "proc:/elektra/intercept/getenv/option/";
	fullName += k;
	ksAppendKey (elektraConfig, keyNew (fullName.c_str (), KEY_VALUE, v.c_str (), KEY_END));
}

void addLayer (string kv)
{
	stringstream ss (kv);
	string k, v;
	getline (ss, k, '%');
	if (ss.get () != '=') return;
	getline (ss, v);
	LOG << "add layer " << k << " with " << v << endl;

	string fullName = "proc:/elektra/intercept/getenv/layer/";
	fullName += k;
	ksAppendKey (elektraConfig, keyNew (fullName.c_str (), KEY_VALUE, v.c_str (), KEY_END));
}

void giveName (string name)
{
	char * n = elektraStrDup (name.c_str ());
	std::string basename = ::basename (n);
	elektraFree (n);
	LOG << "give name " << name << ", basename: " << basename << std::endl;
	ksAppendKey (elektraConfig, keyNew ("proc:/elektra/intercept/getenv/layer/name", KEY_VALUE, name.c_str (), KEY_END));
	ksAppendKey (elektraConfig, keyNew ("proc:/elektra/intercept/getenv/layer/basename", KEY_VALUE, basename.c_str (), KEY_END));
}

void parseArgs (int * argc, char ** argv)
{
	const string prefix = "--elektra";
	LOG << "Parsing args " << *argc << endl;

	giveName (argv[0]);

	int length = *argc;
	for (int i = 1; i < length; ++i)
	{
		std::string argument = argv[i];
		LOG << "Process argument " << argument << std::endl;
		if (argument.size () < prefix.size ())
		{
			LOG << "Skip argument " << argument << std::endl;
		}
		else if (argument.substr (0, prefix.size ()) == prefix)
		{
			string kv = argument.substr (prefix.size ());
			LOG << "Handling parameter: " << kv << endl;

			if (kv.empty ())
				; // ignore but consume --elektra
			else if (kv[0] == '-')
			{
				addOption (kv.substr (1));
			}
			else if (kv[0] == ':')
			{
				addOverride (kv.substr (1));
			}
			else if (kv[0] == '%')
			{
				addLayer (kv.substr (1));
			}
			// ignore but consume all others

			// we consumed a parameter
			argv[i] = nullptr;
		}
	}
	char ** oldEnd = &argv[length];
	char ** newEnd = remove_if<char **> (argv, oldEnd, [] (char * c) { return c == nullptr; });
	*newEnd = nullptr;
	const size_t toSubtract = oldEnd - newEnd;
	*argc -= toSubtract;
}

void addEnvironment (string kv)
{
	std::transform (kv.begin (), kv.end (), kv.begin (), ::tolower);
	stringstream ss (kv);
	string k, v;
	getline (ss, k, '=');
	getline (ss, v);
	LOG << "add option " << k << " with " << v << endl;

	string fullName = "proc:/elektra/intercept/getenv/option/";
	fullName += k;
	ksAppendKey (elektraConfig, keyNew (fullName.c_str (), KEY_VALUE, v.c_str (), KEY_END));
}

extern "C" {
extern char ** environ;
}

void parseEnvironment ()
{
	const string prefix = "ELEKTRA_";
	char ** env;
	for (env = environ; *env != nullptr; env++)
	{
		std::string argument = *env;
		if (argument.substr (0, prefix.size ()) == prefix)
		{
			addEnvironment (argument.substr (prefix.size ()));
		}
	}
}

void addLayersHelper (KeySet * lookupConfig, string prefix)
{
	for (elektraCursor it = 0; it < ksGetSize (elektraConfig); ++it)
	{
		const Key * c = ksAtCursor (elektraConfig, it);
		std::string fullName = keyName (c);
		size_t pos = fullName.find ('/');
		if (pos != string::npos && fullName.substr (pos, prefix.size ()) == prefix)
		{
			std::string cascadingName = fullName.substr (fullName.find ('/'));
			Key * found = ksLookupByName (lookupConfig, cascadingName.c_str (), 0);
			string name = fullName.substr (fullName.find ('/') + prefix.size ());
			string value = keyString (found);
			LOG << "Will add layer " << name << " with " << value << endl;
			elektraEnvContext.addLayer (name, value);
		}
	}
}

void addLayers ()
{
	using namespace ckdb;
	KeySet * lookupConfig = ksDup (elektraConfig);

	addLayersHelper (lookupConfig, "/elektra/intercept/getenv/layer/");

	ksDel (lookupConfig);
}

void elektraSingleCleanup ()
{
	// make everything really proper clean:
	ksDel (elektraDocu);
	elektraLog.reset ();
}


void applyOptions ()
{
	Key * k = nullptr;

	elektraLog.reset ();
	if (((k = ksLookupByName (elektraConfig, "/elektra/intercept/getenv/option/debug", 0))) && !keyIsBinary (k))
	{
		if (keyGetValueSize (k) > 1)
		{
			elektraLog = make_shared<ofstream> (keyString (k), fstream::app);
		}
		else
		{
			elektraLog = shared_ptr<ostream> (&cerr, [] (ostream *) {});
		}
		LOG << "Elektra getenv starts logging to ";
		if (elektraLog.get () == &cerr)
		{
			LOG << "stderr";
		}
		else
		{
			LOG << keyString (k);
		}
		LOG << endl;
	}

	if (((k = ksLookupByName (elektraConfig, "/elektra/intercept/getenv/option/clearenv", 0))) && !keyIsBinary (k))
	{
		LOG << "clearing the environment" << endl;
#ifdef HAVE_CLEARENV
		clearenv ();
#else
#warning Your system does not provide clearenv, this might be a security problem
#endif
		environ = nullptr;
	}

	elektraReloadTimeout = std::chrono::milliseconds::zero ();
	if (((k = ksLookupByName (elektraConfig, "/elektra/intercept/getenv/option/reload_timeout", 0))) && !keyIsBinary (k))
	{
		LOG << "activate reloading feature" << endl;

		// we do not care about errors, 0 is an invalid number anyway
		std::chrono::milliseconds::rep v = atoi (keyString (k));
		elektraReloadTimeout = std::chrono::milliseconds (v);
	}

	if (((k = ksLookupByName (elektraConfig, "/elektra/intercept/getenv/option/help", 0))) && !keyIsBinary (k))
	{
		cout << keyString (ksLookupByName (elektraDocu, "system:/elektra/modules/elektrify-getenv/infos/description", 0)) << endl;
		exit (0);
	}

	if (((k = ksLookupByName (elektraConfig, "/elektra/intercept/getenv/option/version", 0))) && !keyIsBinary (k))
	{
		printVersion ();
		exit (0);
	}
}

extern "C" void elektraOpen (int * argc, char ** argv)
{
	elektraLockMutex ();
	if (elektraRepo) elektraClose (); // already opened

	LOG << "opening elektra" << endl;

	elektraParentKey = keyNew ("/elektra/intercept/getenv", KEY_END);
	elektraConfig = ksNew (20, KS_END);
	elektraRepo = kdbOpen (NULL, elektraParentKey);
	kdbGet (elektraRepo, elektraConfig, elektraParentKey);

	parseEnvironment ();
	if (argc && argv)
	{
		parseArgs (argc, argv);
	}

	// reopen everything (if wrong variable names were used before)
	kdbClose (elektraRepo, elektraParentKey);
	elektraRepo = kdbOpen (NULL, elektraParentKey);
	std::string name = keyName (elektraParentKey);
	kdbGet (elektraRepo, elektraConfig, elektraParentKey);
	addLayers ();
	applyOptions ();
	elektraUnlockMutex ();
}

extern "C" void elektraClose ()
{
	elektraLockMutex ();
	if (elektraRepo)
	{
		kdbClose (elektraRepo, elektraParentKey);
		ksDel (elektraConfig);
		keyDel (elektraParentKey);
		elektraRepo = nullptr;
	}
	elektraUnlockMutex ();
}

extern "C" int __real_main (int argc, char ** argv, char ** env);

#ifdef __powerpc__
extern "C" int __libc_start_main (int argc, char ** argv, char ** ev, ElfW (auxv_t) * auxvec, void (*rtld_fini) (void), void * stinfo,
				  char ** stack_on_entry)
#else
extern "C" int __libc_start_main (int *(main) (int, char **, char **), int argc, char ** argv, void (*init) (void), void (*fini) (void),
				  void (*rtld_fini) (void), void (*stack_end))
#endif
{
	elektraLockMutex (); // dlsym mutex
	LOG << "wrapping main" << endl;
	if (start.d)
	{ // double wrapping situation, do not reopen, just forward to next __libc_start_main
		start.d = dlsym (RTLD_NEXT, "__libc_start_main");
		elektraUnlockMutex (); // dlsym mutex end
#ifdef __powerpc__
		int ret = (*start.f) (argc, argv, ev, auxvec, rtld_fini, stinfo, stack_on_entry);
#else
		int ret = (*start.f) (main, argc, argv, init, fini, rtld_fini, stack_end);
#endif
		return ret;
	}

	start.d = dlsym (RTLD_NEXT, "__libc_start_main");
	sym.d = dlsym (RTLD_NEXT, "getenv");
	ssym.d = dlsym (RTLD_NEXT, "secure_getenv");
	ffork.d = dlsym (RTLD_NEXT, "fork");

	elektraOpen (&argc, argv);
	elektraUnlockMutex (); // dlsym mutex end
#ifdef __powerpc__
	int ret = (*start.f) (argc, argv, ev, auxvec, rtld_fini, stinfo, stack_on_entry);
#else
	int ret = (*start.f) (main, argc, argv, init, fini, rtld_fini, stack_end);
#endif
	elektraClose ();
	return ret;
}

extern "C" pid_t fork ()
{
	pid_t ret = ffork.f ();
	if (ret == 0)
	{
		// reinitialize mutex in new process
		// fixes deadlock in akonadictl
		elektraGetEnvMutex = ELEKTRA_MUTEX_INIT;
	}
	return ret;
}

Key * elektraContextEvaluation (ELEKTRA_UNUSED KeySet * ks, ELEKTRA_UNUSED Key * key, Key * found, elektraLookupFlags option)
{
	if (found && !strncmp (keyName (found), "spec:/", 5) && option == KDB_O_CALLBACK)
	{
		const Key * meta = keyGetMeta (found, "context");
		if (meta)
		{
			string contextName = elektraEnvContext.evaluate (keyString (meta));
			LOG << ", in context: " << contextName;
			// only consider context if key actually exists, otherwise continue searching
			Key * ret = ksLookupByName (ks, contextName.c_str (), 0);
			if (ret) return ret; // use context override!
		}
		else
		{
			LOG << ", NO context";
		}
	}
	return found;
}

Key * elektraLookupWithContext (std::string name)
{
	Key * search = keyNew (name.c_str (), KEY_META, "callback", "", KEY_FUNC, elektraContextEvaluation, KEY_END);
	Key * ret = ksLookup (elektraConfig, search, 0);
	keyDel (search);
	return ret;
}

char * elektraGetEnvKey (std::string const & fullName, bool & finish)
{
	Key * key = elektraLookupWithContext (fullName);
	if (key)
	{
		LOG << " found " << fullName << ": " << keyString (key) << endl;
		finish = true;
		if (keyIsBinary (key)) return nullptr;
		return const_cast<char *> (keyString (key));
	}

	finish = false;
	LOG << " tried " << fullName << ",";
	return nullptr;
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
char * elektraGetEnv (const char * cname, gfcn origGetenv)
{
	LOG << "elektraGetEnv(" << cname << ")";
	if (!elektraRepo)
	{ // no open Repo (needed for bootstrapping, if inside kdbOpen() getenv is used)
		char * ret = (*origGetenv) (cname);
		if (!ret)
		{
			LOG << " orig getenv returned null pointer" << endl;
		}
		else
			LOG << " orig getenv returned (" << strlen (ret) << ") <" << ret << ">" << endl;
		return ret;
	}

	// is reload feature enabled at all?
	if (elektraReloadTimeout > std::chrono::milliseconds::zero ())
	{
		std::chrono::system_clock::time_point const now = std::chrono::system_clock::now ();

		// are we now ready to reload?
		if (now >= elektraReloadNext)
		{
			int ret = kdbGet (elektraRepo, elektraConfig, elektraParentKey);

			// was there a change?
			if (ret == 1)
			{
				elektraEnvContext.clearAllLayer ();
				addLayers ();
				applyOptions ();
			}
		}

		elektraReloadNext = now + elektraReloadTimeout;
	}

	std::string name = cname;
	bool finish = false;
	char * ret = nullptr;
	ret = elektraGetEnvKey ("/elektra/intercept/getenv/override/" + name, finish);
	if (finish) return ret;

	ret = (*origGetenv) (name.c_str ());
	if (ret)
	{
		LOG << " environ returned (" << strlen (ret) << ") <" << ret << ">" << endl;
		return ret;
	}
	else
		LOG << " tried environ,";

	ret = elektraGetEnvKey ("/elektra/intercept/getenv/fallback/" + name, finish);
	if (finish) return ret;

	LOG << " nothing found" << endl;
	return nullptr;
}

/*
// Nice trick to find next execution of elektraMalloc
// set foo to (int*)-1 to trigger it
int *foo = 0;
extern "C" void* elektraMalloc (size_t size)
{
	// LOG << "elektraMalloc " << size << endl;
	if (foo) printf("%d\n", *foo);
	return malloc(size);
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
char * elektraBootstrapGetEnv (const char * name)
{
	int len = strlen (name);
	if (environ == nullptr || len == 0)
	{
		return nullptr;
	}

	char ** env;
	for (env = environ; *env != nullptr; env++)
	{
		if (!strncmp (*env, name, len))
		{
			if ((*env)[len] == '=')
			{
				return &((*env)[len + 1]);
			}
		}
	}

	return nullptr;
}

char * elektraBootstrapSecureGetEnv (const char * name)
{
	return (geteuid () != getuid () || getegid () != getgid ()) ? nullptr : elektraBootstrapGetEnv (name);
}

extern "C" char * getenv (const char * name) // throw ()
{
	elektraLockMutex ();
	if (!sym.f || elektraInGetEnv)
	{
		char * ret = elektraBootstrapGetEnv (name);
		elektraUnlockMutex ();
		return ret;
	}

	elektraInGetEnv = true;
	char * ret = elektraGetEnv (name, sym.f);
	elektraInGetEnv = false;
	elektraUnlockMutex ();
	return ret;
}

extern "C" char * secure_getenv (const char * name) // throw ()
{
	elektraLockMutex ();
	if (!ssym.f || elektraInGetEnv)
	{
		char * ret = elektraBootstrapSecureGetEnv (name);
		elektraUnlockMutex ();
		return ret;
	}

	elektraInGetEnv = true;
	char * ret = elektraGetEnv (name, ssym.f);
	elektraInGetEnv = false;
	elektraUnlockMutex ();
	return ret;
}
} // namespace ckdb
