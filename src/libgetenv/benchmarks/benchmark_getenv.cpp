/**
* @file
*
* @brief benchmark for getenv
*
* @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/

#include <keyset.hpp>
#include <kdbtimer.hpp>

#include <fstream>
#include <iostream>

#include <dlfcn.h>
#include <string.h>
extern "C" char **environ;


const long long nr_keys = 30;

// long long iterations = 100000000000LL; // elitebenchmark lookup
long long iterations = 1000000LL; // elitebenchmark
// long long iterations = 100LL; // valgrind

// not needed in benchmarks:
long long iterations1 = iterations / 100;

const int benchmarkIterations = 11; // is a good number to not need mean values for median

const std::string filename = "check.txt";

const std::string csvfilename = "data.csv";

std::ofstream dump(filename);
std::ofstream data(csvfilename);


// from libgetenv
namespace ckdb{
char *elektraBootstrapGetEnv(const char *name);
}


// very fast benchmarks without any if:

__attribute__((noinline)) void benchmark_nothing()
{
	static Timer t("nothing");

	t.start();
	t.stop();
	std::cout << t;
	dump << t.name << std::endl;
}

#define _STRING_ARCH_unaligned 1

/* Return the value of the environment variable NAME.  This implementation
   is tuned a bit in that it assumes no environment variable has an empty
   name which of course should always be true.  We have a special case for
   one character names so that for the general case we can assume at least
   two characters which we can access.  By doing this we can avoid using the
   `strncmp' most of the time.  */
__attribute__((noinline)) char *
libc_getenv (const char *name)
{
  size_t len = strlen (name);
  char **ep;
  uint16_t name_start;

  if (environ == NULL || name[0] == '\0')
    return NULL;

  if (name[1] == '\0')
    {
      /* The name of the variable consists of only one character.  Therefore
	 the first two characters of the environment entry are this character
	 and a '=' character.  */
#if __BYTE_ORDER == __LITTLE_ENDIAN || !_STRING_ARCH_unaligned
      name_start = ('=' << 8) | *(const unsigned char *) name;
#else
# if __BYTE_ORDER == __BIG_ENDIAN
      name_start = '=' | ((*(const unsigned char *) name) << 8);
# else
 #error "Funny byte order."
# endif
#endif
      for (ep = environ; *ep != NULL; ++ep)
	{
#if _STRING_ARCH_unaligned
	  uint16_t ep_start = *(uint16_t *) *ep;
#else
	  uint16_t ep_start = (((unsigned char *) *ep)[0]
			       | (((unsigned char *) *ep)[1] << 8));
#endif
	  if (name_start == ep_start)
	    return &(*ep)[2];
	}
    }
  else
    {
#if _STRING_ARCH_unaligned
      name_start = *(const uint16_t *) name;
#else
      name_start = (((const unsigned char *) name)[0]
		    | (((const unsigned char *) name)[1] << 8));
#endif
      len -= 2;
      name += 2;

      for (ep = environ; *ep != NULL; ++ep)
	{
#if _STRING_ARCH_unaligned
	  uint16_t ep_start = *(uint16_t *) *ep;
#else
	  uint16_t ep_start = (((unsigned char *) *ep)[0]
			       | (((unsigned char *) *ep)[1] << 8));
#endif

	  if (name_start == ep_start && !strncmp (*ep + 2, name, len)
	      && (*ep)[len + 2] == '=')
	    return &(*ep)[len + 3];
	}
    }

  return NULL;
}

__attribute__((noinline)) void benchmark_libc_getenv()
{
	static Timer t("libc getenv");

	t.start();
	for (long long i=0; i<iterations; ++i)
	{
		libc_getenv("HELLO");
		__asm__("");
	}
	t.stop();
	std::cout << t;
	dump << t.name << std::endl;
}

__attribute__((noinline)) void benchmark_getenv()
{
	static Timer t("elektra getenv");

	t.start();
	for (long long i=0; i<iterations; ++i)
	{
		getenv("HELLO");
		__asm__("");
	}
	t.stop();
	std::cout << t;
	dump << t.name << std::endl;
}

__attribute__((noinline)) void benchmark_dl_next_getenv()
{
	static Timer t("dl next getenv");
	typedef char *(* gfcn)(const char *);
	union Sym{void*d; gfcn f;} sym;
	sym.d = dlsym(RTLD_NEXT, "getenv");
	gfcn dl_libc_getenv = sym.f;

	t.start();
	for (long long i=0; i<iterations; ++i)
	{
		dl_libc_getenv("HELLO");
		__asm__("");
	}
	t.stop();
	std::cout << t;
	dump << t.name << std::endl;
}


__attribute__((noinline)) void benchmark_dl_libc_getenv()
{
	static Timer t("dl libc getenv");
	typedef char *(* gfcn)(const char *);
	union Sym{void*d; gfcn f;} sym;

	void * handle = dlopen("/lib/x86_64-linux-gnu/libc-2.19.so", RTLD_NOW);
	sym.d = dlsym(handle, "getenv");
	gfcn dl_libc_getenv = sym.f;

	t.start();
	for (long long i=0; i<iterations; ++i)
	{
		dl_libc_getenv("HELLO");
		__asm__("");
	}
	t.stop();
	std::cout << t;
	dump << t.name << std::endl;
}


__attribute__((noinline)) void benchmark_bootstrap_getenv()
{
	static Timer t("bootstrap getenv");
	t.start();
	for (long long i=0; i<iterations; ++i)
	{
		ckdb::elektraBootstrapGetEnv("HELLO");
		__asm__("");
	}
	t.stop();
	std::cout << t;
	dump << t.name << std::endl;
}

__attribute__((noinline)) void benchmark_kslookup()
{
	static Timer t("kslookup");
	using namespace kdb; // needed for KS_END
	kdb::KeySet ks( 100,
			/*
			*kdb::Key("user/env/override/some", KEY_END),
			*kdb::Key("user/env/override/a/key", KEY_END),
			*kdb::Key("user/env/override/b/key", KEY_END),
			*kdb::Key("user/env/override/c/key", KEY_END),
			*kdb::Key("user/env/override/d/key", KEY_END),
			*/
			KS_END);
	for (int i=0; i<nr_keys; ++i)
	{
		char x[100];
		sprintf(x, "user/env/override/hello%d_%d", i, i);
		ks.append(*kdb::Key(x, KEY_END));
	}
	Key lookupKey ("user/env/override/HELLO", KEY_END);
	t.start();
	for (long long i=0; i<iterations; ++i)
	{
		ks.lookup(lookupKey);
		__asm__("");
	}
	t.stop();
	std::cout << t;
}

#include <unistd.h>
#ifdef _WIN32
# include <winsock2.h>
#endif

void computer_info()
{
	char hostname[1024];
	gethostname(hostname, 1023);
	std::cout << std::endl;
	std::cout << std::endl;
	std::cout << "hostname " << hostname << std::endl;
#ifdef __GNUC__
	std::cout << "gcc: " << __GNUC__  << std::endl;
#endif
#ifdef __INTEL_COMPILER
	std::cout << "icc: " << __INTEL_COMPILER  << std::endl;
#endif
#ifdef __clang__
	std::cout << "clang: " << __clang__ << std::endl;
#endif
	std::cout << "sizeof(int) " << sizeof(int) << std::endl;
	std::cout << "sizeof(long) " << sizeof(long) << std::endl;
	std::cout << "sizeof(long long) " << sizeof(long long) << std::endl;
	std::cout << "iterations " << iterations << std::endl;
	std::cout << "filename " << filename << std::endl;
	std::cout << std::endl;
}

int main(int argc, char**argv)
{
	computer_info();

	clearenv();
	for (int i=0; i<nr_keys; ++i)
	{
		char x[100];
		sprintf(x, "hello%d_%d", i, i);
		setenv(x, x, 0);
	}

	if (argc==2)
	{
		iterations = atoll(argv[1]);
		iterations1 = iterations / 100;
	}

	for (int i=0; i<benchmarkIterations; ++i)
	{
		std::cout << i << std::endl;

		benchmark_nothing();
		benchmark_getenv();
		benchmark_libc_getenv();
		benchmark_dl_libc_getenv(); //TODO
		benchmark_dl_next_getenv(); //TODO
		benchmark_bootstrap_getenv();

		benchmark_kslookup();

		// benchmark_hashmap();
		// benchmark_hashmap_find();
	}

	data << "value,benchmark" << std::endl;
}

