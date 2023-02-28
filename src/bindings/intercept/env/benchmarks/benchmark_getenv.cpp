/**
 * @file
 *
 * @brief benchmark for getenv
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdbtimer.hpp>
#include <keyset.hpp>

#include <fstream>
#include <iostream>
#include <unistd.h>

#include <dlfcn.h>
#include <string.h>
extern "C" char ** environ;


const long long nr_keys = 30;

// long long iterations = 100000000000LL; // elitebenchmark lookup
long long iterations = 1000000LL; // elitebenchmark
// long long iterations = 100LL; // valgrind

// not needed in benchmarks:
long long iterations1 = iterations / 100;

const int benchmarkIterations = 11; // is a good number to not need mean values for median

const std::string filename = "check.txt";

const std::string csvfilename = "data.csv";

std::ofstream dump (filename);
std::ofstream data (csvfilename);


// from libgetenv
namespace ckdb
{
char * elektraBootstrapGetEnv (const char * name);
}


// very fast benchmarks without any if:

__attribute__ ((noinline)) void benchmark_nothing ()
{
	static Timer t ("nothing");

	t.start ();
	t.stop ();
	std::cout << t;
	dump << t.name << std::endl;
}

__attribute__ ((noinline)) void benchmark_getenv ()
{
	static Timer t ("elektra getenv");

	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		getenv ("HELLO");
		__asm__ ("");
	}
	t.stop ();
	std::cout << t;
	dump << t.name << std::endl;
}

__attribute__ ((noinline)) void benchmark_dl_next_getenv ()
{
	static Timer t ("dl next getenv");
	typedef char * (*gfcn) (const char *);
	union Sym
	{
		void * d;
		gfcn f;
	} sym;
	sym.d = dlsym (RTLD_NEXT, "getenv");
	gfcn dl_libc_getenv = sym.f;

	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		dl_libc_getenv ("HELLO");
		__asm__ ("");
	}
	t.stop ();
	std::cout << t;
	dump << t.name << std::endl;
}


__attribute__ ((noinline)) void benchmark_libc_getenv ()
{
	typedef char * (*gfcn) (const char *);
	union Sym
	{
		void * d;
		gfcn f;
	} sym;

	void * handle = dlopen ("/lib/x86_64-linux-gnu/libc-2.19.so", RTLD_NOW);
	if (!handle)
	{
		handle = dlopen ("/lib/i386-linux-gnu/libc-2.19.so", RTLD_NOW);
	}
	if (!handle)
	{
		std::cout << "Aborting, could not find libc" << std::endl;
		return;
	}

	static Timer t ("libc getenv");
	sym.d = dlsym (handle, "getenv");
	gfcn dl_libc_getenv = sym.f;

	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		dl_libc_getenv ("HELLO");
		__asm__ ("");
	}
	t.stop ();
	std::cout << t;
	dump << t.name << std::endl;
}


__attribute__ ((noinline)) void benchmark_bootstrap_getenv ()
{
	static Timer t ("bootstrap getenv");
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		ckdb::elektraBootstrapGetEnv ("HELLO");
		__asm__ ("");
	}
	t.stop ();
	std::cout << t;
	dump << t.name << std::endl;
}

__attribute__ ((noinline)) void benchmark_kslookup ()
{
	static Timer t ("kslookup");
	using namespace kdb; // needed for KS_END
	kdb::KeySet ks (100,
			/*
			 *kdb::Key("user:/env/override/some", KEY_END),
			 *kdb::Key("user:/env/override/a/key", KEY_END),
			 *kdb::Key("user:/env/override/b/key", KEY_END),
			 *kdb::Key("user:/env/override/c/key", KEY_END),
			 *kdb::Key("user:/env/override/d/key", KEY_END),
			 */
			KS_END);
	for (int i = 0; i < nr_keys; ++i)
	{
		char x[100];
		sprintf (x, "user:/env/override/hello%d_%d", i, i);
		ks.append (*kdb::Key (x, KEY_END));
	}
	Key lookupKey ("user:/env/override/HELLO", KEY_END);
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		ks.lookup (lookupKey);
		__asm__ ("");
	}
	t.stop ();
	std::cout << t;
}

void computer_info ()
{
	std::cout << std::endl;
	std::cout << std::endl;
#ifndef _WIN32
	char hostname[1024];
	gethostname (hostname, 1023);
	std::cout << "hostname " << hostname << std::endl;
#endif
#ifdef __GNUC__
	std::cout << "gcc: " << __GNUC__ << std::endl;
#endif
#ifdef __INTEL_COMPILER
	std::cout << "icc: " << __INTEL_COMPILER << std::endl;
#endif
#ifdef __clang__
	std::cout << "clang: " << __clang__ << std::endl;
#endif
	std::cout << "sizeof(int) " << sizeof (int) << std::endl;
	std::cout << "sizeof(long) " << sizeof (long) << std::endl;
	std::cout << "sizeof(long long) " << sizeof (long long) << std::endl;
	std::cout << "iterations " << iterations << std::endl;
	std::cout << "filename " << filename << std::endl;
	std::cout << std::endl;
}

int main (int argc, char ** argv)
{
	computer_info ();

	clearenv ();
	for (int i = 0; i < nr_keys; ++i)
	{
		char x[100];
		sprintf (x, "hello%d_%d", i, i);
		setenv (x, x, 0);
	}

	if (argc == 2)
	{
		iterations = atoll (argv[1]);
		iterations1 = iterations / 100;
	}

	for (int i = 0; i < benchmarkIterations; ++i)
	{
		std::cout << i << std::endl;

		benchmark_nothing ();
		benchmark_getenv ();
		benchmark_libc_getenv ();
		benchmark_bootstrap_getenv ();

		benchmark_kslookup ();

		// benchmark_hashmap();
		// benchmark_hashmap_find();
	}

	data << "value,benchmark" << std::endl;
}
