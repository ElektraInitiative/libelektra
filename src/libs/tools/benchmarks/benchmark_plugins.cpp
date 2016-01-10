/**
 * @file
 *
 * @brief benchmark for getenv
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <backendbuilder.hpp>
#include <kdbtimer.hpp>

#include <unistd.h>

#include <fstream>
#include <iostream>

extern "C" char **environ;


const long long nr_keys = 1000LL;
long long iterations = 100000LL; // markusbyte

// not needed in benchmarks:
long long iterations1 = iterations / 100;

const int benchmarkIterations = 11; // is a good number to not need mean values for median

const std::string filename = "check.txt";

kdb::KeySet toWrite;


__attribute__((noinline)) void benchmark_nothing()
{
	static Timer t("nothing");

	t.start();
	unlink ("/etc/kdb/default.ecf");
	t.stop();
	std::cout << t;
}

template <int PLUGINS>
__attribute__((noinline)) void benchmark_backend()
{
	using namespace kdb;
	using namespace kdb::tools;
	static Timer t("backend"+std::to_string(PLUGINS));

	Key mp ("system/iterate/" + std::to_string(PLUGINS), KEY_END);
	std::string cf = "/tmp/file"+std::to_string(PLUGINS)+".ecf";
	unlink (cf.c_str());

	{
		KDB kdb;
		KeySet mountConfig;
		kdb.get (mountConfig, "system/elektra/mountpoints");

		Backend b;
		b.setMountpoint(mp, KeySet(0, KS_END));
		b.addPlugin(PluginSpec("resolver"));
		b.addPlugin(PluginSpec("dump"));
		b.useConfigFile(cf);
		b.validated();

		b.serialize(mountConfig);

		kdb.set (mountConfig, "system/elektra/mountpoints");
	}

	{
		KeySet ks;
		KDB kdb;
		kdb.get (ks, mp);
		for (int i = 0; i< nr_keys; ++i)
		{
			ks.append (Key ("system/iterate/" + std::to_string (i%10) + "/" + std::to_string (i/10),
					KEY_VALUE, "value",
					KEY_META, "iterate", "value",
					KEY_META, "iterate0", "value",
					KEY_META, "iterate1", "value",
					KEY_META, "iterate2", "value",
					KEY_META, "iterate3", "value",
					KEY_META, "iterate4", "value",
					KEY_META, "iterate5", "value",
					KEY_META, "iterate6", "value",
					KEY_META, "iterate7", "value",
					KEY_META, "iterate8", "value",
					KEY_META, "iterate9", "value",
					KEY_END));
		}
		kdb.set (ks, mp);
	}

	KDB kdb;
	t.start();
	for (int i = 0; i< iterations; ++i)
	{
		KeySet ks;
		kdb.get (ks, mp);
		kdb.set (ks, mp);
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

	if (argc==2)
	{
		iterations = atoll(argv[1]);
		iterations1 = iterations / 100;
	}

	for (int i=0; i<benchmarkIterations; ++i)
	{
		std::cout << i << std::endl;

		benchmark_nothing();
		benchmark_backend<0>();
		benchmark_backend<1>();
		benchmark_backend<2>();
		benchmark_backend<3>();
		benchmark_backend<4>();
		benchmark_backend<5>();
		benchmark_backend<6>();
		benchmark_backend<7>();
		benchmark_backend<8>();
		benchmark_backend<9>();
	}
	std::cout << "value,benchmark" << std::endl;
}

