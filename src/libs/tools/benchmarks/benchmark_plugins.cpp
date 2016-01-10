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

#include <fstream>
#include <iostream>

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


__attribute__((noinline)) void benchmark_nothing()
{
	static Timer t("nothing");

	t.start();
	t.stop();
	std::cout << t;
	dump << t.name << std::endl;
}

template <int PLUGINS>
__attribute__((noinline)) void benchmark_backend()
{
	using namespace kdb;
	using namespace kdb::tools;
	static Timer t("backend"+std::to_string(PLUGINS));

	t.start();

	for (int i = 0; i < iterations; ++i)
	{
		Backend b;
		b.setMountpoint(Key("/", KEY_CASCADING_NAME, KEY_END), KeySet(0, KS_END));
		b.addPlugin(PluginSpec("resolver"));
		b.addPlugin(PluginSpec("dump"));
		for (int p = 0; p < PLUGINS; ++p)
		{
			b.addPlugin(PluginSpec("type", p));
		}
		b.useConfigFile("file.ecf");
		b.validated();

		KeySet mountConfig;
		b.serialize(mountConfig);
	}
	t.stop();
	std::cout << t;
	dump << t.name << std::endl;
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
		benchmark_backend<0>();
		benchmark_backend<1>();
		benchmark_backend<2>();
		benchmark_backend<3>();
		benchmark_backend<4>();
	}

	data << "value,benchmark" << std::endl;
}

