/**
 * @file
 *
 * @brief benchmark for getenv
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include <backendbuilder.hpp>
#include <kdbconfig.h>
#include <kdbtimer.hpp>

#include <unistd.h>

#include <fstream>
#include <iostream>

extern "C" char ** environ;

// test key is located at src/plugins/crypto/test_key.asc
#define GPG_TEST_KEY_ID "DDEBEF9EE2DC931701338212DAF635B17F230E8D"

enum PluginVariant
{
	NO_CRYPTO = 0,
	CRYPTO_OPENSSL = 1,
	CRYPTO_GCRYPT = 2,
	CRYPTO_BOTAN = 3
};

static const std::string plugin_variant_names[] = { "no_crypto_plugin", "crypto_openssl", "crypto_gcrypt", "crypto_botan" };

long long nr_keys = 100LL; // should be higher but crypto performance is actually quite bad atm

const int benchmarkIterations = 11; // is a good number to not need mean values for median

kdb::KeySet toWrite;

template <int VARIANT>
kdb::Key mountBackend (int iteration)
{
	using namespace kdb;
	using namespace kdb::tools;

	Key mp ("user/iterate/" + plugin_variant_names[VARIANT] + std::to_string (iteration), KEY_END);
	std::string cf = "benchmark_" + plugin_variant_names[VARIANT] + "_" + std::to_string (iteration) + ".ecf";
	// unlink (cf.c_str ());

	KDB kdb;
	KeySet mountConfig;
	kdb.get (mountConfig, "system/elektra/mountpoints");

	MountBackendBuilder b;
	b.setMountpoint (mp, KeySet (0, KS_END));
	b.addPlugin (PluginSpec ("resolver"));
	b.addPlugin (PluginSpec ("dump"));
	if (VARIANT > 0)
	{
		KeySet pluginConfig;
		pluginConfig.append (Key ("/gpg/key", KEY_VALUE, GPG_TEST_KEY_ID, KEY_END));
		b.addPlugin (PluginSpec (plugin_variant_names[VARIANT], pluginConfig));
	}
	b.useConfigFile (cf);
	b.validated ();
	b.serialize (mountConfig);
	kdb.set (mountConfig, "system/elektra/mountpoints");
	kdb.close ();
	return mp;
}

template <int VARIANT>
__attribute__ ((noinline)) void benchmark_crypto (int iteration)
{
	using namespace kdb;
	using namespace kdb::tools;
	static Timer t (plugin_variant_names[VARIANT]);

	Key mp = mountBackend<VARIANT> (iteration);

	{
		KDB kdb;
		KeySet ks;

		/***************************************************************************
		 * start of measurement
		 **************************************************************************/
		t.start ();
		kdb.get (ks, mp);
		for (int i = 0; i < nr_keys; ++i)
		{
			// clang-format off
			ks.append (Key ("user/iterate/" + plugin_variant_names[VARIANT] + std::to_string(iteration) + "/k" + std::to_string (i),
					KEY_VALUE, "value",
					KEY_META, "crypto/encrypt", "1",
					KEY_END));
			// clang-format on
		}
		kdb.set (ks, mp);
		kdb.close ();
		t.stop ();
		/***************************************************************************
		 * end of measurement
		 **************************************************************************/
	}

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
	std::cout << "number of keys " << nr_keys << std::endl;
	std::cout << std::endl;
}

int main (int argc, char ** argv)
{
	if (argc > 1)
	{
		nr_keys = atoll (argv[1]);
	}

	computer_info ();

#ifdef HAVE_CLEARENV
	clearenv ();
#endif

	for (int i = 0; i < benchmarkIterations; ++i)
	{
		std::cout << i << std::endl;

		benchmark_crypto<NO_CRYPTO> (i);
		benchmark_crypto<CRYPTO_OPENSSL> (i);
		benchmark_crypto<CRYPTO_GCRYPT> (i);
		benchmark_crypto<CRYPTO_BOTAN> (i);
	}
}
