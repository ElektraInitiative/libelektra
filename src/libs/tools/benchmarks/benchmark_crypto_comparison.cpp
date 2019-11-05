/**
 * @file
 *
 * @brief benchmark for comparing the cryptographic providers used in the crypto plugin.
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
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
	NO_CRYPTO = 0, ///< use no crypto variant
	CRYPTO_OPENSSL = 1,
	CRYPTO_GCRYPT = 2,
	CRYPTO_BOTAN = 3,
	FCRYPT = 4
};

static const std::string plugin_variant_names[] = { "no_crypto_plugin", "crypto_openssl", "crypto_gcrypt", "crypto_botan", "fcrypt" };

long long nr_keys = 100LL; // should be higher but crypto performance is actually quite bad atm

const int benchmarkIterations = 11; // is a good number to not need mean values for median

kdb::KeySet toWrite;

template <enum PluginVariant VARIANT>
kdb::Key getMountpointForIteration (int iteration)
{
	return kdb::Key ("user:/benchmark_" + plugin_variant_names[VARIANT] + std::to_string (iteration), KEY_END);
}

template <enum PluginVariant VARIANT>
kdb::Key mountBackend (int iteration)
{
	using namespace kdb;
	using namespace kdb::tools;

	Key mp = getMountpointForIteration<VARIANT> (iteration);
	std::string cf = "benchmark_" + plugin_variant_names[VARIANT] + "_" + std::to_string (iteration) + ".ecf";
	unlink (cf.c_str ());

	KDB kdb;
	KeySet mountConfig;
	kdb.get (mountConfig, "system:/elektra/mountpoints");

	MountBackendBuilder b;
	b.setMountpoint (mp, KeySet (0, KS_END));
	b.addPlugin (PluginSpec ("resolver"));
	b.useConfigFile (cf);

	b.addPlugin (PluginSpec ("dump"));
	if (VARIANT != NO_CRYPTO)
	{
		KeySet pluginConfig;
		pluginConfig.append (Key ("user:/encrypt/key", KEY_VALUE, GPG_TEST_KEY_ID, KEY_END));
		pluginConfig.append (Key ("user:/gpg/unit_test", KEY_VALUE, "1", KEY_END));
		b.addPlugin (PluginSpec (plugin_variant_names[VARIANT], pluginConfig));
	}

	b.validated ();
	b.serialize (mountConfig);
	kdb.set (mountConfig, "system:/elektra/mountpoints");
	kdb.close ();
	return mp;
}

template <enum PluginVariant VARIANT>
__attribute__ ((noinline)) void benchmark_crypto_set (int iteration)
{
	using namespace kdb;
	using namespace kdb::tools;
	static Timer t (plugin_variant_names[VARIANT]);

	Key mp = mountBackend<VARIANT> (iteration);

	{
		KDB kdb;
		KeySet ks;

		kdb.get (ks, mp);
		for (int i = 0; i < nr_keys; ++i)
		{
			// clang-format off
			ks.append (Key (mp.getName () + "/k" + std::to_string (i),
					KEY_VALUE, "value",
					KEY_META, "crypto/encrypt", "1",
					KEY_END));
			// clang-format on
		}

		/***************************************************************************
		 * start of measurement
		 **************************************************************************/
		t.start ();
		kdb.set (ks, mp);
		t.stop ();
		/***************************************************************************
		 * end of measurement
		 **************************************************************************/

		kdb.close ();
	}

	std::cout << t;
}

template <enum PluginVariant VARIANT>
__attribute__ ((noinline)) void benchmark_crypto_get (int iteration)
{
	using namespace kdb;
	using namespace kdb::tools;
	static Timer t (plugin_variant_names[VARIANT]);

	KDB kdb;
	KeySet ks;
	Key mp = getMountpointForIteration<VARIANT> (iteration);

	/***************************************************************************
	 * start of measurement
	 **************************************************************************/
	t.start ();
	kdb.get (ks, mp);
	t.stop ();
	/***************************************************************************
	 * end of measurement
	 **************************************************************************/

	kdb.close ();
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

	std::cout << "----- KDB SET -----" << std::endl;
	for (int i = 0; i < benchmarkIterations; ++i)
	{
		std::cout << i << std::endl;

		benchmark_crypto_set<NO_CRYPTO> (i);
		benchmark_crypto_set<CRYPTO_OPENSSL> (i);
		benchmark_crypto_set<CRYPTO_GCRYPT> (i);
		benchmark_crypto_set<CRYPTO_BOTAN> (i);
		benchmark_crypto_set<FCRYPT> (i);
	}

	std::cout << "----- KDB GET -----" << std::endl;
	for (int i = 0; i < benchmarkIterations; ++i)
	{
		std::cout << i << std::endl;

		benchmark_crypto_get<NO_CRYPTO> (i);
		benchmark_crypto_get<CRYPTO_OPENSSL> (i);
		benchmark_crypto_get<CRYPTO_GCRYPT> (i);
		benchmark_crypto_get<CRYPTO_BOTAN> (i);
		benchmark_crypto_get<FCRYPT> (i);
	}
}
