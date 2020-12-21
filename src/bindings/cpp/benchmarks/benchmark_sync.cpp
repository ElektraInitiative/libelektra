/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbthread.hpp>
#include <kdbtimer.hpp>

// long long iterations = 100000000000LL; // elitebenchmark lookup
// long long iterations = 100000LL; // elitebenchmark with/activate
long long iterations = 1000LL; // elitebenchmark sync/reload benchmark
// long long iterations = 100LL; // valgrind

const int benchmarkIterations = 11; // is a good number to not need mean values for median

const std::string filename = "check.txt";
std::ofstream dump (filename);

const uint32_t value = 55;
const char * s_value = "55";
const uint32_t othervalue = 66;
const char * s_othervalue = "66";

kdb::ThreadInteger::type add_contextual (kdb::ThreadInteger const & i1, kdb::ThreadInteger const & i2)
{
	return i1 + i2;
}

std::unique_ptr<Timer> tSync[10]{
	std::unique_ptr<Timer> (new Timer ("layer sync0", Timer::quiet)), std::unique_ptr<Timer> (new Timer ("layer sync1", Timer::quiet)),
	std::unique_ptr<Timer> (new Timer ("layer sync2", Timer::quiet)), std::unique_ptr<Timer> (new Timer ("layer sync3", Timer::quiet)),
	std::unique_ptr<Timer> (new Timer ("layer sync4", Timer::quiet)), std::unique_ptr<Timer> (new Timer ("layer sync5", Timer::quiet)),
	std::unique_ptr<Timer> (new Timer ("layer sync6", Timer::quiet)), std::unique_ptr<Timer> (new Timer ("layer sync7", Timer::quiet)),
	std::unique_ptr<Timer> (new Timer ("layer sync8", Timer::quiet)), std::unique_ptr<Timer> (new Timer ("layer sync9", Timer::quiet))
};

std::vector<std::shared_ptr<kdb::ThreadInteger>> createCV (kdb::KeySet & ks, kdb::ThreadContext & tc, int N)
{
	std::vector<std::shared_ptr<kdb::ThreadInteger>> vi;
	for (long long i = 0; i < N; ++i)
	{
		std::ostringstream os;
		os << "/test/layer1";
		/*
		for (long long j = 0; j < i; ++j)
		{
			os << "/%layer" << j << "%";
		}
		*/
		os << "/" << i;
		// std::cout << os.str().c_str() << std::endl;
		vi.push_back (std::make_shared<kdb::ThreadInteger> (ks, tc,
								    kdb::Key (os.str ().c_str (), KEY_META, "default", s_value, KEY_END)));
	}
	return vi;
}


__attribute__ ((noinline)) void benchmark_layer_syncN (long long N)
{
	kdb::Coordinator c;
	kdb::ThreadContext tc (c);
	kdb::KeySet ks;
	kdb::ThreadInteger ti (ks, tc, kdb::Key ("/test/nolayer", KEY_META, "default", s_value, KEY_END));
	ti = 5;
	kdb::ThreadInteger::type x = ti;

	Timer & t = *tSync[N];
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		tc.sync ();
		x ^= add_contextual (ti, ti);
	}
	t.stop ();
	std::cout << t;
	dump << t.name << x << std::endl;
}

std::unique_ptr<Timer> tReload[10]{ std::unique_ptr<Timer> (new Timer ("layer reload0", Timer::quiet)),
				    std::unique_ptr<Timer> (new Timer ("layer reload1", Timer::quiet)),
				    std::unique_ptr<Timer> (new Timer ("layer reload2", Timer::quiet)),
				    std::unique_ptr<Timer> (new Timer ("layer reload3", Timer::quiet)),
				    std::unique_ptr<Timer> (new Timer ("layer reload4", Timer::quiet)),
				    std::unique_ptr<Timer> (new Timer ("layer reload5", Timer::quiet)),
				    std::unique_ptr<Timer> (new Timer ("layer reload6", Timer::quiet)),
				    std::unique_ptr<Timer> (new Timer ("layer reload7", Timer::quiet)),
				    std::unique_ptr<Timer> (new Timer ("layer reload8", Timer::quiet)),
				    std::unique_ptr<Timer> (new Timer ("layer reload9", Timer::quiet)) };

__attribute__ ((noinline)) void benchmark_kdb_reloadN (long long N)
{
	kdb::Coordinator c;
	kdb::ThreadContext tc (c);
	kdb::KeySet ks;
	kdb::KDB kdb;
	kdb::KDB kdb2;

	kdb.get (ks, "/test");
	kdb2.get (ks, "/test");
	ks.append (kdb::Key ("system:/test/key", KEY_VALUE, "value", KEY_END));
	kdb.set (ks, "/test");
	ks.append (kdb::Key ("system:/test/key2", KEY_VALUE, "value2", KEY_END));
	// kdb2.set (ks, "/test");

	kdb::ThreadInteger ti (ks, tc, kdb::Key ("/test/nolayer", KEY_META, "default", s_value, KEY_END));
	ti = 5;
	kdb::ThreadInteger::type x = ti;

	Timer & t = *tReload[N];

	std::vector<kdb::KDB> kdbx;
	kdbx.resize (iterations); // really expensive operation, but need to be clean for every benchmark

	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		// kdb.set(ks, "/test");
		kdbx[i].get (ks, "/test");
		tc.sync ();
		x ^= add_contextual (ti, ti);
	}
	t.stop ();
	std::cout << t;
	dump << t.name << x << std::endl;
}


template <int N>
class Layer : public kdb::Layer
{
public:
	std::string id () const override
	{
		std::string ret ("layerX");
		ret[5] = ('0' + N);
		return ret;
	}
	std::string operator() () const override
	{
		std::string ret ("X");
		ret[0] = '0' + N;
		return ret;
	}
};


std::unique_ptr<Timer> tSwitch[10]{ std::unique_ptr<Timer> (new Timer ("layer switch0", Timer::quiet)),
				    std::unique_ptr<Timer> (new Timer ("layer switch1", Timer::quiet)),
				    std::unique_ptr<Timer> (new Timer ("layer switch2", Timer::quiet)),
				    std::unique_ptr<Timer> (new Timer ("layer switch3", Timer::quiet)),
				    std::unique_ptr<Timer> (new Timer ("layer switch4", Timer::quiet)),
				    std::unique_ptr<Timer> (new Timer ("layer switch5", Timer::quiet)),
				    std::unique_ptr<Timer> (new Timer ("layer switch6", Timer::quiet)),
				    std::unique_ptr<Timer> (new Timer ("layer switch7", Timer::quiet)),
				    std::unique_ptr<Timer> (new Timer ("layer switch8", Timer::quiet)),
				    std::unique_ptr<Timer> (new Timer ("layer switch9", Timer::quiet)) };

__attribute__ ((noinline)) void benchmark_layer_switchN (long long N)
{
	kdb::Coordinator c;
	kdb::ThreadContext tc (c);
	kdb::KeySet ks;
	kdb::ThreadInteger ti (ks, tc, kdb::Key ("/test/nolayer", KEY_META, "default", s_value, KEY_END));
	ti = 5;
	kdb::ThreadInteger::type x = ti;

	Timer & t = *tSwitch[N];
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		if (N > 0) tc.activate<Layer<0>> ();
		if (N > 1) tc.activate<Layer<1>> ();
		if (N > 2) tc.activate<Layer<2>> ();
		if (N > 3) tc.activate<Layer<3>> ();
		if (N > 4) tc.activate<Layer<4>> ();
		if (N > 5) tc.activate<Layer<5>> ();
		if (N > 6) tc.activate<Layer<6>> ();
		if (N > 7) tc.activate<Layer<7>> ();
		if (N > 8) tc.activate<Layer<8>> ();
		if (N > 9) tc.activate<Layer<9>> ();
		x ^= add_contextual (ti, ti);
	}
	t.stop ();
	std::cout << t;
	dump << t.name << x << std::endl;
}

std::unique_ptr<Timer> tCV[10]{
	std::unique_ptr<Timer> (new Timer ("CV switch0", Timer::quiet)), std::unique_ptr<Timer> (new Timer ("CV switch1", Timer::quiet)),
	std::unique_ptr<Timer> (new Timer ("CV switch2", Timer::quiet)), std::unique_ptr<Timer> (new Timer ("CV switch3", Timer::quiet)),
	std::unique_ptr<Timer> (new Timer ("CV switch4", Timer::quiet)), std::unique_ptr<Timer> (new Timer ("CV switch5", Timer::quiet)),
	std::unique_ptr<Timer> (new Timer ("CV switch6", Timer::quiet)), std::unique_ptr<Timer> (new Timer ("CV switch7", Timer::quiet)),
	std::unique_ptr<Timer> (new Timer ("CV switch8", Timer::quiet)), std::unique_ptr<Timer> (new Timer ("CV switch9", Timer::quiet))
};

__attribute__ ((noinline)) void benchmark_cv_switchN (long long N)
{
	kdb::Coordinator c;
	kdb::ThreadContext tc (c);
	kdb::KeySet ks;
	kdb::ThreadInteger ti (ks, tc, kdb::Key ("/test/nolayer", KEY_META, "default", s_value, KEY_END));
	ti = 5;
	kdb::ThreadInteger::type x = ti;

	std::vector<std::shared_ptr<kdb::ThreadInteger>> vi = createCV (ks, tc, N);

	Timer & t = *tCV[N];
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		if (N > 0) tc.activate (*vi[0]);
		if (N > 1) tc.activate (*vi[1]);
		if (N > 2) tc.activate (*vi[2]);
		if (N > 3) tc.activate (*vi[3]);
		if (N > 4) tc.activate (*vi[4]);
		if (N > 5) tc.activate (*vi[5]);
		if (N > 6) tc.activate (*vi[6]);
		if (N > 7) tc.activate (*vi[7]);
		if (N > 8) tc.activate (*vi[8]);
		if (N > 9) tc.activate (*vi[9]);
		x ^= add_contextual (ti, ti);
	}
	t.stop ();
	std::cout << t;
	dump << t.name << x << std::endl;
}

#include <unistd.h>
#ifdef _WIN32
#include <winsock2.h>
#endif

void computer_info ()
{
	char hostname[1024];
	gethostname (hostname, 1023);
	std::cout << std::endl;
	std::cout << std::endl;
	std::cout << "hostname " << hostname << std::endl;
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
	std::cout << "sizeof(Integer::type) " << sizeof (kdb::ThreadInteger::type) << std::endl;
	std::cout << "value " << value << std::endl;
	std::cout << "othervalue " << othervalue << std::endl;
	std::cout << "iterations " << iterations << std::endl;
	std::cout << "filename " << filename << std::endl;
	std::cout << std::endl;
}

int main (int argc, char ** argv)
{
	computer_info ();
	std::cout << std::endl;

	{
		using namespace kdb;
		std::string testRoot = "/test/";
		Key parent (testRoot, KEY_END);

		KDB first;
		KeySet firstReturned;
		first.get (firstReturned, parent);
		firstReturned.append (Key ("system:/" + testRoot + "key1", KEY_VALUE, "value1", KEY_END));

		KDB second;
		KeySet secondReturned;
		second.get (secondReturned, parent);
		secondReturned.append (Key ("system:/" + testRoot + "key2", KEY_VALUE, "value2", KEY_END));

		second.set (secondReturned, parent);
		// first.set(firstReturned, parent); // exception expected
	}

	if (argc == 2)
	{
		iterations = atoll (argv[1]);
	}

	for (int i = 0; i < benchmarkIterations; ++i)
	{
		std::cout << i << std::endl;
		for (int j = 0; j < 10; ++j)
		{
			benchmark_layer_syncN (j);
			benchmark_kdb_reloadN (j);
			benchmark_layer_switchN (j);
			benchmark_cv_switchN (j);
		}
	}

	for (int i = 0; i < 10; ++i)
	{
		std::cerr << i << "," << tSync[i]->getMedian () << "," << tReload[i]->getMedian () << "," << tSwitch[i]->getMedian () << ","
			  << tCV[i]->getMedian () << std::endl;
	}

	// data << "value,benchmark" << std::endl;
}
