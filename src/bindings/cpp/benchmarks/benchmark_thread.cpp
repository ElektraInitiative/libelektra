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
long long iterations = 100000LL; // elitebenchmark with/activate
// long long iterations = 100LL; // valgrind

// not needed in benchmarks:
long long iterations1 = iterations / 100;

const int benchmarkIterations = 11; // is a good number to not need mean values for median

const std::string filename = "check.txt";

const std::string csvfilename = "data.csv";

std::ofstream dump (filename);
std::ofstream data (csvfilename);


const uint32_t value = 55;
const char * s_value = "55";
const uint32_t othervalue = 66;
const char * s_othervalue = "66";

namespace kdb
{

class Person : public ThreadString
{
public:
	Person (KeySet & ks, ThreadContext & context_)
	: ThreadString (ks, context_, Key ("/%layer1%/person", KEY_META, "default", "no name", KEY_END))
	{
	}
	using ThreadString::operator=;

private:
	Person () = delete;
	Person (Person const &) = delete;
	Person (Person &&) = delete;
};

class Nested : public ThreadInteger
{
public:
	Nested (KeySet & ks, ThreadContext & context_)
	: ThreadInteger (ks, context_, Key ("/test/%layer1%/%thread%/%layer2%/nested", KEY_META, "default", s_value, KEY_END))
	{
	}
	using ThreadInteger::operator=;
};

class Environment : public ThreadBoolean
{
public:
	Environment (KeySet & ks, ThreadContext & context_)
	: ThreadBoolean (ks, context_, Key ("/test/%layer1%", KEY_META, "default", "1", KEY_END)), nested (ks, context_),
	  person (ks, context_), profile (ks, context_, Key ("/%layer1%/profile", KEY_META, "default", "default", KEY_END)),
	  bm (ks, context_,
	      Key ("/%layer1%/%layer2%/%layer3%/%layer4%/%layer5%/%layer6%/%layer7%/%layer8%/%layer9%/", KEY_META, "default", s_value,
		   KEY_END))
	{
	}

	Nested nested;
	Person person;
	ThreadString profile;
	ThreadInteger bm;
};
} // namespace kdb


// benchmarking

class Layer1 : public kdb::Layer
{
public:
	std::string id () const override
	{
		return "layer1";
	}
	std::string operator() () const override
	{
		return "1";
	}
};
class Layer2 : public kdb::Layer
{
public:
	std::string id () const override
	{
		return "layer2";
	}
	std::string operator() () const override
	{
		return "2";
	}
};
class Layer3 : public kdb::Layer
{
public:
	std::string id () const override
	{
		return "layer3";
	}
	std::string operator() () const override
	{
		return "3";
	}
};
class Layer4 : public kdb::Layer
{
public:
	std::string id () const override
	{
		return "layer4";
	}
	std::string operator() () const override
	{
		return "4";
	}
};
class Layer5 : public kdb::Layer
{
public:
	std::string id () const override
	{
		return "layer5";
	}
	std::string operator() () const override
	{
		return "5";
	}
};
class Layer6 : public kdb::Layer
{
public:
	std::string id () const override
	{
		return "layer6";
	}
	std::string operator() () const override
	{
		return "6";
	}
};
class Layer7 : public kdb::Layer
{
public:
	std::string id () const override
	{
		return "layer7";
	}
	std::string operator() () const override
	{
		return "7";
	}
};
class Layer8 : public kdb::Layer
{
public:
	std::string id () const override
	{
		return "layer8";
	}
	std::string operator() () const override
	{
		return "8";
	}
};
class Layer9 : public kdb::Layer
{
public:
	std::string id () const override
	{
		return "layer9";
	}
	std::string operator() () const override
	{
		return "9";
	}
};

kdb::ThreadInteger::type add_native (uint32_t const & i1, uint32_t const & i2)
{
	return i1 + i2;
}

kdb::ThreadInteger::type add_contextual (kdb::ThreadInteger const & i1, kdb::ThreadInteger const & i2)
{
	return i1 + i2;
}


// very fast benchmarks without any if:

__attribute__ ((noinline)) void benchmark_nothing ()
{
	static Timer t ("nothing");

	t.start ();
	t.stop ();
	std::cout << t;
}

__attribute__ ((noinline)) void benchmark_native ()
{
	kdb::ThreadInteger::type val = value;
	kdb::ThreadInteger::type x = 0;

	static Timer t ("native cmp noif");
	t.start ();
	//{benchmark/loop}
	for (long long i = 0; i < iterations; ++i)
	{
		x ^= add_native (val, val);
	}
	//{end}
	t.stop ();
	std::cout << t;
	dump << t.name << x << std::endl;
}

__attribute__ ((noinline)) void benchmark_native_sum ()
{
	kdb::ThreadInteger::type val = value;
	kdb::ThreadInteger::type x = 0;

	static Timer t ("native noif sum");
	t.start ();
	//{benchmark/sumloop}
	for (long long i = 0; i < iterations; ++i)
	{
		x += add_native (val, val);
	}
	//{end}
	t.stop ();
	std::cout << t;
	dump << t.name << x << std::endl;
}

__attribute__ ((noinline)) void benchmark_native_sum_asm ()
{
	kdb::ThreadInteger::type val = value;
	kdb::ThreadInteger::type x = 0;

	static Timer t ("native sum asm");
	t.start ();
	//{benchmark/sumloopasm}
	for (long long i = 0; i < iterations; ++i)
	{
		x += add_native (val, val);
		__asm__ ("");
	}
	//{end}
	t.stop ();
	std::cout << t;
	dump << t.name << x << std::endl;
}

__attribute__ ((noinline)) void benchmark_contextual_noif (kdb::Environment & s)
{
	s.nested = value;
	kdb::ThreadInteger::type x = 0;

	static Timer t ("context cmp noif");
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		x ^= add_contextual (s.nested, s.nested);
	}
	t.stop ();
	std::cout << t;
	dump << t.name << x << std::endl;
}

__attribute__ ((noinline)) void benchmark_contextual_noif_sum_asm (kdb::Environment & s)
{
	s.nested = value;
	long long x = 0;

	static Timer t ("context sum asm");
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		x += add_contextual (s.nested, s.nested);
		__asm__ ("");
	}
	t.stop ();
	std::cout << t;
	dump << t.name << x << std::endl;
}


__attribute__ ((noinline)) void benchmark_contextual_noif_sum (kdb::Environment & s)
{
	s.nested = value;
	long long x = 0;

	static Timer t ("context noif sum");
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		x += add_contextual (s.nested, s.nested);
	}
	t.stop ();
	std::cout << t;
	dump << t.name << x << std::endl;
}


__attribute__ ((noinline)) void benchmark_hashmap ()
{
	std::unordered_map<std::string, kdb::ThreadInteger::type> hashmap;
	hashmap["user:/abc/test/hello"] = 5;
	hashmap["user:/abc/test/h1"] = 6;
	hashmap["user:/abc/test/h2"] = 7;
	hashmap["user:/abc/test/test"] = 9;
	hashmap["user:/abc/nested/hello"] = 12;
	kdb::ThreadInteger::type x = 0;

	static Timer t ("hashmap");
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		x ^= hashmap["user:/abc/nested/notav"];
	}
	t.stop ();
	std::cout << t;
	dump << "hashmap " << x << std::endl;
}

__attribute__ ((noinline)) void benchmark_hashmap_find ()
{
	std::unordered_map<std::string, kdb::ThreadInteger::type> hashmap;
	hashmap["user:/abc/test/hello"] = 5;
	hashmap["user:/abc/test/h1"] = 6;
	hashmap["user:/abc/test/h2"] = 7;
	hashmap["user:/abc/test/test"] = 9;
	hashmap["user:/abc/nested/hello"] = 12;
	kdb::ThreadInteger::type x = 0;

	static Timer t ("hashmap find");
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		auto it = hashmap.find ("user:/abc/nested/hello");
		if (it != hashmap.end ())
		{
			x ^= it->second;
		}
	}
	t.stop ();

	std::cout << t;
	dump << "hashmap " << x << std::endl;
}

__attribute__ ((noinline)) void benchmark_evaluate_no_sub (kdb::Environment & s)
{
	static Timer t ("evaluate no sub");
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		s.context ().evaluate ("some/string/but/%no%/%sub%/%that%/will/work");
	}
	t.stop ();
	std::cout << t;
}

__attribute__ ((noinline)) void benchmark_evaluate (kdb::Environment & s)
{
	static Timer t ("evaluate");
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		s.context ().evaluate ("some/%application%/%profile%/%at%/will/work");
	}
	t.stop ();
	std::cout << t;
}

__attribute__ ((noinline)) void benchmark_evaluate0 (kdb::Environment & s)
{
	static Timer t ("evaluate0");
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		s.context ().evaluate ("nolayer/_________________________________________________________/key");
	}
	t.stop ();
	std::cout << t;
}

__attribute__ ((noinline)) void benchmark_evaluate1 (kdb::Environment & s)
{
	static Timer t ("evaluate1");
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		s.context ().evaluate ("%layer%/_________________________________________________________/key");
	}
	t.stop ();
	std::cout << t;
}

__attribute__ ((noinline)) void benchmark_evaluate2 (kdb::Environment & s)
{
	static Timer t ("evaluate2");
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		s.context ().evaluate ("%layer%/%layer2%/________________________________________________/key");
	}
	t.stop ();
	std::cout << t;
}

__attribute__ ((noinline)) void benchmark_evaluate3 (kdb::Environment & s)
{
	static Timer t ("evaluate3");
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		s.context ().evaluate ("%layer%/%layer2%/%layer3%/_______________________________________/key");
	}
	t.stop ();
	std::cout << t;
}

__attribute__ ((noinline)) void benchmark_evaluate4 (kdb::Environment & s)
{
	static Timer t ("evaluate4");
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		s.context ().evaluate ("%layer%/%layer2%/%layer3%/%layer4%/______________________________/key");
	}
	t.stop ();
	std::cout << t;
}

__attribute__ ((noinline)) void benchmark_evaluate5 (kdb::Environment & s)
{
	static Timer t ("evaluate5");
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		s.context ().evaluate ("%layer%/%layer2%/%layer3%/%layer4%/%layer5%/_____________________/key");
	}
	t.stop ();
	std::cout << t;
}

__attribute__ ((noinline)) void benchmark_kslookup ()
{
	static Timer t ("kslookup");
	using namespace kdb; // needed for KS_END
	kdb::KeySet ks (100, *kdb::Key ("user:/hello/some", KEY_END), *kdb::Key ("user:/hello/a/key", KEY_END),
			*kdb::Key ("user:/hello/b/key", KEY_END), *kdb::Key ("user:/hello/c/key", KEY_END),
			*kdb::Key ("user:/hello/d/key", KEY_END), *kdb::Key ("user:/other", KEY_END), KS_END);
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		ks.lookup ("user:/notfound");
	}
	t.stop ();
	std::cout << t;
}

__attribute__ ((noinline)) void benchmark_layer_with (kdb::Environment & s)
{
	s.nested = value;
	s.context ().with<Layer1> () ([&] { s.nested = othervalue; });

	static Timer t ("layer with");
	t.start ();
	for (long long i = 0; i < iterations / 2; ++i)
	{
		s.context ().with<Layer1> () ([] {});
	}
	t.stop ();
	std::cout << t;
}

__attribute__ ((noinline)) void benchmark_layer_withx (kdb::Environment & s)
{
	s.nested = value;
	s.context ().with<Layer1> () ([&] { s.nested = othervalue; });
	kdb::ThreadInteger::type x = 0;

	static Timer t ("layer withx");
	t.start ();
	for (long long i = 0; i < iterations / 2; ++i)
	{
		s.context ().with<Layer1> () ([&] { x ^= add_contextual (s.bm, s.bm); });
		x ^= add_contextual (s.bm, s.bm);
	}
	t.stop ();
	std::cout << t;
}

__attribute__ ((noinline)) void benchmark_layer_with1valout (kdb::Environment & s)
{
	s.bm = value;
	s.context ().with<Layer1> () ([&] { s.bm = othervalue; });
	kdb::ThreadInteger::type x = 0;

	static Timer t ("layer with1valout");
	t.start ();
	for (long long i = 0; i < iterations / 2; ++i)
	{
		s.context ().activate<Layer1> ();
		x ^= add_contextual (s.bm, s.bm);
		s.context ().deactivate<Layer1> ();
		x ^= add_contextual (s.bm, s.bm);
	}
	t.stop ();
	std::cout << t;
	dump << t.name << x << std::endl;
}


__attribute__ ((noinline)) void benchmark_layer_with1val (kdb::Environment & s)
{
	s.bm = value;
	s.context ().with<Layer1> () ([&] { s.bm = othervalue; });
	kdb::ThreadInteger::type x = 0;

	static Timer t ("layer with1val");
	t.start ();
	for (long long i = 0; i < iterations1 / 2; ++i)
	{
		s.context ().with<Layer1> () ([&] { x ^= add_contextual (s.bm, s.bm); });
		x ^= add_contextual (s.bm, s.bm);
	}
	t.stop ();
	std::cout << t;
	dump << "Layer With1Val " << x << std::endl;
}

__attribute__ ((noinline)) void benchmark_layer_with2val (kdb::Environment & s)
{
	s.bm = value;
	s.context ().with<Layer2> () ([&] { s.bm = othervalue; });
	kdb::ThreadInteger::type x = 0;

	static Timer t ("layer with2val");
	t.start ();
	for (long long i = 0; i < iterations1 / 2; ++i)
	{
		s.context ().with<Layer1> ().with<Layer2> () ([&] { x ^= add_contextual (s.bm, s.bm); });
		x ^= add_contextual (s.bm, s.bm);
	}
	t.stop ();
	std::cout << t;
	dump << "Layer With2Val " << x << std::endl;
}

__attribute__ ((noinline)) void benchmark_layer_with3val (kdb::Environment & s)
{
	s.bm = value;
	s.context ().with<Layer2> () ([&] { s.bm = othervalue; });
	kdb::ThreadInteger::type x = 0;

	static Timer t ("layer with3val");
	t.start ();
	for (long long i = 0; i < iterations1 / 2; ++i)
	{
		s.context ().with<Layer1> ().with<Layer2> ().with<Layer3> () ([&] { x ^= add_contextual (s.bm, s.bm); });
		x ^= add_contextual (s.bm, s.bm);
	}
	t.stop ();
	std::cout << t;
	dump << "Layer With3Val " << x << std::endl;
}

__attribute__ ((noinline)) void benchmark_layer_with4val (kdb::Environment & s)
{
	s.bm = value;
	s.context ().with<Layer2> () ([&] { s.bm = othervalue; });
	kdb::ThreadInteger::type x = 0;

	static Timer t ("layer with4val");
	t.start ();
	for (long long i = 0; i < iterations1 / 2; ++i)
	{
		s.context ().with<Layer1> ().with<Layer2> ().with<Layer3> ().with<Layer4> () ([&] { x ^= add_contextual (s.bm, s.bm); });
		x ^= add_contextual (s.bm, s.bm);
	}
	t.stop ();
	std::cout << t;
	dump << "Layer With4Val " << x << std::endl;
}

__attribute__ ((noinline)) void benchmark_layer_with5val (kdb::Environment & s)
{
	s.bm = value;
	s.context ().with<Layer2> () ([&] { s.bm = othervalue; });
	kdb::ThreadInteger::type x = 0;

	static Timer t ("layer with5val");
	t.start ();
	for (long long i = 0; i < iterations1 / 2; ++i)
	{
		s.context ().with<Layer1> ().with<Layer2> ().with<Layer3> ().with<Layer4> ().with<Layer5> () (
			[&] { x ^= add_contextual (s.bm, s.bm); });
		x ^= add_contextual (s.bm, s.bm);
	}
	t.stop ();
	std::cout << t;
	dump << "Layer With5Val " << x << std::endl;
}

__attribute__ ((noinline)) void benchmark_layer_with6val (kdb::Environment & s)
{
	s.bm = value;
	s.context ().with<Layer2> () ([&] { s.bm = othervalue; });
	kdb::ThreadInteger::type x = 0;

	static Timer t ("layer with6val");
	t.start ();
	for (long long i = 0; i < iterations1 / 2; ++i)
	{
		s.context ().with<Layer1> ().with<Layer2> ().with<Layer3> ().with<Layer4> ().with<Layer5> ().with<Layer6> () (
			[&] { x ^= add_contextual (s.bm, s.bm); });
		x ^= add_contextual (s.bm, s.bm);
	}
	t.stop ();
	std::cout << t;
	dump << "Layer With6Val " << x << std::endl;
}

__attribute__ ((noinline)) void benchmark_layer_with7val (kdb::Environment & s)
{
	s.bm = value;
	s.context ().with<Layer2> () ([&] { s.bm = othervalue; });
	kdb::ThreadInteger::type x = 0;

	static Timer t ("layer with7val");
	t.start ();
	for (long long i = 0; i < iterations1 / 2; ++i)
	{
		s.context ()
			.with<Layer1> ()
			.with<Layer2> ()
			.with<Layer3> ()
			.with<Layer4> ()
			.with<Layer5> ()
			.with<Layer6> ()
			.with<Layer7> () ([&] { x ^= add_contextual (s.bm, s.bm); });
		x ^= add_contextual (s.bm, s.bm);
	}
	t.stop ();
	std::cout << t;
	dump << "Layer With7Val " << x << std::endl;
}

__attribute__ ((noinline)) void benchmark_layer_with8val (kdb::Environment & s)
{
	s.bm = value;
	s.context ().with<Layer2> () ([&] { s.bm = othervalue; });
	kdb::ThreadInteger::type x = 0;

	static Timer t ("layer with8val");
	t.start ();
	for (long long i = 0; i < iterations1 / 2; ++i)
	{
		s.context ()
			.with<Layer1> ()
			.with<Layer2> ()
			.with<Layer3> ()
			.with<Layer4> ()
			.with<Layer5> ()
			.with<Layer6> ()
			.with<Layer7> ()
			.with<Layer8> () ([&] { x ^= add_contextual (s.bm, s.bm); });
		x ^= add_contextual (s.bm, s.bm);
	}
	t.stop ();
	std::cout << t;
	dump << "Layer With8Val " << x << std::endl;
}

__attribute__ ((noinline)) void benchmark_layer_with9val (kdb::Environment & s)
{
	s.bm = value;
	s.context ().with<Layer2> () ([&] { s.bm = othervalue; });
	kdb::ThreadInteger::type x = 0;

	static Timer t ("layer with9val");
	t.start ();
	for (long long i = 0; i < iterations1 / 2; ++i)
	{
		s.context ()
			.with<Layer1> ()
			.with<Layer2> ()
			.with<Layer3> ()
			.with<Layer4> ()
			.with<Layer5> ()
			.with<Layer6> ()
			.with<Layer7> ()
			.with<Layer8> ()
			.with<Layer9> () ([&] { x ^= add_contextual (s.bm, s.bm); });
		x ^= add_contextual (s.bm, s.bm);
	}
	t.stop ();
	std::cout << t;
	dump << "Layer With9Val " << x << std::endl;
}

kdb::ThreadInteger::type g_x = 0;

__attribute__ ((noinline)) void benchmark_layer_withgval9 (kdb::Environment & s)
{
	s.bm = value;

	g_x = 0; // avoid reference overhead by lambda capture
	static Timer t ("layer withgval9");
	t.start ();
	for (long long i = 0; i < iterations1; ++i)
	{
		s.context ()
			.with<Layer1> ()
			.with<Layer2> ()
			.with<Layer3> ()
			.with<Layer4> ()
			.with<Layer5> ()
			.with<Layer6> ()
			.with<Layer7> ()
			.with<Layer8> ()
			.with<Layer9> () ([&] { g_x ^= add_contextual (s.bm, s.bm); });
		g_x ^= add_contextual (s.bm, s.bm);
	}
	t.stop ();
	std::cout << t;
	dump << t.name << g_x << std::endl;
}

__attribute__ ((noinline)) void benchmark_layer_withnoalloc9 (kdb::Environment & s)
{
	s.bm = value;

	kdb::ThreadInteger::type x = 0;
	static Timer t ("layer withnoalloc");
	t.start ();
	std::shared_ptr<kdb::Layer> l1 = std::make_shared<Layer1> ();
	std::shared_ptr<kdb::Layer> l2 = std::make_shared<Layer2> ();
	std::shared_ptr<kdb::Layer> l3 = std::make_shared<Layer3> ();
	std::shared_ptr<kdb::Layer> l4 = std::make_shared<Layer4> ();
	std::shared_ptr<kdb::Layer> l5 = std::make_shared<Layer5> ();
	std::shared_ptr<kdb::Layer> l6 = std::make_shared<Layer6> ();
	std::shared_ptr<kdb::Layer> l7 = std::make_shared<Layer7> ();
	std::shared_ptr<kdb::Layer> l8 = std::make_shared<Layer8> ();
	std::shared_ptr<kdb::Layer> l9 = std::make_shared<Layer9> ();
	for (long long i = 0; i < iterations1; ++i)
	{
		s.context ().withl (l1).withl (l2).withl (l3).withl (l4).withl (l5).withl (l6).withl (l7).withl (l8).withl (
			l9, [&] () { x ^= add_contextual (s.bm, s.bm); });
		x ^= add_contextual (s.bm, s.bm);
	}
	t.stop ();
	std::cout << t;
	dump << t.name << g_x << std::endl;
}

__attribute__ ((noinline)) void benchmark_layer_with1act (kdb::Environment & s)
{
	kdb::ThreadInteger::type x = 0;

	static Timer t ("layer with1act");
	t.start ();
	s.context ().activate<Layer1> ();
	for (long long i = 0; i < iterations; ++i)
	{
		x ^= add_contextual (s.bm, s.bm);
	}
	s.context ().deactivate<Layer1> ();
	t.stop ();
	std::cout << t;
	dump << t.name << x << std::endl;
}

__attribute__ ((noinline)) void benchmark_layer_with1outin (kdb::Environment & s)
{
	s.bm = value;
	s.context ().with<Layer2> () ([&] { s.bm = othervalue; });

	static Timer t ("layer with1outin");
	t.start ();
	s.context ().with<Layer1> () ([&] {
		kdb::ThreadInteger::type x = 0;
		for (long long i = 0; i < iterations; ++i)
		{
			x ^= add_contextual (s.bm, s.bm);
		}
		dump << t.name << x << std::endl;
	});
	t.stop ();
	std::cout << t;
}

__attribute__ ((noinline)) void benchmark_layer_with0out (kdb::Environment & s)
{
	static Timer t ("with 0 layer");
	s.bm = value;

	kdb::ThreadInteger::type x = 0;
	t.start ();
	for (long long i = 0; i < iterations; ++i)
	{
		x ^= add_contextual (s.bm, s.bm);
	}
	t.stop ();
	dump << t.name << x << std::endl;
	std::cout << t;
}


__attribute__ ((noinline)) void benchmark_layer_with1out (kdb::Environment & s)
{
	static Timer t ("with 1 layer");
	t.start ();
	s.context ().with<Layer1> () ([&] {
		kdb::ThreadInteger::type x = 0;
		s.bm = value;

		for (long long i = 0; i < iterations; ++i)
		{
			x ^= add_contextual (s.bm, s.bm);
		}
		dump << t.name << x << std::endl;
	});
	t.stop ();
	std::cout << t;
}


__attribute__ ((noinline)) void benchmark_layer_with2out (kdb::Environment & s)
{
	static Timer t ("with 2 layer");
	t.start ();
	//{benchmark/layer2}
	s.context ().with<Layer1> ().with<Layer2> () ([&] {
		s.bm = value;
		kdb::ThreadInteger::type x = 0;
		for (long long i = 0; i < iterations; ++i)
		{
			x ^= add_contextual (s.bm, s.bm);
		}
		dump << x << std::endl;
	});
	//{end}
	t.stop ();
	std::cout << t;
}


__attribute__ ((noinline)) void benchmark_layer_with3out (kdb::Environment & s)
{
	static Timer t ("with 3 layer");
	t.start ();
	s.context ().with<Layer1> ().with<Layer2> ().with<Layer3> () ([&] {
		s.bm = value;

		kdb::ThreadInteger::type x = 0;
		for (long long i = 0; i < iterations; ++i)
		{
			x ^= add_contextual (s.bm, s.bm);
		}
		dump << t.name << x << std::endl;
	});
	t.stop ();
	std::cout << t;
}


__attribute__ ((noinline)) void benchmark_layer_with4out (kdb::Environment & s)
{
	static Timer t ("with 4 layer");
	t.start ();
	s.context ().with<Layer1> ().with<Layer2> ().with<Layer3> ().with<Layer4> () ([&] {
		s.bm = value;

		kdb::ThreadInteger::type x = 0;
		for (long long i = 0; i < iterations; ++i)
		{
			x ^= add_contextual (s.bm, s.bm);
		}
		dump << t.name << x << std::endl;
	});
	t.stop ();
	std::cout << t;
}


__attribute__ ((noinline)) void benchmark_layer_with5out (kdb::Environment & s)
{
	static Timer t ("with 5 layer");
	t.start ();
	s.context ().with<Layer1> ().with<Layer2> ().with<Layer3> ().with<Layer4> ().with<Layer5> () ([&] {
		s.bm = value;

		kdb::ThreadInteger::type x = 0;
		for (long long i = 0; i < iterations; ++i)
		{
			x ^= add_contextual (s.bm, s.bm);
		}
		dump << t.name << x << std::endl;
	});
	t.stop ();
	std::cout << t;
}


__attribute__ ((noinline)) void benchmark_layer_with6out (kdb::Environment & s)
{
	static Timer t ("with 6 layer");
	t.start ();
	s.context ().with<Layer1> ().with<Layer2> ().with<Layer3> ().with<Layer4> ().with<Layer5> ().with<Layer6> () ([&] {
		s.bm = value;

		kdb::ThreadInteger::type x = 0;
		for (long long i = 0; i < iterations; ++i)
		{
			x ^= add_contextual (s.bm, s.bm);
		}
		dump << t.name << x << std::endl;
	});
	t.stop ();
	std::cout << t;
}


__attribute__ ((noinline)) void benchmark_layer_with7out (kdb::Environment & s)
{
	static Timer t ("with 7 layer");
	t.start ();
	s.context ().with<Layer1> ().with<Layer2> ().with<Layer3> ().with<Layer4> ().with<Layer5> ().with<Layer6> ().with<Layer7> () ([&] {
		s.bm = value;

		kdb::ThreadInteger::type x = 0;
		for (long long i = 0; i < iterations; ++i)
		{
			x ^= add_contextual (s.bm, s.bm);
		}
		dump << t.name << x << std::endl;
	});
	t.stop ();
	std::cout << t;
}


__attribute__ ((noinline)) void benchmark_layer_with8out (kdb::Environment & s)
{
	static Timer t ("with 8 layer");
	t.start ();
	s.context ()
		.with<Layer1> ()
		.with<Layer2> ()
		.with<Layer3> ()
		.with<Layer4> ()
		.with<Layer5> ()
		.with<Layer6> ()
		.with<Layer7> ()
		.with<Layer8> () ([&] {
			s.bm = value;

			kdb::ThreadInteger::type x = 0;
			for (long long i = 0; i < iterations; ++i)
			{
				x ^= add_contextual (s.bm, s.bm);
			}
			dump << t.name << x << std::endl;
		});
	t.stop ();
	std::cout << t;
}


__attribute__ ((noinline)) void benchmark_layer_with9out (kdb::Environment & s)
{
	static Timer t ("with 9 layer");
	t.start ();
	s.context ()
		.with<Layer1> ()
		.with<Layer2> ()
		.with<Layer3> ()
		.with<Layer4> ()
		.with<Layer5> ()
		.with<Layer6> ()
		.with<Layer7> ()
		.with<Layer8> ()
		.with<Layer9> () ([&] {
			s.bm = value;

			kdb::ThreadInteger::type x = 0;
			for (long long i = 0; i < iterations; ++i)
			{
				x ^= add_contextual (s.bm, s.bm);
			}
			dump << t.name << x << std::endl;
		});
	t.stop ();
	std::cout << t;
}

__attribute__ ((noinline)) void benchmark_layer_with9outsum (kdb::Environment & s)
{
	static Timer t ("with 9 layer sum");
	t.start ();
	s.context ()
		.with<Layer1> ()
		.with<Layer2> ()
		.with<Layer3> ()
		.with<Layer4> ()
		.with<Layer5> ()
		.with<Layer6> ()
		.with<Layer7> ()
		.with<Layer8> ()
		.with<Layer9> () ([&] {
			s.bm = value;

			kdb::ThreadInteger::type x = 0;
			for (long long i = 0; i < iterations; ++i)
			{
				x += add_contextual (s.bm, s.bm);
			}
			dump << t.name << x << std::endl;
		});
	t.stop ();
	std::cout << t;
}


__attribute__ ((noinline)) void benchmark_layer_activation (kdb::Environment & s)
{
	s.nested = value;
	s.context ().with<Layer2> () ([&] { s.nested = othervalue; });

	static Timer t ("layer activation");
	t.start ();
	for (long long i = 0; i < iterations / 2; ++i)
	{
		s.context ().activate<Layer2> ();
		s.context ().activate<Layer1> ();
	}
	t.stop ();
	std::cout << t;
}

__attribute__ ((noinline)) void benchmark_layer_activationval (kdb::Environment & s)
{
	s.nested = value;
	s.context ().with<Layer2> () ([&] { s.nested = othervalue; });
	kdb::ThreadInteger::type x = 0;

	static Timer t ("layer activationval");
	t.start ();
	for (long long i = 0; i < iterations / 2; ++i)
	{
		s.context ().activate<Layer2> ();
		x ^= add_contextual (s.nested, s.nested);
		s.context ().activate<Layer1> ();
		x ^= add_contextual (s.nested, s.nested);
	}
	t.stop ();
	std::cout << t;
	dump << "Layer ActivationVal " << x << std::endl;
}

__attribute__ ((noinline)) void benchmark_layer_withN (kdb::Environment &, long long N)
{
	kdb::Coordinator c;
	kdb::ThreadContext tc (c);
	kdb::KeySet ks;
	kdb::ThreadInteger ti (ks, tc, kdb::Key ("/test/nolayer", KEY_META, "default", s_value, KEY_END));
	ti = 5;
	kdb::ThreadInteger::type x = ti;

	// N additional integers producing overhead
	std::vector<std::shared_ptr<kdb::ThreadInteger>> vi;
	for (long long i = 0; i < N; ++i)
	{
		std::ostringstream os;
		os << "/test/%layer1%/";
		os << i;
		// std::cout << os.str().c_str() << std::endl;
		vi.push_back (std::make_shared<kdb::ThreadInteger> (ks, tc,
								    kdb::Key (os.str ().c_str (), KEY_META, "default", s_value, KEY_END)));
	}

	static Timer * ts[10]{ new Timer ("layer with0"), new Timer ("layer with1"), new Timer ("layer with2"), new Timer ("layer with3"),
			       new Timer ("layer with4"), new Timer ("layer with5"), new Timer ("layer with6"), new Timer ("layer with7"),
			       new Timer ("layer with8"), new Timer ("layer with9") };
	Timer & t = *ts[N];
	t.start ();
	for (long long i = 0; i < iterations / 2; ++i)
	{
		tc.with<Layer1> () ([&] { x ^= add_contextual (ti, ti); });
		x ^= add_contextual (ti, ti);
	}
	t.stop ();
	std::cout << t;
	dump << t.name << x << std::endl;
}


__attribute__ ((noinline)) void benchmark_layer_switch (kdb::Environment & s)
{
	s.nested = value;
	kdb::ThreadInteger::type x = 0;

	static Timer t ("layer switch");
	t.start ();
	//{benchmark/layerswitch}
	for (long long i = 0; i < iterations / 2; ++i)
	{
		s.context ().activate<Layer1> ();
		x ^= add_contextual (s.nested, s.nested);
		s.context ().deactivate<Layer1> ();
		x ^= add_contextual (s.nested, s.nested);
	}
	//{end}
	t.stop ();
	std::cout << t;
	dump << "layer switch " << x << std::endl;
}


__attribute__ ((noinline)) void benchmark_layer_switchN (kdb::Environment &, long long N)
{
	kdb::Coordinator c;
	kdb::ThreadContext tc (c);
	kdb::KeySet ks;
	kdb::ThreadInteger ti (ks, tc, kdb::Key ("/test/nolayer", KEY_META, "default", s_value, KEY_END));
	ti = 5;
	kdb::ThreadInteger::type x = ti;

	// N additional integers producing overhead
	std::vector<std::shared_ptr<kdb::ThreadInteger>> vi;
	for (long long i = 0; i < N; ++i)
	{
		std::ostringstream os;
		os << "/test/%layer1%/";
		os << i;
		// std::cout << os.str().c_str() << std::endl;
		vi.push_back (std::make_shared<kdb::ThreadInteger> (ks, tc,
								    kdb::Key (os.str ().c_str (), KEY_META, "default", s_value, KEY_END)));
	}

	static Timer * ts[10]{ new Timer ("layer switch0"), new Timer ("layer switch1"), new Timer ("layer switch2"),
			       new Timer ("layer switch3"), new Timer ("layer switch4"), new Timer ("layer switch5"),
			       new Timer ("layer switch6"), new Timer ("layer switch7"), new Timer ("layer switch8"),
			       new Timer ("layer switch9") };
	Timer & t = *ts[N];
	t.start ();
	for (long long i = 0; i < iterations / 2; ++i)
	{
		tc.activate<Layer1> ();
		x ^= add_contextual (ti, ti);
		tc.deactivate<Layer1> ();
		x ^= add_contextual (ti, ti);
	}
	t.stop ();
	std::cout << t;
	dump << t.name << x << std::endl;
}


__attribute__ ((noinline)) void benchmark_layer_sync (kdb::Environment & s)
{
	s.nested = value;
	kdb::ThreadInteger::type x = 0;

	static Timer t ("sync layers");
	t.start ();
	//{benchmark/layerswitch}
	for (long long i = 0; i < iterations; ++i)
	{
		s.context ().syncLayers ();
	}
	//{end}
	t.stop ();
	std::cout << t;
	dump << "layer switch " << x << std::endl;
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
	kdb::Coordinator gc;
	kdb::ThreadContext c (gc);
	kdb::KeySet ks;
	kdb::Environment s (ks, c);
	computer_info ();
	s.nested = 20;
	s.bm = 20;
	std::cout << std::endl;

	if (argc == 2)
	{
		iterations = atoll (argv[1]);
		iterations1 = iterations / 100;
	}

	for (int i = 0; i < benchmarkIterations; ++i)
	{
		std::cout << i << std::endl;

		// if:
		/*
		benchmark_nothing();
		benchmark_native();
		benchmark_native_sum();
		benchmark_native_sum_asm();
		benchmark_contextual_noif(s);
		benchmark_contextual_noif_sum(s);
		benchmark_contextual_noif_sum_asm(s); // same as xor


		// 1-9 layer activations outside a loop
		benchmark_layer_with0out(s);
		benchmark_layer_with1out(s);
		benchmark_layer_with2out(s);
		benchmark_layer_with3out(s);
		benchmark_layer_with4out(s);
		benchmark_layer_with5out(s);
		benchmark_layer_with6out(s);
		benchmark_layer_with7out(s);
		benchmark_layer_with8out(s);
		benchmark_layer_with9out(s);
		*/

		// 1-9 values with layer activations in a loop
		/*
		benchmark_layer_with1val(s);
		benchmark_layer_with2val(s);
		benchmark_layer_with3val(s);
		benchmark_layer_with4val(s);
		benchmark_layer_with5val(s);
		benchmark_layer_with6val(s);
		benchmark_layer_with7val(s);
		benchmark_layer_with8val(s);
		benchmark_layer_with9val(s);

		benchmark_layer_with1act(s);
		benchmark_layer_with2act(s);

		// obsolete benchmarks not needed for paper:
		benchmark_layer_withnoalloc9(s); // not faster
		benchmark_layer_with9outsum(s); // 30 us
		benchmark_contextual_wrongop(s);
		benchmark_layer_activation(s);
		benchmark_layer_activationval(s);

		benchmark_layer_with1outin(s);
		benchmark_layer_with1valout(s);
		*/
		benchmark_evaluate_no_sub (s);

		benchmark_layer_sync (s);
		benchmark_layer_with (s);
		benchmark_layer_withx (s);
		for (int j = 0; j < 10; ++j)
		{
			benchmark_layer_withN (s, j);
		}
		benchmark_layer_switch (s);
		for (int j = 0; j < 10; ++j)
		{
			benchmark_layer_switchN (s, j);
		}

		benchmark_evaluate (s);
		benchmark_evaluate0 (s);
		benchmark_evaluate1 (s);
		benchmark_evaluate2 (s);
		benchmark_evaluate3 (s);
		benchmark_evaluate4 (s);
		benchmark_evaluate5 (s);

		benchmark_kslookup ();

		benchmark_hashmap ();
		benchmark_hashmap_find ();
	}

	data << "value,benchmark" << std::endl;
}
