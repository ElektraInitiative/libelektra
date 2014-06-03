#include <contextual.hpp>

#include <thread>

#include <tests.hpp>

const uint32_t value = 55;
const char * s_value = "55";
const uint32_t othervalue = 66;
const char * s_othervalue = "66";

class CountryGermanyLayer : public kdb::Layer
{
public:
	std::string id() const override
	{
		return "country";
	}
	std::string operator()() const override
	{
		return "germany";
	}
};

class LanguageGermanLayer : public kdb::Layer
{
public:
	std::string id() const override
	{
		return "language";
	}
	std::string operator()() const override
	{
		return "german";
	}
};

class CountryGPSLayer : public kdb::Layer
{
public:
	CountryGPSLayer() : m_country("austria") {}
	string id() const { return "country"; }
	string operator()() const { return m_country; }
private:
	std::string m_country;
};

class ThreadLayer : public kdb::Layer
{
public:
	ThreadLayer() {}
    string id() const { return "thread"; }
    string operator()() const {
	    ostringstream os;
		thread::id tid = this_thread::get_id();
	    if (tid != g_main_id) { os << tid; }
	    return os.str(); };
private:
	static const thread::id g_main_id;
};

const std::thread::id ThreadLayer::g_main_id = std::this_thread::get_id();

class CountingLayer : public kdb::Layer
{
public:
	CountingLayer() : m_id() {}
	string id() const { return "counting"; }
	string operator()() const {
		ostringstream os;
		os << m_id++;
		return os.str();
	};
private:
	mutable long long m_id;
};


class SelectedPrinterLayer : public kdb::Layer
{
public:
	std::string id() const override
	{
		return "printer";
	}
	std::string operator()() const override
	{
		return "Laserdrucker";
	}
};

class MainApplicationLayer : public kdb::Layer
{
public:
	std::string id() const override
	{ return "application"; }
	std::string operator()() const override
	{ return "main"; }
};

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

class ProfileLayer : public kdb::Layer
{
public:
	ProfileLayer(String const & profile) :
		m_profile(profile) {}
	string id() const { return "profile"; }
	string operator()() const { return m_profile; }
private:
	String const & m_profile;
};


void test_integer()
{
	std::cout << "Testing integer" << std::endl;

	using namespace kdb;
	KeySet ks;
	Context c;
	Integer i(ks, c, Key("", KEY_VALUE, "/%language%/%country%/%dialect%/test",
						 KEY_META, "default", s_value, KEY_END));
	assert(i == value);
	i = 5;
	assert(i == 5);
	assert(i.getEvaluatedName() == "/%/%/%/test");
	assert(!ks.lookup("/%/%/%/test"));
	i.syncKeySet();
	assert(ks.lookup("/%/%/%/test").getString() == "5");
	i = 10;
	assert(i == 10);
	assert(ks.lookup("/%/%/%/test").getString() == "5");

	c.activate<LanguageGermanLayer>();
	assert(i == value);
//{debug/assert}
assert(i.context()["language"] == "german");
assert(i.getEvaluatedName() == "/german/%/%/test");
//{end}
	assert(ks.lookup("/%/%/%/test").getString() == "10");
	i = 15;
	assert(i == 15);
	assert(ks.lookup("/%/%/%/test").getString() == "10");
	assert(!ks.lookup("/german/%/%/test"));
	i.syncKeySet();
	assert(ks.lookup("/german/%/%/test").getString() == "15");

	c.deactivate<LanguageGermanLayer>();
	assert(i == 10);
	assert(ks.lookup("/%/%/%/test").getString() == "10");
	assert(ks.lookup("/german/%/%/test").getString() == "15");

	c.with<LanguageGermanLayer>()([&]()
	{
		assert(i == 15);
		assert(ks.lookup("/%/%/%/test").getString() == "10");
		assert(ks.lookup("/german/%/%/test").getString() == "15");
		c.without<LanguageGermanLayer>()([&]()
		{
			assert(i == 10);
			assert(ks.lookup("/%/%/%/test").getString() == "10");
			assert(ks.lookup("/german/%/%/test").getString() == "15");
		});
		assert(i == 15);
		assert(ks.lookup("/%/%/%/test").getString() == "10");
		assert(ks.lookup("/german/%/%/test").getString() == "15");
	});
	assert(i == 10);
	assert(ks.lookup("/%/%/%/test").getString() == "10");
	assert(ks.lookup("/german/%/%/test").getString() == "15");

	c.with<LanguageGermanLayer>()
	 .with<CountryGermanyLayer>()([&]()
	{
		assert(i == value);
		assert(ks.lookup("/%/%/%/test").getString() == "10");
		assert(ks.lookup("/german/%/%/test").getString() == "15");
		i = 20;
		assert(i.getEvaluatedName() == "/german/germany/%/test");
/*
//{debug/backtrace}
#3  0x0000000000407a56 in operator() at first.cpp:1521
        i = @0x7fffe36b69a0: { ...
          m_evaluated_name = "/german/germany/%/test" }
//{end}
//{debug/breakpoint}
break 1520 if i.getEvaluatedName()
              .compare("/german/germany/%/test") == 0
//{end}
*/
		assert(i == 20);
		assert(!ks.lookup("/german/germany/%/test"));
	});
	assert(i == 10);
	assert(ks.lookup("/%/%/%/test").getString() == "10");
	assert(ks.lookup("/german/%/%/test").getString() == "15");
	assert(ks.lookup("/german/germany/%/test").getString() == "20");

	c.with<LanguageGermanLayer>().with<CountryGermanyLayer>()([&]()
	{
		assert(i == 20);
		assert(ks.lookup("/%/%/%/test").getString() == "10");
		assert(ks.lookup("/german/%/%/test").getString() == "15");
		i = 30;
		assert(i == 30);
		assert(ks.lookup("/german/germany/%/test").getString() == "20");
	});
	assert(i == 10);
	assert(ks.lookup("/%/%/%/test").getString() == "10");
	assert(ks.lookup("/german/%/%/test").getString() == "15");
	assert(ks.lookup("/german/germany/%/test").getString() == "30");

	c.with<LanguageGermanLayer>()
	 .with<CountryGermanyLayer>()([&]()
	{
		assert(i == 30);
		assert(ks.lookup("/%/%/%/test").getString() == "10");
		assert(ks.lookup("/german/%/%/test").getString() == "15");
		assert(ks.lookup("/german/germany/%/test").getString() == "30");
		c.with<CountryGPSLayer>()([&]()
		{
			assert(i == value);
		});
		assert(ks.lookup("/german/austria/%/test").getString() == s_value);
	});
	assert(i == 10);
	assert(ks.lookup("/%/%/%/test").getString() == "10");
	assert(ks.lookup("/german/%/%/test").getString() == "15");
}


void test_counting()
{
	std::cout << "Testing counting" << std::endl;

	std::shared_ptr<Layer> l = std::make_shared<CountingLayer>();
	KeySet ks;
	Context c;
	c.with<CountingLayer>()([&]
	{
		// std::cout << "c " << c["counting"] << std::endl;
		assert(c["counting"] == "0");
	});
	// is it a specification error to have counting
	// two times?
	Integer i(ks, c, Key("", KEY_VALUE, "/%counting%/%counting%",
						 KEY_META, "default", s_value, KEY_END));

	assert((*l)() == "0");
	assert((*l)() == "1");
	c.withl(l, [&]
	{
		assert(c["counting"] == "4");
		assert(c["counting"] == "5");
	});
	assert((*l)() == "6");
	c.with<CountingLayer>()([&]
	{
		assert(c["counting"] == "2");
		assert(c["counting"] == "3");
	});
	assert(c["counting"].empty());
	c.with<CountingLayer>()([&]
	{
		assert(c["counting"] == "2");
		assert(c["counting"] == "3");
	});
	assert(c["counting"].empty());
	c.activate<CountingLayer>();
	assert(c["counting"] == "2");
	assert(c["counting"] == "3");
	c.deactivate<CountingLayer>();
	assert(c["counting"].empty());
}

void test_groups()
{
	std::cout << "Testing groups" << std::endl;

	KeySet ks;
	Context c;

	Integer i(ks, c, Key("", KEY_VALUE,
	"/%application%/%version profile thread module%/%manufacturer type family model%/serial_number",
	KEY_META, "default", s_value, KEY_END));
	assert(i.getEvaluatedName() == "/%/%/%/serial_number");
	c.activate<MainApplicationLayer>();
	String s(ks, c, Key("", KEY_VALUE, "/%x%", KEY_META, "default", "anonymous", KEY_END));
	c.activate<ProfileLayer>(s);
	assert(i.getEvaluatedName() == "/main/%/%/serial_number");
	c.activate<KeyValueLayer>("version", "1");
	assert(i.getEvaluatedName() == "/main/%1%anonymous/%/serial_number");
	c.activate<KeyValueLayer>("module", "M1");
	assert(i.getEvaluatedName() == "/main/%1%anonymous/%/serial_number");
	c.activate<KeyValueLayer>("manufacturer", "hp");
	assert(i.getEvaluatedName() == "/main/%1%anonymous/%hp/serial_number");
	c.activate<KeyValueLayer>("family", "EliteBook");
	assert(i.getEvaluatedName() == "/main/%1%anonymous/%hp/serial_number");
	c.activate<KeyValueLayer>("type", "MobileWorkstation");
	assert(i.getEvaluatedName() == "/main/%1%anonymous/%hp%MobileWorkstation%EliteBook/serial_number");
	c.activate<KeyValueLayer>("model", "8570");
	assert(i.getEvaluatedName() == "/main/%1%anonymous/%hp%MobileWorkstation%EliteBook%8570/serial_number");
	c.activate<KeyValueLayer>("thread", "40");
	assert(i.getEvaluatedName() == "/main/%1%anonymous%40%M1/%hp%MobileWorkstation%EliteBook%8570/serial_number");
	c.deactivate<KeyValueLayer>("version", "");
	assert(i.getEvaluatedName() == "/main/%/%hp%MobileWorkstation%EliteBook%8570/serial_number");
	c.activate<KeyValueLayer>("version", "4");
	assert(i.getEvaluatedName() == "/main/%4%anonymous%40%M1/%hp%MobileWorkstation%EliteBook%8570/serial_number");
	c.deactivate<KeyValueLayer>("manufacturer", "");
	assert(i.getEvaluatedName() == "/main/%4%anonymous%40%M1/%/serial_number");
	c.activate<KeyValueLayer>("manufacturer", "HP");
	assert(i.getEvaluatedName() == "/main/%4%anonymous%40%M1/%HP%MobileWorkstation%EliteBook%8570/serial_number");
	c.deactivate<KeyValueLayer>("type", "");
	assert(i.getEvaluatedName() == "/main/%4%anonymous%40%M1/%HP/serial_number");
	c.with<KeyValueLayer>("type", "Notebook")([&]
	{
		assert(i.getEvaluatedName() == "/main/%4%anonymous%40%M1/%HP%Notebook%EliteBook%8570/serial_number");
		c.without<KeyValueLayer>("type", "")([&]
		{
			assert(i.getEvaluatedName() == "/main/%4%anonymous%40%M1/%HP/serial_number");
		});
	});
}


void test_integer_copy()
{
	std::cout << "Testing integer copy" << std::endl;

	using namespace kdb;
	KeySet ks;
	Context c;
	Integer i(ks, c, Key("", KEY_VALUE, "/%language%/%country%/%dialect%/test",
						 KEY_META, "default", s_value, KEY_END));
	assert(i == value);
	i = 5;
	assert(i == 5);
	assert(i.getEvaluatedName() == "/%/%/%/test");
	assert(!ks.lookup("/%/%/%/test"));
	i.syncKeySet();
	assert(ks.lookup("/%/%/%/test").getString() == "5");

	KeySet ks2;
	Integer i2(i, ks2);
	assert(i2 == 5);
	i2 = 10;
	assert(i2 == 10);
	assert(i == 5);
	i2.syncKeySet();
	assert(ks.lookup("/%/%/%/test").getString() == "5");
	assert(ks2.lookup("/%/%/%/test").getString() == "10");
}

void test_evaluate()
{
	std::cout << "Testing evaluate" << std::endl;

	kdb::Context c;
	assert (c["language"] == "");
	assert (c["country"] == "");
	assert (c["dialect"] == "");
	assert(c.evaluate("/%language%/%country%/%dialect%/test") == "/%/%/%/test");

	c.activate<LanguageGermanLayer>();
	assert (c["language"] == "german");
	assert (c["country"] == "");
	assert (c["dialect"] == "");
	assert(c.evaluate("/%language%/%country%/%dialect%/test") == "/german/%/%/test");

	c.deactivate<LanguageGermanLayer>();
	assert (c["language"] == "");
	assert (c["country"] == "");
	assert (c["dialect"] == "");
	assert(c.evaluate("/%language%/%country%/%dialect%/test") == "/%/%/%/test");

	c.with<LanguageGermanLayer>()([&]()
	{
		assert (c["language"] == "german");
		assert (c["country"] == "");
		assert (c["dialect"] == "");
		assert(c.evaluate("/%language%/%country%/%dialect%/test") == "/german/%/%/test");
		c.without<LanguageGermanLayer>()([&]()
		{
			assert (c["language"] == "");
			assert (c["country"] == "");
			assert (c["dialect"] == "");
			assert(c.evaluate("/%language%/%country%/%dialect%/test") == "/%/%/%/test");
		});
		assert (c["language"] == "german");
		assert (c["country"] == "");
		assert (c["dialect"] == "");
		assert(c.evaluate("/%language%/%country%/%dialect%/test") == "/german/%/%/test");
	});
	assert (c["language"] == "");
	assert (c["country"] == "");
	assert (c["dialect"] == "");
	assert(c.evaluate("/%language%/%country%/%dialect%/test") == "/%/%/%/test");

	c.with<LanguageGermanLayer>()
	 .with<CountryGermanyLayer>()([&]()
	{
		assert (c["language"] == "german");
		assert (c["country"] == "germany");
		assert (c["dialect"] == "");
		assert(c.evaluate("/%language%/%country%/%dialect%/test") == "/german/germany/%/test");
		c.with<CountryGPSLayer>()([&]()
		{
			assert (c["language"] == "german");
			assert (c["country"] == "austria");
			assert (c["dialect"] == "");
			assert(c.evaluate("/%language%/%country%/%dialect%/test") == "/german/austria/%/test");
			c.without<CountryGPSLayer>()([&]()
			{
				assert (c["language"] == "german");
				assert (c["country"] == "");
				assert (c["dialect"] == "");
				assert(c.evaluate("/%language%/%country%/%dialect%/test") == "/german/%/%/test");
			});
		});
		assert (c["language"] == "german");
		assert (c["country"] == "germany");
		assert (c["dialect"] == "");
		assert(c.evaluate("/%language%/%country%/%dialect%/test") == "/german/germany/%/test");
	});
	assert (c["language"] == "");
	assert (c["country"] == "");
	assert (c["dialect"] == "");
	assert(c.evaluate("/%language%/%country%/%dialect%/test") == "/%/%/%/test");

	c.with<LanguageGermanLayer>().with<CountryGermanyLayer>()([&]
	{
		assert (c["language"] == "german");
		assert (c["country"] == "germany");
		assert (c["dialect"] == "");
		assert(c.evaluate("/%language%/%country%/%dialect%/test") == "/german/germany/%/test");
	});
	assert (c["language"] == "");
	assert (c["country"] == "");
	assert (c["dialect"] == "");
	assert(c.evaluate("/%language%/%country%/%dialect%/test") == "/%/%/%/test");

	KeySet ks;
	Integer i(ks, c, Key("", KEY_VALUE,
	"/%application%/%version%/%profile%/%thread%/%module%/%manufacturer%/%type%/%family%/%model%/serial_number",
	KEY_META, "default", s_value, KEY_END));
	assert(i.getEvaluatedName() == "/%/%/%/%/%/%/%/%/%/serial_number");
	c.activate<MainApplicationLayer>();
	assert(i.getEvaluatedName() == "/main/%/%/%/%/%/%/%/%/serial_number");
	String s(ks, c, Key("", KEY_VALUE, "/%x%", KEY_META, "default", "anonymous", KEY_END));
	c.activate<ProfileLayer>(s);
	assert(i.getEvaluatedName() == "/main/%/anonymous/%/%/%/%/%/%/serial_number");
	c.activate<KeyValueLayer>("module", "M1");
	assert(i.getEvaluatedName() == "/main/%/anonymous/%/M1/%/%/%/%/serial_number");
	c.activate<KeyValueLayer>("manufacturer", "hp");
	assert(i.getEvaluatedName() == "/main/%/anonymous/%/M1/hp/%/%/%/serial_number");
	c.activate<KeyValueLayer>("family", "EliteBook");
	assert(i.getEvaluatedName() == "/main/%/anonymous/%/M1/hp/%/EliteBook/%/serial_number");
}


struct MockObserver : kdb::Observer
{
	MockObserver() : counter()
	{}

	virtual void update() const
	{
		++ counter;
	}

	mutable long long counter;
};

// duplicates need to be filtered
void test_observer()
{
	std::cout << "Testing observer" << std::endl;

	kdb::Context c;
	MockObserver o1;
	MockObserver o2;
	MockObserver o3;
	c.attachByName("/%event1%/%event2%", o1);
	c.attachByName("/%event1%/%event3%", o2);
	c.attachByName("/%eventX%", o3);
	c.attachByName("/%eventX%", o3);
	assert(o1.counter == 0);
	assert(o2.counter == 0);
	c.notify({"event1"});
	assert(o1.counter == 1);
	assert(o2.counter == 1);
	c.notify({"event2"});
	assert(o1.counter == 2);
	assert(o2.counter == 1);
	c.notify({"event3"});
	assert(o1.counter == 2);
	assert(o2.counter == 2);
	c.notify({"event4"});
	assert(o1.counter == 2);
	assert(o2.counter == 2);
	c.notify({"event1", "event2"});
	assert(o1.counter == 3);
	assert(o2.counter == 3);
	c.notify({"event1", "event3"});
	assert(o1.counter == 4);
	assert(o2.counter == 4);
	assert(o3.counter == 0);
	c.notify();
	assert(o1.counter == 5);
	assert(o2.counter == 5);
	assert(o3.counter == 1);
}



bool fooFirst = true;

//{foo}
void foo(kdb::Integer & e)
{
	e.context().with<SelectedPrinterLayer>()([&]()
	{
		if (fooFirst) assert(e == value);
		else assert(e == 20);
		e = 20;
		assert(e == 20);
	});
	assert (e == 12);
	fooFirst = false;
}
//{end}

bool barFirst = true;

void bar(kdb::Integer const & e)
{
	e.context().with<ThreadLayer>()
		.with<SelectedPrinterLayer>()([&]()
	{
		if (barFirst) assert(e == 20);
		else assert(e == value);

		e.context().without<ThreadLayer>()([&]()
		{
			assert(e == 20);
		});
	});
	assert (e == 12);
	barFirst = false;
}

void test_threads()
{
	using namespace kdb;

	std::cout << "Testing threads" << std::endl;

	KeySet ks;
	Context c;
	kdb::Integer n(ks, c,  Key("", KEY_VALUE, "/%thread%/%printer%/test",
						 KEY_META, "default", s_value, KEY_END));


	n.context().activate<MainApplicationLayer>();
	assert(n == value);

	n = 18;
	assert(n == 18);

	Integer & d = n;

	d = value;
	assert(d == value);

	d = 12;
	assert(d == 12);

	foo(d);
	assert(d == 12);

	foo(d);
	assert(d == 12);

	bar(d);
	assert(d == 12);

	std::thread t1(bar, std::cref(d));
	t1.join();
	assert(d == 12);

	std::thread t2(foo, std::ref(d));
	t2.join();
	assert(d == 12);
}



int main()
{
	cout << "CONTEXTUAL TESTS" << endl;
	cout << "===============" << endl << endl;

	test_evaluate();
	test_counting();
	test_groups();
	test_integer();
	test_integer_copy();
	test_observer();
	test_threads();

	cout << endl;
	cout << "testcpp_contextual RESULTS: " << nbTest << " test(s) done. " << nbError << " error(s)." << endl;
	return nbError;
}
