#include "kdbconfig.h"

#include <kdbvalue.hpp>
#include <kdbcontext.hpp>

#include <thread>

#include <gtest/gtest.h>

const uint32_t i_value = 55;
const char * s_value = "55";

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
	std::string id() const { return "country"; }
	std::string operator()() const { return m_country; }
private:
	std::string m_country;
};

class ThreadLayer : public kdb::Layer
{
public:
	ThreadLayer() {}
	std::string id() const { return "thread"; }
	std::string operator()() const {
		std::ostringstream os;
		std::thread::id tid = std::this_thread::get_id();
		if (tid != g_main_id) { os << tid; }
		return os.str(); };
private:
	static const std::thread::id g_main_id;
};

const std::thread::id ThreadLayer::g_main_id = std::this_thread::get_id();

class CountingLayer : public kdb::Layer
{
public:
	CountingLayer() : m_id() {}
	std::string id() const { return "counting"; }
	std::string operator()() const {
		std::ostringstream os;
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
	ProfileLayer(kdb::String const & profile) :
		m_profile(profile) {}
	std::string id() const { return "profile"; }
	std::string operator()() const { return m_profile; }
private:
	kdb::String const & m_profile;
};


TEST(test_contextual_basic, integer)
{
	using namespace kdb;
	KeySet ks;
	Context c;
	Integer i(ks, c, Key("/%language%/%country%/%dialect%/test",
			KEY_CASCADING_NAME,
			KEY_META, "default", s_value, KEY_END));
	ASSERT_EQ(i , i_value);
	i = 5;
	ASSERT_EQ(i , 5);
	ASSERT_EQ(i.getSpec().getName() , "/%/%/%/test");
	ASSERT_TRUE(!ks.lookup("/%/%/%/test"));
	i.syncKeySet();
	ASSERT_EQ(ks.lookup("/%/%/%/test").getString() , "5");
	i = 10;
	ASSERT_EQ(i , 10);
	ASSERT_EQ(ks.lookup("/%/%/%/test").getString() , "5");

	c.activate<LanguageGermanLayer>();
	ASSERT_EQ(i , i_value);
//{debug/ASSERT_TRUE}
ASSERT_EQ(i.context()["language"] , "german");
ASSERT_EQ(i.getSpec().getName() , "/german/%/%/test");
//{end}
	ASSERT_EQ(ks.lookup("/%/%/%/test").getString() , "10");
	i = 15;
	ASSERT_EQ(i , 15);
	ASSERT_EQ(ks.lookup("/%/%/%/test").getString() , "10");
	ASSERT_TRUE(!ks.lookup("/german/%/%/test"));
	i.syncKeySet();
	ASSERT_EQ(ks.lookup("/german/%/%/test").getString() , "15");

	c.deactivate<LanguageGermanLayer>();
	ASSERT_EQ(i , 10);
	ASSERT_EQ(ks.lookup("/%/%/%/test").getString() , "10");
	ASSERT_EQ(ks.lookup("/german/%/%/test").getString() , "15");

	c.with<LanguageGermanLayer>()([&]()
	{
		ASSERT_EQ(i , 15);
		ASSERT_EQ(ks.lookup("/%/%/%/test").getString() , "10");
		ASSERT_EQ(ks.lookup("/german/%/%/test").getString() , "15");
		c.without<LanguageGermanLayer>()([&]()
		{
			ASSERT_EQ(i , 10);
			ASSERT_EQ(ks.lookup("/%/%/%/test").getString() , "10");
			ASSERT_EQ(ks.lookup("/german/%/%/test").getString() , "15");
		});
		ASSERT_EQ(i , 15);
		ASSERT_EQ(ks.lookup("/%/%/%/test").getString() , "10");
		ASSERT_EQ(ks.lookup("/german/%/%/test").getString() , "15");
	});
	ASSERT_EQ(i , 10);
	ASSERT_EQ(ks.lookup("/%/%/%/test").getString() , "10");
	ASSERT_EQ(ks.lookup("/german/%/%/test").getString() , "15");

	c.with<LanguageGermanLayer>()
	 .with<CountryGermanyLayer>()([&]()
	{
		ASSERT_EQ(i , i_value);
		ASSERT_EQ(ks.lookup("/%/%/%/test").getString() , "10");
		ASSERT_EQ(ks.lookup("/german/%/%/test").getString() , "15");
		i = 20;
		ASSERT_EQ(i.getSpec().getName() , "/german/germany/%/test");
/*
//{debug/backtrace}
#3  0x0000000000407a56 in operator() at first.cpp:1521
        i = @0x7fffe36b69a0: { ...
          m_evaluated_name = "/german/germany/%/test" }
//{end}
//{debug/breakpoint}
break 1520 if i.getSpec().getName()
              .compare("/german/germany/%/test") == 0
//{end}
*/
		ASSERT_EQ(i , 20);
		ASSERT_TRUE(!ks.lookup("/german/germany/%/test"));
	});
	ASSERT_EQ(i , 10);
	ASSERT_EQ(ks.lookup("/%/%/%/test").getString() , "10");
	ASSERT_EQ(ks.lookup("/german/%/%/test").getString() , "15");
	ASSERT_EQ(ks.lookup("/german/germany/%/test").getString() , "20");

	c.with<LanguageGermanLayer>().with<CountryGermanyLayer>()([&]()
	{
		ASSERT_EQ(i , 20);
		ASSERT_EQ(ks.lookup("/%/%/%/test").getString() , "10");
		ASSERT_EQ(ks.lookup("/german/%/%/test").getString() , "15");
		i = 30;
		ASSERT_EQ(i , 30);
		ASSERT_EQ(ks.lookup("/german/germany/%/test").getString() , "20");
	});
	ASSERT_EQ(i , 10);
	ASSERT_EQ(ks.lookup("/%/%/%/test").getString() , "10");
	ASSERT_EQ(ks.lookup("/german/%/%/test").getString() , "15");
	ASSERT_EQ(ks.lookup("/german/germany/%/test").getString() , "30");

	c.with<LanguageGermanLayer>()
	 .with<CountryGermanyLayer>()([&]()
	{
		ASSERT_EQ(i , 30);
		ASSERT_EQ(ks.lookup("/%/%/%/test").getString() , "10");
		ASSERT_EQ(ks.lookup("/german/%/%/test").getString() , "15");
		ASSERT_EQ(ks.lookup("/german/germany/%/test").getString() , "30");
		c.with<CountryGPSLayer>()([&]()
		{
			ASSERT_EQ(i , i_value);
		});
		ASSERT_EQ(ks.lookup("/german/austria/%/test").getString() , s_value);
	});
	ASSERT_EQ(i , 10);
	ASSERT_EQ(ks.lookup("/%/%/%/test").getString() , "10");
	ASSERT_EQ(ks.lookup("/german/%/%/test").getString() , "15");
}


TEST(test_contextual_basic, counting)
{
	using namespace kdb;

	std::shared_ptr<kdb::Layer> l = std::make_shared<CountingLayer>();
	KeySet ks;
	Context c;
	c.with<CountingLayer>()([&]
	{
		ASSERT_EQ(c["counting"] , "0");
	});
	// is it a specification error to have counting
	// two times?
	Integer i(ks, c, Key("/%counting%/%counting%",
				KEY_CASCADING_NAME,
				KEY_META, "default", s_value, KEY_END));

	ASSERT_EQ((*l)() , "0");
	ASSERT_EQ((*l)() , "1");
	c.withl(l, [&]
	{
		ASSERT_EQ(c["counting"] , "4");
		ASSERT_EQ(c["counting"] , "5");
	});
	ASSERT_EQ((*l)() , "6");
	c.with<CountingLayer>()([&]
	{
		ASSERT_EQ(c["counting"] , "2");
		ASSERT_EQ(c["counting"] , "3");
	});
	ASSERT_TRUE(c["counting"].empty());
	c.with<CountingLayer>()([&]
	{
		ASSERT_EQ(c["counting"] , "2");
		ASSERT_EQ(c["counting"] , "3");
	});
	ASSERT_TRUE(c["counting"].empty());
	c.activate<CountingLayer>();
	ASSERT_EQ(c["counting"] , "2");
	ASSERT_EQ(c["counting"] , "3");
	c.deactivate<CountingLayer>();
	ASSERT_TRUE(c["counting"].empty());
}

TEST(test_contextual_basic, groups)
{
	using namespace kdb;

	KeySet ks;
	Context c;

	Integer i(ks, c, Key(
	"/%application%/%version profile thread module%/%manufacturer type family model%/serial_number",
	KEY_CASCADING_NAME,
	KEY_META, "default", s_value, KEY_END));
	ASSERT_EQ(i.getSpec().getName() , "/%/%/%/serial_number");
	c.activate<MainApplicationLayer>();
	String s(ks, c, Key("/%x%",
		KEY_CASCADING_NAME,
		KEY_META, "default", "anonymous", KEY_END));
	c.activate<ProfileLayer>(s);
	ASSERT_EQ(i.getSpec().getName() , "/main/%/%/serial_number");
	c.activate<KeyValueLayer>("version", "1");
	ASSERT_EQ(i.getSpec().getName() , "/main/%1%anonymous/%/serial_number");
	c.activate<KeyValueLayer>("module", "M1");
	ASSERT_EQ(i.getSpec().getName() , "/main/%1%anonymous/%/serial_number");
	c.activate<KeyValueLayer>("manufacturer", "hp");
	ASSERT_EQ(i.getSpec().getName() , "/main/%1%anonymous/%hp/serial_number");
	c.activate<KeyValueLayer>("family", "EliteBook");
	ASSERT_EQ(i.getSpec().getName() , "/main/%1%anonymous/%hp/serial_number");
	c.activate<KeyValueLayer>("type", "MobileWorkstation");
	ASSERT_EQ(i.getSpec().getName() , "/main/%1%anonymous/%hp%MobileWorkstation%EliteBook/serial_number");
	c.activate<KeyValueLayer>("model", "8570");
	ASSERT_EQ(i.getSpec().getName() , "/main/%1%anonymous/%hp%MobileWorkstation%EliteBook%8570/serial_number");
	c.activate<KeyValueLayer>("thread", "40");
	ASSERT_EQ(i.getSpec().getName() , "/main/%1%anonymous%40%M1/%hp%MobileWorkstation%EliteBook%8570/serial_number");
	c.deactivate<KeyValueLayer>("version", "");
	ASSERT_EQ(i.getSpec().getName() , "/main/%/%hp%MobileWorkstation%EliteBook%8570/serial_number");
	c.activate<KeyValueLayer>("version", "4");
	ASSERT_EQ(i.getSpec().getName() , "/main/%4%anonymous%40%M1/%hp%MobileWorkstation%EliteBook%8570/serial_number");
	c.deactivate<KeyValueLayer>("manufacturer", "");
	ASSERT_EQ(i.getSpec().getName() , "/main/%4%anonymous%40%M1/%/serial_number");
	c.activate<KeyValueLayer>("manufacturer", "HP");
	ASSERT_EQ(i.getSpec().getName() , "/main/%4%anonymous%40%M1/%HP%MobileWorkstation%EliteBook%8570/serial_number");
	c.deactivate<KeyValueLayer>("type", "");
	ASSERT_EQ(i.getSpec().getName() , "/main/%4%anonymous%40%M1/%HP/serial_number");
	c.with<KeyValueLayer>("type", "Notebook")([&]
	{
		ASSERT_EQ(i.getSpec().getName() , "/main/%4%anonymous%40%M1/%HP%Notebook%EliteBook%8570/serial_number");
		c.without<KeyValueLayer>("type", "")([&]
		{
			ASSERT_EQ(i.getSpec().getName() , "/main/%4%anonymous%40%M1/%HP/serial_number");
		});
	});
}


TEST(test_contextual_basic, integer_copy)
{
	using namespace kdb;
	KeySet ks;
	Context c;
	Integer i(ks, c, Key("/%language%/%country%/%dialect%/test",
		KEY_CASCADING_NAME,
		KEY_META, "default", s_value, KEY_END));
	ASSERT_EQ(i , i_value);
	i = 5;
	ASSERT_EQ(i , 5);
	ASSERT_EQ(i.getSpec().getName() , "/%/%/%/test");
	ASSERT_TRUE(!ks.lookup("/%/%/%/test"));
	i.syncKeySet();
	ASSERT_EQ(ks.lookup("/%/%/%/test").getString() , "5");

	/* TODO: cannot copy KeySet anymore? -> feature maybe not needed
	KeySet ks2;
	Integer i2(i, ks2);
	ASSERT_EQ(i2 , 5);
	i2 = 10;
	ASSERT_EQ(i2 , 10);
	ASSERT_EQ(i , 5);
	i2.syncKeySet();
	ASSERT_EQ(ks.lookup("/%/%/%/test").getString() , "5");
	ASSERT_EQ(ks2.lookup("/%/%/%/test").getString() , "10");
	*/
}

TEST(test_contextual_basic, evaluate)
{
	using namespace kdb;
	kdb::Context c;
	ASSERT_EQ(c["language"] , "");
	ASSERT_EQ(c["country"] , "");
	ASSERT_EQ(c["dialect"] , "");
	ASSERT_EQ(c.evaluate("/%language%/%country%/%dialect%/test") , "/%/%/%/test");

	c.activate<LanguageGermanLayer>();
	ASSERT_EQ(c["language"] , "german");
	ASSERT_EQ(c["country"] , "");
	ASSERT_EQ(c["dialect"] , "");
	ASSERT_EQ(c.evaluate("/%language%/%country%/%dialect%/test") , "/german/%/%/test");

	c.deactivate<LanguageGermanLayer>();
	ASSERT_EQ(c["language"] , "");
	ASSERT_EQ(c["country"] , "");
	ASSERT_EQ(c["dialect"] , "");
	ASSERT_EQ(c.evaluate("/%language%/%country%/%dialect%/test") , "/%/%/%/test");

	c.with<LanguageGermanLayer>()([&]()
	{
		ASSERT_EQ(c["language"] , "german");
		ASSERT_EQ(c["country"] , "");
		ASSERT_EQ(c["dialect"] , "");
		ASSERT_EQ(c.evaluate("/%language%/%country%/%dialect%/test") , "/german/%/%/test");
		c.without<LanguageGermanLayer>()([&]()
		{
			ASSERT_EQ(c["language"] , "");
			ASSERT_EQ(c["country"] , "");
			ASSERT_EQ(c["dialect"] , "");
			ASSERT_EQ(c.evaluate("/%language%/%country%/%dialect%/test") , "/%/%/%/test");
		});
		ASSERT_EQ(c["language"] , "german");
		ASSERT_EQ(c["country"] , "");
		ASSERT_EQ(c["dialect"] , "");
		ASSERT_EQ(c.evaluate("/%language%/%country%/%dialect%/test") , "/german/%/%/test");
	});
	ASSERT_EQ(c["language"] , "");
	ASSERT_EQ(c["country"] , "");
	ASSERT_EQ(c["dialect"] , "");
	ASSERT_EQ(c.evaluate("/%language%/%country%/%dialect%/test") , "/%/%/%/test");

	c.with<LanguageGermanLayer>()
	 .with<CountryGermanyLayer>()([&]()
	{
		ASSERT_EQ(c["language"] , "german");
		ASSERT_EQ(c["country"] , "germany");
		ASSERT_EQ(c["dialect"] , "");
		ASSERT_EQ(c.evaluate("/%language%/%country%/%dialect%/test") , "/german/germany/%/test");
		c.with<CountryGPSLayer>()([&]()
		{
			ASSERT_EQ(c["language"] , "german");
			ASSERT_EQ(c["country"] , "austria");
			ASSERT_EQ(c["dialect"] , "");
			ASSERT_EQ(c.evaluate("/%language%/%country%/%dialect%/test") , "/german/austria/%/test");
			c.without<CountryGPSLayer>()([&]()
			{
				ASSERT_EQ(c["language"] , "german");
				ASSERT_EQ(c["country"] , "");
				ASSERT_EQ(c["dialect"] , "");
				ASSERT_EQ(c.evaluate("/%language%/%country%/%dialect%/test") , "/german/%/%/test");
			});
		});
		ASSERT_EQ(c["language"] , "german");
		ASSERT_EQ(c["country"] , "germany");
		ASSERT_EQ(c["dialect"] , "");
		ASSERT_EQ(c.evaluate("/%language%/%country%/%dialect%/test") , "/german/germany/%/test");
	});
	ASSERT_EQ(c["language"] , "");
	ASSERT_EQ(c["country"] , "");
	ASSERT_EQ(c["dialect"] , "");
	ASSERT_EQ(c.evaluate("/%language%/%country%/%dialect%/test") , "/%/%/%/test");

	c.with<LanguageGermanLayer>().with<CountryGermanyLayer>()([&]
	{
		ASSERT_EQ(c["language"] , "german");
		ASSERT_EQ(c["country"] , "germany");
		ASSERT_EQ(c["dialect"] , "");
		ASSERT_EQ(c.evaluate("/%language%/%country%/%dialect%/test") , "/german/germany/%/test");
	});
	ASSERT_EQ(c["language"] , "");
	ASSERT_EQ(c["country"] , "");
	ASSERT_EQ(c["dialect"] , "");
	ASSERT_EQ(c.evaluate("/%language%/%country%/%dialect%/test") , "/%/%/%/test");

	KeySet ks;
	Integer i(ks, c, Key(
	"/%application%/%version%/%profile%/%thread%/%module%/%manufacturer%/%type%/%family%/%model%/serial_number",
	KEY_CASCADING_NAME,
	KEY_META, "default", s_value, KEY_END));
	ASSERT_EQ(i.getSpec().getName() , "/%/%/%/%/%/%/%/%/%/serial_number");
	c.activate<MainApplicationLayer>();
	ASSERT_EQ(i.getSpec().getName() , "/main/%/%/%/%/%/%/%/%/serial_number");
	String s(ks, c, Key("/%x%", 
		KEY_CASCADING_NAME,
		KEY_META, "default", "anonymous", KEY_END));
	c.activate<ProfileLayer>(s);
	ASSERT_EQ(i.getSpec().getName() , "/main/%/anonymous/%/%/%/%/%/%/serial_number");
	c.activate<KeyValueLayer>("module", "M1");
	ASSERT_EQ(i.getSpec().getName() , "/main/%/anonymous/%/M1/%/%/%/%/serial_number");
	c.activate<KeyValueLayer>("manufacturer", "hp");
	ASSERT_EQ(i.getSpec().getName() , "/main/%/anonymous/%/M1/hp/%/%/%/serial_number");
	c.activate<KeyValueLayer>("family", "EliteBook");
	ASSERT_EQ(i.getSpec().getName() , "/main/%/anonymous/%/M1/hp/%/EliteBook/%/serial_number");
}


struct MockObserver : kdb::ValueObserver
{
	MockObserver() : counter()
	{}

	virtual void updateContext() const
	{
		++ counter;
	}

	mutable long long counter;
};

// duplicates need to be filtered
TEST(test_contextual_basic, valueObserver)
{
	kdb::Context c;
	MockObserver o1;
	MockObserver o2;
	MockObserver o3;
	c.attachByName("/%event1%/%event2%", o1);
	c.attachByName("/%event1%/%event3%", o2);
	c.attachByName("/%eventX%", o3);
	c.attachByName("/%eventX%", o3);
	ASSERT_EQ(o1.counter , 0);
	ASSERT_EQ(o2.counter , 0);
	c.notifyByEvents({"event1"});
	ASSERT_EQ(o1.counter , 1);
	ASSERT_EQ(o2.counter , 1);
	c.notifyByEvents({"event2"});
	ASSERT_EQ(o1.counter , 2);
	ASSERT_EQ(o2.counter , 1);
	c.notifyByEvents({"event3"});
	ASSERT_EQ(o1.counter , 2);
	ASSERT_EQ(o2.counter , 2);
	c.notifyByEvents({"event4"});
	ASSERT_EQ(o1.counter , 2);
	ASSERT_EQ(o2.counter , 2);
	c.notifyByEvents({"event1", "event2"});
	ASSERT_EQ(o1.counter , 3);
	ASSERT_EQ(o2.counter , 3);
	c.notifyByEvents({"event1", "event3"});
	ASSERT_EQ(o1.counter , 4);
	ASSERT_EQ(o2.counter , 4);
	ASSERT_EQ(o3.counter , 0);
	c.notifyAllEvents();
	ASSERT_EQ(o1.counter , 5);
	ASSERT_EQ(o2.counter , 5);
	ASSERT_EQ(o3.counter , 1);
}



bool fooFirst = true;

//{foo}
void foo(kdb::Integer & e)
{
	e.context().with<SelectedPrinterLayer>()([&]()
	{
		if (fooFirst) ASSERT_EQ(e , i_value);
		else ASSERT_EQ(e , 20);
		e = 20;
		ASSERT_EQ(e , 20);
	});
	ASSERT_EQ(e , 12);
	fooFirst = false;
}
//{end}

bool barFirst = true;

void bar(kdb::Integer const & e)
{
	e.context().with<ThreadLayer>()
		.with<SelectedPrinterLayer>()([&]()
	{
		if (barFirst) ASSERT_EQ(e , 20);
		else ASSERT_EQ(e , i_value);

		e.context().without<ThreadLayer>()([&]()
		{
			ASSERT_EQ(e , 20);
		});
	});
	ASSERT_EQ(e , 12);
	barFirst = false;
}

TEST(test_contextual_basic, threads)
{
	using namespace kdb;

	KeySet ks;
	Context c;
	kdb::Integer n(ks, c,  Key("/%thread%/%printer%/test",
					KEY_CASCADING_NAME,
					KEY_META, "default", s_value, KEY_END));


	n.context().activate<MainApplicationLayer>();
	ASSERT_EQ(n , i_value);

	n = 18;
	ASSERT_EQ(n , 18);

	Integer & d = n;

	d = i_value;
	ASSERT_EQ(d , i_value);

	d = 12;
	ASSERT_EQ(d , 12);

	foo(d);
	ASSERT_EQ(d , 12);

	foo(d);
	ASSERT_EQ(d , 12);

	bar(d);
	ASSERT_EQ(d , 12);

	std::thread t1(bar, std::cref(d));
	t1.join();
	ASSERT_EQ(d , 12);

	std::thread t2(foo, std::ref(d));
	t2.join();
	ASSERT_EQ(d , 12);
}

TEST(test_contextual_basic, nocontext)
{
	using namespace kdb;
	KeySet ks;
	NoContext c;
	kdb::Value<int> n(ks, c,  Key("/test",
				KEY_CASCADING_NAME,
				KEY_META, "default", s_value, KEY_END));
	ASSERT_EQ(n , i_value);

	n = 18;
	ASSERT_EQ(n , 18);
}
