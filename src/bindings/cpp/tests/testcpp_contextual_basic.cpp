/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <internal/kdb/config.h>

#include <kdbcontext.hpp>
#include <kdbthread.hpp>
#include <kdbvalue.hpp>

#include <thread>

#include <gtest/gtest.h>

class TestValueSubject : public kdb::ValueSubject
{
	virtual void notifyInThread () override
	{
	}
};

void fooxx (kdb::Command &)
{
}

TEST (test_contextual_basic, command)
{
	using namespace kdb;

	TestValueSubject v;
	Key k;
	Command::Func f = [k] () -> Command::Pair { return Command::Pair ("", ""); };

	Command c (v, f);
	fooxx (c);
	c ();
	EXPECT_EQ (&f, &c.execute);
	EXPECT_EQ (&v, &c.v);
}

const uint32_t i_value = 55;
const char * s_value = "55";

class CountryGermanyLayer : public kdb::Layer
{
public:
	std::string id () const override
	{
		return "country";
	}
	std::string operator() () const override
	{
		return "germany";
	}
};

class LanguageGermanLayer : public kdb::Layer
{
public:
	std::string id () const override
	{
		return "language";
	}
	std::string operator() () const override
	{
		return "german";
	}
};

class CountryGPSLayer : public kdb::Layer
{
public:
	CountryGPSLayer () : m_country ("austria")
	{
	}
	std::string id () const override
	{
		return "country";
	}
	std::string operator() () const override
	{
		return m_country;
	}

private:
	std::string m_country;
};

class ThreadLayer : public kdb::Layer
{
public:
	ThreadLayer ()
	{
	}
	std::string id () const override
	{
		return "thread";
	}
	std::string operator() () const override
	{
		std::ostringstream os;
		std::thread::id tid = std::this_thread::get_id ();
		if (tid != g_main_id)
		{
			os << tid;
		}
		return os.str ();
	};

private:
	static const std::thread::id g_main_id;
};

const std::thread::id ThreadLayer::g_main_id = std::this_thread::get_id ();

class CountingLayer : public kdb::Layer
{
public:
	CountingLayer () : m_id ()
	{
	}
	std::string id () const override
	{
		return "counting";
	}
	std::string operator() () const override
	{
		std::ostringstream os;
		os << m_id++;
		return os.str ();
	};

private:
	mutable long long m_id;
};


class SelectedPrinterLayer : public kdb::Layer
{
public:
	std::string id () const override
	{
		return "printer";
	}
	std::string operator() () const override
	{
		return "Laserdrucker";
	}
};

class MainApplicationLayer : public kdb::Layer
{
public:
	std::string id () const override
	{
		return "application";
	}
	std::string operator() () const override
	{
		return "main";
	}
};

class ProfileLayer : public kdb::Layer
{
public:
	explicit ProfileLayer (kdb::String const & profile) : m_profile (profile)
	{
	}
	std::string id () const override
	{
		return "profile";
	}
	std::string operator() () const override
	{
		return m_profile;
	}

private:
	kdb::String const & m_profile;
};

template <typename T>
class test_contextual_basic : public ::testing::Test
{
public:
	T context;
};

template <>
class test_contextual_basic<kdb::ThreadContext> : public ::testing::Test
{
public:
	test_contextual_basic () : context (coordinator)
	{
	}
	static kdb::Coordinator coordinator;
	kdb::ThreadContext context;
};

kdb::Coordinator test_contextual_basic<kdb::ThreadContext>::coordinator{};

typedef ::testing::Types<kdb::Context, kdb::ThreadContext> myContextualPolicies;
TYPED_TEST_CASE (test_contextual_basic, myContextualPolicies);

TYPED_TEST (test_contextual_basic, integer)
{
	using namespace kdb;
	KeySet ks;
	TypeParam c = this->context;
	EXPECT_TRUE (!ks.lookup ("/%/%/%/test"));
	Value<int, ContextPolicyIs<TypeParam>> i (ks, c,
						  Key ("/%language%/%country%/%dialect%/test", KEY_META, "default", s_value, KEY_END));
	EXPECT_EQ (i, i_value);
	// The value always needs a connection to a key
	EXPECT_TRUE (ks.lookup ("/%/%/%/test"));
	i = 5;
	EXPECT_EQ (i, 5);
	EXPECT_EQ (i.getName (), "user:/%/%/%/test");
	i.syncKeySet ();
	EXPECT_EQ (ks.lookup ("/%/%/%/test").getString (), "5");
	EXPECT_EQ (ks.lookup ("user:/%/%/%/test").getString (), "5");
	i = 10;
	EXPECT_EQ (i, 10);
	EXPECT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
	EXPECT_EQ (i.getName (), "user:/%/%/%/test");

	c.template activate<LanguageGermanLayer> ();
	EXPECT_EQ (i, i_value);
	//{debug/EXPECT_TRUE}
	EXPECT_EQ (i.context ()["language"], "german");
	EXPECT_EQ (i.getName (), "default:/german/%/%/test");
	//{end}
	EXPECT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
	EXPECT_TRUE (ks.lookup ("/german/%/%/test"));
	i = 15;
	EXPECT_EQ (i, 15);
	EXPECT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
	i.syncKeySet ();
	EXPECT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");

	c.template deactivate<LanguageGermanLayer> ();
	EXPECT_EQ (i, 10);
	EXPECT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
	EXPECT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");

	c.template with<LanguageGermanLayer> () ([&] () {
		EXPECT_EQ (i, 15);
		EXPECT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
		EXPECT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");
		c.template without<LanguageGermanLayer> () ([&] () {
			EXPECT_EQ (i, 10);
			EXPECT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
			EXPECT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");
		});
		EXPECT_EQ (i, 15);
		EXPECT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
		EXPECT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");
	});
	EXPECT_EQ (i, 10);
	EXPECT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
	EXPECT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");

	c.template with<LanguageGermanLayer> ().template with<CountryGermanyLayer> () ([&] () {
		EXPECT_EQ (i, i_value);
		EXPECT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
		EXPECT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");
		EXPECT_TRUE (ks.lookup ("/german/germany/%/test"));
		i = 20;
		EXPECT_EQ (i.getName (), "user:/german/germany/%/test");
		EXPECT_EQ (ks.lookup ("/german/germany/%/test").getString (), "20");
		/*
		//{debug/backtrace}
		#3  0x0000000000407a56 in operator() at first.cpp:1521
			i = @0x7fffe36b69a0: { ...
			  m_evaluated_name = "/german/germany/%/test" }
		//{end}
		//{debug/breakpoint}
		break 1520 if i.getName()
			      .compare("/german/germany/%/test") == 0
		//{end}
		*/
		EXPECT_EQ (i, 20);
	});
	EXPECT_EQ (i, 10);
	EXPECT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
	EXPECT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");
	EXPECT_EQ (ks.lookup ("/german/germany/%/test").getString (), "20");

	c.template with<LanguageGermanLayer> ().template with<CountryGermanyLayer> () ([&] () {
		EXPECT_EQ (i, 20);
		EXPECT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
		EXPECT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");
		EXPECT_EQ (ks.lookup ("/german/germany/%/test").getString (), "20");
		i = 30;
		EXPECT_EQ (i, 30);
		EXPECT_EQ (ks.lookup ("/german/germany/%/test").getString (), "30");
	});
	EXPECT_EQ (i, 10);
	EXPECT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
	EXPECT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");
	EXPECT_EQ (ks.lookup ("/german/germany/%/test").getString (), "30");

	c.template with<LanguageGermanLayer> ().template with<CountryGermanyLayer> () ([&] () {
		EXPECT_EQ (i, 30);
		EXPECT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
		EXPECT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");
		EXPECT_EQ (ks.lookup ("/german/germany/%/test").getString (), "30");
		c.template with<CountryGPSLayer> () ([&] () { EXPECT_EQ (i, i_value); });
		EXPECT_EQ (ks.lookup ("/german/austria/%/test").getString (), s_value);
	});
	EXPECT_EQ (i, 10);
	EXPECT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
	EXPECT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");
}

TYPED_TEST (test_contextual_basic, mixedWithActivate)
{
	using namespace kdb;
	KeySet ks;
	TypeParam c = this->context;
	EXPECT_TRUE (!ks.lookup ("/%/%/%/test"));
	Value<int, ContextPolicyIs<TypeParam>> i (ks, c,
						  Key ("/%language%/%country%/%dialect%/test", KEY_META, "default", s_value, KEY_END));
	EXPECT_EQ (i, i_value);
	// The value always needs a connection to a key
	EXPECT_TRUE (ks.lookup ("/%/%/%/test"));
	i = 5;
	EXPECT_EQ (i, 5);
	EXPECT_EQ (i.getName (), "user:/%/%/%/test");
	EXPECT_EQ (ks.lookup ("user:/%/%/%/test").getString (), "5");

	c.template activate<LanguageGermanLayer> ();
	i = 6;
	EXPECT_EQ (i, 6);
	EXPECT_EQ (i.getName (), "user:/german/%/%/test");
	EXPECT_EQ (ks.lookup ("user:/german/%/%/test").getString (), "6");

	c.template with<CountryGermanyLayer> () ([&] () {
		i = 7;
		EXPECT_EQ (i, 7);
		EXPECT_EQ (i.getName (), "user:/german/germany/%/test");
		EXPECT_EQ (ks.lookup ("user:/german/germany/%/test").getString (), "7");
	});

	// LanguageGermanLayer still active
	EXPECT_EQ (i, 6);
	EXPECT_EQ (i.getName (), "user:/german/%/%/test");
	EXPECT_EQ (ks.lookup ("user:/german/%/%/test").getString (), "6");

	c.template deactivate<LanguageGermanLayer> ();
	EXPECT_EQ (i, 5);
	EXPECT_EQ (i.getName (), "user:/%/%/%/test");
	EXPECT_EQ (ks.lookup ("user:/%/%/%/test").getString (), "5");
}

TYPED_TEST (test_contextual_basic, nestedWithActivate)
{
	using namespace kdb;
	KeySet ks;
	TypeParam c = this->context;
	EXPECT_TRUE (!ks.lookup ("/%/%/%/test"));
	Value<int, ContextPolicyIs<TypeParam>> i (ks, c,
						  Key ("/%language%/%country%/%dialect%/test", KEY_META, "default", s_value, KEY_END));
	EXPECT_EQ (i, i_value);
	// The value always needs a connection to a key
	EXPECT_TRUE (ks.lookup ("/%/%/%/test"));
	i = 5;
	EXPECT_EQ (i, 5);
	EXPECT_EQ (i.getName (), "user:/%/%/%/test");
	EXPECT_EQ (ks.lookup ("user:/%/%/%/test").getString (), "5");

	c.template with<CountryGermanyLayer> () ([&] () {
		i = 7;
		EXPECT_EQ (i, 7);
		EXPECT_EQ (i.getName (), "user:/%/germany/%/test");
		EXPECT_EQ (ks.lookup ("user:/%/germany/%/test").getString (), "7");

		c.template without<CountryGermanyLayer> () ([&] () {
			c.template activate<LanguageGermanLayer> ();

			i = 6;
			EXPECT_EQ (i, 6);
			EXPECT_EQ (i.getName (), "user:/german/%/%/test");
			EXPECT_EQ (ks.lookup ("user:/german/%/%/test").getString (), "6");
		});
	});

	// LanguageGermanLayer still active
	EXPECT_EQ (i, 6);
	EXPECT_EQ (i.getName (), "user:/german/%/%/test");
	EXPECT_EQ (ks.lookup ("user:/german/%/%/test").getString (), "6");

	c.template deactivate<LanguageGermanLayer> ();
	EXPECT_EQ (i, 5);
	EXPECT_EQ (i.getName (), "user:/%/%/%/test");
	EXPECT_EQ (ks.lookup ("user:/%/%/%/test").getString (), "5");
}


TYPED_TEST (test_contextual_basic, nestedWithActivateConflicting)
{
	using namespace kdb;
	KeySet ks;
	TypeParam c = this->context;
	EXPECT_TRUE (!ks.lookup ("/%/%/%/test"));
	Value<int, ContextPolicyIs<TypeParam>> i (ks, c,
						  Key ("/%language%/%country%/%dialect%/test", KEY_META, "default", s_value, KEY_END));
	EXPECT_EQ (i, i_value);
	// The value always needs a connection to a key
	EXPECT_TRUE (ks.lookup ("/%/%/%/test"));
	i = 5;
	EXPECT_EQ (i, 5);
	EXPECT_EQ (i.getName (), "user:/%/%/%/test");
	EXPECT_EQ (ks.lookup ("user:/%/%/%/test").getString (), "5");

	c.template with<CountryGermanyLayer> () ([&] () {
		i = 7;
		EXPECT_EQ (i, 7);
		EXPECT_EQ (i.getName (), "user:/%/germany/%/test");
		EXPECT_EQ (ks.lookup ("user:/%/germany/%/test").getString (), "7");

		c.template without<CountryGermanyLayer> () ([&] () {
			EXPECT_EQ (i, 5);
			EXPECT_EQ (i.getName (), "user:/%/%/%/test");
			EXPECT_EQ (ks.lookup ("user:/%/%/%/test").getString (), "5");

			c.template activate<CountryGermanyLayer> ();

			i = 6;
			EXPECT_EQ (i, 6);
			EXPECT_EQ (i.getName (), "user:/%/germany/%/test");
			EXPECT_EQ (ks.lookup ("user:/%/germany/%/test").getString (), "6");
		});
		// restore activation of layer

		EXPECT_EQ (i, 6);
		EXPECT_EQ (i.getName (), "user:/%/germany/%/test");
		EXPECT_EQ (ks.lookup ("user:/%/germany/%/test").getString (), "6");
	});
	// restore deactivation of layer

	EXPECT_EQ (i, 5);
	EXPECT_EQ (i.getName (), "user:/%/%/%/test");
	EXPECT_EQ (ks.lookup ("user:/%/%/%/test").getString (), "5");
}


TYPED_TEST (test_contextual_basic, counting)
{
	using namespace kdb;

	std::shared_ptr<kdb::Layer> l = std::make_shared<CountingLayer> ();
	KeySet ks;
	TypeParam c = this->context;
	c.template with<CountingLayer> () ([&] { EXPECT_EQ (c["counting"], "0"); });
	// is it a specification error to have counting
	// two times?
	Value<int, ContextPolicyIs<TypeParam>> i (ks, c, Key ("/%counting%/%counting%", KEY_META, "default", s_value, KEY_END));

	EXPECT_EQ ((*l) (), "0");
	EXPECT_EQ ((*l) (), "1");
	c.withl (l, [&] {
		EXPECT_EQ (c["counting"], "4");
		EXPECT_EQ (c["counting"], "5");
	});
	EXPECT_EQ ((*l) (), "6");
	c.template with<CountingLayer> () ([&] {
		EXPECT_EQ (c["counting"], "2");
		EXPECT_EQ (c["counting"], "3");
	});
	EXPECT_TRUE (c["counting"].empty ());
	c.template with<CountingLayer> () ([&] {
		EXPECT_EQ (c["counting"], "2");
		EXPECT_EQ (c["counting"], "3");
	});
	EXPECT_TRUE (c["counting"].empty ());
	c.template activate<CountingLayer> ();
	EXPECT_EQ (c["counting"], "2");
	EXPECT_EQ (c["counting"], "3");
	c.template deactivate<CountingLayer> ();
	EXPECT_TRUE (c["counting"].empty ());
}

TYPED_TEST (test_contextual_basic, groups)
{
	using namespace kdb;

	KeySet ks;
	TypeParam c = this->context;
	Value<int, ContextPolicyIs<TypeParam>> i (
		ks, c,
		Key ("/%application%/%version profile thread module%/%manufacturer type family model%/serial_number", KEY_META, "default",
		     s_value, KEY_END));
	EXPECT_EQ (i.getName (), "default:/%/%/%/serial_number");
	c.template activate<MainApplicationLayer> ();
	String s (ks, c, Key ("/%x%", KEY_META, "default", "anonymous", KEY_END));
	c.template activate<ProfileLayer> (s);
	EXPECT_EQ (i.getName (), "default:/main/%/%/serial_number");
	c.activate ("version", "1");
	EXPECT_EQ (i.getName (), "default:/main/%1%anonymous/%/serial_number");
	c.activate ("module", "M1");
	EXPECT_EQ (i.getName (), "default:/main/%1%anonymous/%/serial_number");
	c.activate ("manufacturer", "hp");
	EXPECT_EQ (i.getName (), "default:/main/%1%anonymous/%hp/serial_number");
	c.activate ("family", "EliteBook");
	EXPECT_EQ (i.getName (), "default:/main/%1%anonymous/%hp/serial_number");
	c.template activate<KeyValueLayer> ("type", "MobileWorkstation");
	EXPECT_EQ (i.getName (), "default:/main/%1%anonymous/%hp%MobileWorkstation%EliteBook/serial_number");
	c.template activate<KeyValueLayer> ("model", "8570");
	EXPECT_EQ (i.getName (), "default:/main/%1%anonymous/%hp%MobileWorkstation%EliteBook%8570/serial_number");
	c.template activate<KeyValueLayer> ("thread", "40");
	EXPECT_EQ (i.getName (), "default:/main/%1%anonymous%40%M1/%hp%MobileWorkstation%EliteBook%8570/serial_number");
	c.template deactivate<KeyValueLayer> ("version", "");
	EXPECT_EQ (i.getName (), "default:/main/%/%hp%MobileWorkstation%EliteBook%8570/serial_number");
	c.template activate<KeyValueLayer> ("version", "4");
	EXPECT_EQ (i.getName (), "default:/main/%4%anonymous%40%M1/%hp%MobileWorkstation%EliteBook%8570/serial_number");
	c.template deactivate<KeyValueLayer> ("manufacturer", "");
	EXPECT_EQ (i.getName (), "default:/main/%4%anonymous%40%M1/%/serial_number");
	c.template activate<KeyValueLayer> ("manufacturer", "HP");
	EXPECT_EQ (i.getName (), "default:/main/%4%anonymous%40%M1/%HP%MobileWorkstation%EliteBook%8570/serial_number");
	c.template deactivate<KeyValueLayer> ("type", "");
	EXPECT_EQ (i.getName (), "default:/main/%4%anonymous%40%M1/%HP/serial_number");
	c.template with<KeyValueLayer> ("type", "Notebook") ([&] {
		EXPECT_EQ (i.getName (), "default:/main/%4%anonymous%40%M1/%HP%Notebook%EliteBook%8570/serial_number");
		c.template without<KeyValueLayer> ("type", "") (
			[&] { EXPECT_EQ (i.getName (), "default:/main/%4%anonymous%40%M1/%HP/serial_number"); });
		EXPECT_EQ (i.getName (), "default:/main/%4%anonymous%40%M1/%HP%Notebook%EliteBook%8570/serial_number");
	});
}

class myId : public kdb::Wrapped
{
	virtual std::string layerId () const
	{
		return "id";
	}

	virtual std::string layerVal () const
	{
		return "my";
	}
};


TYPED_TEST (test_contextual_basic, wrapped)
{
	using namespace kdb;

	KeySet ks;
	TypeParam c = this->context;
	Value<int, ContextPolicyIs<TypeParam>> i (ks, c, Key ("/%id%/key", KEY_META, "default", s_value, KEY_END));
	EXPECT_EQ (i.getName (), "default:/%/key");
	c.activate (myId ());
	EXPECT_EQ (i.getName (), "default:/my/key");
}


TYPED_TEST (test_contextual_basic, cvWrapped)
{
	using namespace kdb;

	KeySet ks;
	TypeParam c = this->context;
	Value<std::string, ContextPolicyIs<TypeParam>> i (ks, c, Key ("/ignore/id", KEY_META, "default", "my", KEY_END));

	Value<int, ContextPolicyIs<TypeParam>> x (ks, c, Key ("/%id%/key", KEY_META, "default", s_value, KEY_END));

	EXPECT_EQ (x.getName (), "default:/%/key");
	EXPECT_TRUE (ks.lookup ("/%/key"));
	c.activate (i);
	EXPECT_EQ (x.getName (), "default:/my/key");
	EXPECT_TRUE (ks.lookup ("/my/key"));

	ks.append (Key ("default:/other/key", KEY_VALUE, "88", KEY_END));
	i = "other";
	c.activate (i);
	EXPECT_EQ (x.getName (), "default:/other/key");
	EXPECT_TRUE (ks.lookup ("default:/other/key"));
	EXPECT_EQ (x, 88);
	EXPECT_EQ (ks.lookup ("default:/other/key").getString (), "88");

	ks.append (Key ("default:/other/key", KEY_VALUE, "100", KEY_END));
	EXPECT_EQ (ks.lookup ("default:/other/key").getString (), "100");
	EXPECT_EQ (x, 88) << "updated from KeySet?";
}


TYPED_TEST (test_contextual_basic, cvWrappedInt)
{
	using namespace kdb;

	KeySet ks;
	TypeParam c = this->context;
	Value<int, ContextPolicyIs<TypeParam>> i (ks, c, Key ("/ignore/id", KEY_META, "default", "88", KEY_END));

	Value<int, ContextPolicyIs<TypeParam>> x (ks, c, Key ("/%id%/key", KEY_META, "default", s_value, KEY_END));

	EXPECT_EQ (x.getName (), "default:/%/key");
	c.activate (i);
	EXPECT_EQ (x.getName (), "default:/88/key");
}


TEST (test_contextual_basic, integer_copy)
{
	using namespace kdb;
	KeySet ks;
	Context c;
	EXPECT_TRUE (!ks.lookup ("/%/%/%/test"));
	Integer i (ks, c, Key ("/%language%/%country%/%dialect%/test", KEY_META, "default", s_value, KEY_END));
	EXPECT_EQ (i, i_value);
	EXPECT_TRUE (ks.lookup ("/%/%/%/test"));
	i = 5;
	EXPECT_EQ (i, 5);
	EXPECT_EQ (i.getName (), "user:/%/%/%/test");
	i.syncKeySet ();
	EXPECT_EQ (ks.lookup ("/%/%/%/test").getString (), "5");
}

TEST (test_contextual_basic, evaluate)
{
	using namespace kdb;
	kdb::Context c;
	EXPECT_EQ (c["language"], "");
	EXPECT_EQ (c["country"], "");
	EXPECT_EQ (c["dialect"], "");
	EXPECT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/%/%/%/test");
	EXPECT_EQ (c.evaluate ("/%language country dialect%/test"), "/%/test");

	c.activate<LanguageGermanLayer> ();
	EXPECT_EQ (c["language"], "german");
	EXPECT_EQ (c["country"], "");
	EXPECT_EQ (c["dialect"], "");
	EXPECT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/german/%/%/test");
	EXPECT_EQ (c.evaluate ("/%language country dialect%/test"), "/%german/test");

	c.activate<LanguageGermanLayer> ();
	c.activate<CountryGermanyLayer> ();
	EXPECT_EQ (c["language"], "german");
	EXPECT_EQ (c["country"], "germany");
	EXPECT_EQ (c["dialect"], "");
	EXPECT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/german/germany/%/test");
	EXPECT_EQ (c.evaluate ("/%language country dialect%/test"), "/%german%germany/test");
	c.deactivate<CountryGermanyLayer> ();

	c.activate<LanguageGermanLayer> ();
	c.activate<KeyValueLayer> ("country", "%");
	EXPECT_EQ (c["language"], "german");
	EXPECT_EQ (c["country"], "%");
	EXPECT_EQ (c["dialect"], "");
	// TODO: Escaping not implemented
	// EXPECT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/german/\\%/%/test");
	// EXPECT_EQ (c.evaluate ("/%language country dialect%/test"), "/%german%\\%/test");
	c.deactivate<KeyValueLayer> ("country", "%");

	c.deactivate<LanguageGermanLayer> ();
	EXPECT_EQ (c["language"], "");
	EXPECT_EQ (c["country"], "");
	EXPECT_EQ (c["dialect"], "");
	EXPECT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/%/%/%/test");

	c.with<LanguageGermanLayer> () ([&] () {
		EXPECT_EQ (c["language"], "german");
		EXPECT_EQ (c["country"], "");
		EXPECT_EQ (c["dialect"], "");
		EXPECT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/german/%/%/test");
		c.without<LanguageGermanLayer> () ([&] () {
			EXPECT_EQ (c["language"], "");
			EXPECT_EQ (c["country"], "");
			EXPECT_EQ (c["dialect"], "");
			EXPECT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/%/%/%/test");
		});
		EXPECT_EQ (c["language"], "german");
		EXPECT_EQ (c["country"], "");
		EXPECT_EQ (c["dialect"], "");
		EXPECT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/german/%/%/test");
	});
	EXPECT_EQ (c["language"], "");
	EXPECT_EQ (c["country"], "");
	EXPECT_EQ (c["dialect"], "");
	EXPECT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/%/%/%/test");

	c.with<LanguageGermanLayer> ().with<CountryGermanyLayer> () ([&] () {
		EXPECT_EQ (c["language"], "german");
		EXPECT_EQ (c["country"], "germany");
		EXPECT_EQ (c["dialect"], "");
		EXPECT_EQ (c.size (), 2);
		EXPECT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/german/germany/%/test");
		EXPECT_EQ (c.evaluate ("/%language country dialect%/test"), "/%german%germany/test");
		c.with<CountryGPSLayer> () ([&] () {
			EXPECT_EQ (c["language"], "german");
			EXPECT_EQ (c["country"], "austria");
			EXPECT_EQ (c["dialect"], "");
			EXPECT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/german/austria/%/test");
			EXPECT_EQ (c.evaluate ("/%language country dialect%/test"), "/%german%austria/test");
			c.without<CountryGPSLayer> () ([&] () {
				EXPECT_EQ (c["language"], "german");
				EXPECT_EQ (c["country"], "");
				EXPECT_EQ (c["dialect"], "");
				EXPECT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/german/%/%/test");
			});
		});
		EXPECT_EQ (c["language"], "german");
		EXPECT_EQ (c["country"], "germany");
		EXPECT_EQ (c["dialect"], "");
		EXPECT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/german/germany/%/test");
		EXPECT_EQ (c.evaluate ("/%language country%/%dialect%/test"), "/%german%germany/%/test");
	});
	EXPECT_EQ (c["language"], "");
	EXPECT_EQ (c["country"], "");
	EXPECT_EQ (c["dialect"], "");
	EXPECT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/%/%/%/test");

	c.with<LanguageGermanLayer> ().with<CountryGermanyLayer> () ([&] {
		EXPECT_EQ (c["language"], "german");
		EXPECT_EQ (c["country"], "germany");
		EXPECT_EQ (c["dialect"], "");
		EXPECT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/german/germany/%/test");
		EXPECT_EQ (c.evaluate ("/%language%/%language%/%dialect%/test"), "/german/german/%/test");

		EXPECT_EQ (c.evaluate ("/%language%%country%%dialect%/test"), "/germangermany%/test");
		EXPECT_EQ (c.evaluate ("/%language%%language%%dialect%/test"), "/germangerman%/test");
	});
	EXPECT_EQ (c["language"], "");
	EXPECT_EQ (c["country"], "");
	EXPECT_EQ (c["dialect"], "");
	EXPECT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/%/%/%/test");

	EXPECT_EQ (c["language"], "");
	EXPECT_EQ (c["country"], "");
	EXPECT_EQ (c["dialect"], "");
	EXPECT_EQ (c.evaluate ("/%language%%country%%dialect%/test"), "/%%%/test");

	KeySet ks;
	Integer i (ks, c,
		   Key ("/%application%/%version%/%profile%/%thread%/%module%/%manufacturer%/%type%/%family%/%model%/serial_number",
			KEY_META, "default", s_value, KEY_END));
	Integer j (ks, c,
		   Key ("/%application version profile thread module manufacturer type family model%/serial_number", KEY_META, "default",
			s_value, KEY_END));
	EXPECT_EQ (i.getName (), "default:/%/%/%/%/%/%/%/%/%/serial_number");
	EXPECT_EQ (j.getName (), "default:/%/serial_number");
	c.activate<MainApplicationLayer> ();
	EXPECT_EQ (i.getName (), "default:/main/%/%/%/%/%/%/%/%/serial_number");
	EXPECT_EQ (j.getName (), "default:/%main/serial_number");
	String s (ks, c, Key ("/%x%", KEY_META, "default", "anonymous", KEY_END));
	c.activate<ProfileLayer> (s);
	EXPECT_EQ (i.getName (), "default:/main/%/anonymous/%/%/%/%/%/%/serial_number");
	EXPECT_EQ (j.getName (), "default:/%main/serial_number");
	c.activate<KeyValueLayer> ("module", "M1");
	EXPECT_EQ (i.getName (), "default:/main/%/anonymous/%/M1/%/%/%/%/serial_number");
	EXPECT_EQ (j.getName (), "default:/%main/serial_number");
	c.activate<KeyValueLayer> ("manufacturer", "hp");
	EXPECT_EQ (i.getName (), "default:/main/%/anonymous/%/M1/hp/%/%/%/serial_number");
	EXPECT_EQ (j.getName (), "default:/%main/serial_number");
	c.activate<KeyValueLayer> ("family", "EliteBook");
	EXPECT_EQ (i.getName (), "default:/main/%/anonymous/%/M1/hp/%/EliteBook/%/serial_number");
	EXPECT_EQ (j.getName (), "default:/%main/serial_number");
	c.activate<KeyValueLayer> ("version", "1");
	EXPECT_EQ (i.getName (), "default:/main/1/anonymous/%/M1/hp/%/EliteBook/%/serial_number");
	EXPECT_EQ (j.getName (), "default:/%main%1%anonymous/serial_number");
}


struct MockObserver : kdb::ValueObserver
{
	MockObserver () : counter ()
	{
	}

	virtual void updateContext (bool) const override
	{
		++counter;
	}

	virtual kdb::Key getDepKey () const override
	{
		throw "should not happen";
	}

	mutable long long counter;
};

// duplicates need to be filtered
TEST (test_contextual_basic, valueObserver)
{
	kdb::Context c;
	MockObserver o1;
	MockObserver o2;
	MockObserver o3;
	c.attachByName ("/%event1%/%event2%", o1);
	c.attachByName ("/%event1%/%event3%", o2);
	c.attachByName ("/%eventX%", o3);
	c.attachByName ("/%eventX%", o3);
	EXPECT_EQ (o1.counter, 0);
	EXPECT_EQ (o2.counter, 0);
	c.notifyByEvents ({ "event1" });
	EXPECT_EQ (o1.counter, 1);
	EXPECT_EQ (o2.counter, 1);
	c.notifyByEvents ({ "event2" });
	EXPECT_EQ (o1.counter, 2);
	EXPECT_EQ (o2.counter, 1);
	c.notifyByEvents ({ "event3" });
	EXPECT_EQ (o1.counter, 2);
	EXPECT_EQ (o2.counter, 2);
	c.notifyByEvents ({ "event4" });
	EXPECT_EQ (o1.counter, 2);
	EXPECT_EQ (o2.counter, 2);
	c.notifyByEvents ({ "event1", "event2" });
	EXPECT_EQ (o1.counter, 3);
	EXPECT_EQ (o2.counter, 3);
	c.notifyByEvents ({ "event1", "event3" });
	EXPECT_EQ (o1.counter, 4);
	EXPECT_EQ (o2.counter, 4);
	EXPECT_EQ (o3.counter, 0);
	c.notifyAllEvents ();
	EXPECT_EQ (o1.counter, 5);
	EXPECT_EQ (o2.counter, 5);
	EXPECT_EQ (o3.counter, 1);
}


bool fooFirst = true;

//{foo}
void foo (kdb::Integer & e)
{
	e.context ().with<SelectedPrinterLayer> () ([&] () {
		if (fooFirst)
			EXPECT_EQ (e, i_value);
		else
			EXPECT_EQ (e, 20);
		e = 20;
		EXPECT_EQ (e, 20);
	});
	EXPECT_EQ (e, 12);
	fooFirst = false;
}
//{end}

bool barFirst = true;

void bar (kdb::Integer const & e)
{
	e.context ().with<ThreadLayer> ().with<SelectedPrinterLayer> () ([&] () {
		if (barFirst)
			EXPECT_EQ (e, 20);
		else
			EXPECT_EQ (e, i_value);

		e.context ().without<ThreadLayer> () ([&] () { EXPECT_EQ (e, 20); });
	});
	EXPECT_EQ (e, 12);
	barFirst = false;
}

TEST (test_contextual_basic, threads)
{
	using namespace kdb;

	KeySet ks;
	Context c;
	kdb::Integer n (ks, c, Key ("/%thread%/%printer%/test", KEY_META, "default", s_value, KEY_END));


	n.context ().activate<MainApplicationLayer> ();
	EXPECT_EQ (n, i_value);

	n = 18;
	EXPECT_EQ (n, 18);

	Integer & d = n;

	d = i_value;
	EXPECT_EQ (d, i_value);

	d = 12;
	EXPECT_EQ (d, 12);

	foo (d);
	EXPECT_EQ (d, 12);

	foo (d);
	EXPECT_EQ (d, 12);

	bar (d);
	EXPECT_EQ (d, 12);

	std::thread t1 (bar, std::cref (d));
	t1.join ();
	EXPECT_EQ (d, 12);

	std::thread t2 (foo, std::ref (d));
	t2.join ();
	EXPECT_EQ (d, 12);
}

TEST (test_contextual_basic, nocontext)
{
	using namespace kdb;
	KeySet ks;
	NoContext c;
	kdb::Value<int> n (ks, c, Key ("/test", KEY_META, "default", s_value, KEY_END));
	EXPECT_EQ (n, i_value);

	n = 18;
	EXPECT_EQ (n, 18);
}

TEST (test_contextual_basic, operators)
{
	using namespace kdb;
	KeySet ks;
	NoContext c;
	kdb::Value<int> n (ks, c, Key ("/test/n", KEY_META, "default", s_value, KEY_END));
	kdb::Value<int> m (ks, c, Key ("/test/m", KEY_META, "default", s_value, KEY_END));
	EXPECT_EQ (n, i_value);
	EXPECT_EQ (m, i_value);

	n = 18;
	EXPECT_EQ (n, 18);
	EXPECT_EQ (18, n);
	EXPECT_EQ (n, n);
	EXPECT_EQ (!n, 0);
	EXPECT_EQ (0, !n);
	EXPECT_EQ (~n, ~18);
	EXPECT_EQ (~18, ~n);

	EXPECT_NE (n, 19);
	EXPECT_NE (19, n);
	EXPECT_NE (!n, n);
	EXPECT_NE (~n, n);

	EXPECT_LT (n, 19);
	EXPECT_GT (n, 17);
	EXPECT_LE (n, 19);
	EXPECT_GE (n, 17);

	EXPECT_LE (n, 18);
	EXPECT_GE (n, 18);

	n = 18;
	m = 18;

	EXPECT_EQ (n, m);
	EXPECT_EQ (m, n);

	n += 3;
	m += 3;

	EXPECT_EQ (n, m);
	EXPECT_EQ (m, n);

	m += n;
	EXPECT_EQ (n, 21);
	EXPECT_EQ (m, 42);

	EXPECT_EQ (n + n, m);
	EXPECT_EQ (m, n + n);

	--n;
	EXPECT_EQ (n, 20);

	EXPECT_EQ (n && n, true);

	n -= 10;
	EXPECT_EQ (n, 10);

	n *= 2;
	EXPECT_EQ (n, 20);

	n /= 2;
	EXPECT_EQ (n, 10);

	n %= 12;
	EXPECT_EQ (n, 10 % 12);

	n = 4 | 8;
	n |= 16;
	EXPECT_EQ (n, 4 | 8 | 16);

	n = 8;
	n = *&n; // *& added to suppress clang warning
	m = n;
	EXPECT_EQ (n, 8);
	EXPECT_EQ (m, 8);
	EXPECT_EQ (8, n);
	EXPECT_EQ (8, m);

	n = -8;
	m = 8;
	EXPECT_EQ (n, -m);
	EXPECT_EQ (-n, m);
}
