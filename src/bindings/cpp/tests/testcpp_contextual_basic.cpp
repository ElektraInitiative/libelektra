/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "kdbconfig.h"

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
	Command::Func f = [k]() -> Command::Pair { return Command::Pair ("", ""); };

	Command c (v, f);
	fooxx (c);
	c ();
	ASSERT_EQ (&f, &c.execute);
	ASSERT_EQ (&v, &c.v);
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
	ProfileLayer (kdb::String const & profile) : m_profile (profile)
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
	ASSERT_TRUE (!ks.lookup ("/%/%/%/test"));
	Value<int, ContextPolicyIs<TypeParam>> i (
		ks, c, Key ("/%language%/%country%/%dialect%/test", KEY_CASCADING_NAME, KEY_META, "default", s_value, KEY_END));
	ASSERT_EQ (i, i_value);
	// The value always needs a connection to a key
	ASSERT_TRUE (ks.lookup ("/%/%/%/test"));
	i = 5;
	ASSERT_EQ (i, 5);
	ASSERT_EQ (i.getName (), "user/%/%/%/test");
	i.syncKeySet ();
	ASSERT_EQ (ks.lookup ("/%/%/%/test").getString (), "5");
	ASSERT_EQ (ks.lookup ("user/%/%/%/test").getString (), "5");
	i = 10;
	ASSERT_EQ (i, 10);
	ASSERT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
	ASSERT_EQ (i.getName (), "user/%/%/%/test");

	c.template activate<LanguageGermanLayer> ();
	ASSERT_EQ (i, i_value);
	//{debug/ASSERT_TRUE}
	ASSERT_EQ (i.context ()["language"], "german");
	ASSERT_EQ (i.getName (), "/german/%/%/test");
	//{end}
	ASSERT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
	ASSERT_TRUE (ks.lookup ("/german/%/%/test"));
	i = 15;
	ASSERT_EQ (i, 15);
	ASSERT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
	i.syncKeySet ();
	ASSERT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");

	c.template deactivate<LanguageGermanLayer> ();
	ASSERT_EQ (i, 10);
	ASSERT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
	ASSERT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");

	c.template with<LanguageGermanLayer> () ([&]() {
		ASSERT_EQ (i, 15);
		ASSERT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
		ASSERT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");
		c.template without<LanguageGermanLayer> () ([&]() {
			ASSERT_EQ (i, 10);
			ASSERT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
			ASSERT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");
		});
		ASSERT_EQ (i, 15);
		ASSERT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
		ASSERT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");
	});
	ASSERT_EQ (i, 10);
	ASSERT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
	ASSERT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");

	c.template with<LanguageGermanLayer> ().template with<CountryGermanyLayer> () ([&]() {
		ASSERT_EQ (i, i_value);
		ASSERT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
		ASSERT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");
		ASSERT_TRUE (ks.lookup ("/german/germany/%/test"));
		i = 20;
		ASSERT_EQ (i.getName (), "user/german/germany/%/test");
		ASSERT_EQ (ks.lookup ("/german/germany/%/test").getString (), "20");
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
		ASSERT_EQ (i, 20);
	});
	ASSERT_EQ (i, 10);
	ASSERT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
	ASSERT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");
	ASSERT_EQ (ks.lookup ("/german/germany/%/test").getString (), "20");

	c.template with<LanguageGermanLayer> ().template with<CountryGermanyLayer> () ([&]() {
		ASSERT_EQ (i, 20);
		ASSERT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
		ASSERT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");
		ASSERT_EQ (ks.lookup ("/german/germany/%/test").getString (), "20");
		i = 30;
		ASSERT_EQ (i, 30);
		ASSERT_EQ (ks.lookup ("/german/germany/%/test").getString (), "30");
	});
	ASSERT_EQ (i, 10);
	ASSERT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
	ASSERT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");
	ASSERT_EQ (ks.lookup ("/german/germany/%/test").getString (), "30");

	c.template with<LanguageGermanLayer> ().template with<CountryGermanyLayer> () ([&]() {
		ASSERT_EQ (i, 30);
		ASSERT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
		ASSERT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");
		ASSERT_EQ (ks.lookup ("/german/germany/%/test").getString (), "30");
		c.template with<CountryGPSLayer> () ([&]() { ASSERT_EQ (i, i_value); });
		ASSERT_EQ (ks.lookup ("/german/austria/%/test").getString (), s_value);
	});
	ASSERT_EQ (i, 10);
	ASSERT_EQ (ks.lookup ("/%/%/%/test").getString (), "10");
	ASSERT_EQ (ks.lookup ("/german/%/%/test").getString (), "15");
}

TYPED_TEST (test_contextual_basic, mixedWithActivate)
{
	using namespace kdb;
	KeySet ks;
	TypeParam c = this->context;
	ASSERT_TRUE (!ks.lookup ("/%/%/%/test"));
	Value<int, ContextPolicyIs<TypeParam>> i (
		ks, c, Key ("/%language%/%country%/%dialect%/test", KEY_CASCADING_NAME, KEY_META, "default", s_value, KEY_END));
	ASSERT_EQ (i, i_value);
	// The value always needs a connection to a key
	ASSERT_TRUE (ks.lookup ("/%/%/%/test"));
	i = 5;
	ASSERT_EQ (i, 5);
	ASSERT_EQ (i.getName (), "user/%/%/%/test");
	ASSERT_EQ (ks.lookup ("user/%/%/%/test").getString (), "5");

	c.template activate<LanguageGermanLayer> ();
	i = 6;
	ASSERT_EQ (i, 6);
	ASSERT_EQ (i.getName (), "user/german/%/%/test");
	ASSERT_EQ (ks.lookup ("user/german/%/%/test").getString (), "6");

	c.template with<CountryGermanyLayer> () ([&]() {
		i = 7;
		ASSERT_EQ (i, 7);
		ASSERT_EQ (i.getName (), "user/german/germany/%/test");
		ASSERT_EQ (ks.lookup ("user/german/germany/%/test").getString (), "7");
	});

	// LanguageGermanLayer still active
	ASSERT_EQ (i, 6);
	ASSERT_EQ (i.getName (), "user/german/%/%/test");
	ASSERT_EQ (ks.lookup ("user/german/%/%/test").getString (), "6");

	c.template deactivate<LanguageGermanLayer> ();
	ASSERT_EQ (i, 5);
	ASSERT_EQ (i.getName (), "user/%/%/%/test");
	ASSERT_EQ (ks.lookup ("user/%/%/%/test").getString (), "5");
}

TYPED_TEST (test_contextual_basic, nestedWithActivate)
{
	using namespace kdb;
	KeySet ks;
	TypeParam c = this->context;
	ASSERT_TRUE (!ks.lookup ("/%/%/%/test"));
	Value<int, ContextPolicyIs<TypeParam>> i (
		ks, c, Key ("/%language%/%country%/%dialect%/test", KEY_CASCADING_NAME, KEY_META, "default", s_value, KEY_END));
	ASSERT_EQ (i, i_value);
	// The value always needs a connection to a key
	ASSERT_TRUE (ks.lookup ("/%/%/%/test"));
	i = 5;
	ASSERT_EQ (i, 5);
	ASSERT_EQ (i.getName (), "user/%/%/%/test");
	ASSERT_EQ (ks.lookup ("user/%/%/%/test").getString (), "5");

	c.template with<CountryGermanyLayer> () ([&]() {
		i = 7;
		ASSERT_EQ (i, 7);
		ASSERT_EQ (i.getName (), "user/%/germany/%/test");
		ASSERT_EQ (ks.lookup ("user/%/germany/%/test").getString (), "7");

		c.template without<CountryGermanyLayer> () ([&]() {
			c.template activate<LanguageGermanLayer> ();

			i = 6;
			ASSERT_EQ (i, 6);
			ASSERT_EQ (i.getName (), "user/german/%/%/test");
			ASSERT_EQ (ks.lookup ("user/german/%/%/test").getString (), "6");
		});
	});

	// LanguageGermanLayer still active
	ASSERT_EQ (i, 6);
	ASSERT_EQ (i.getName (), "user/german/%/%/test");
	ASSERT_EQ (ks.lookup ("user/german/%/%/test").getString (), "6");

	c.template deactivate<LanguageGermanLayer> ();
	ASSERT_EQ (i, 5);
	ASSERT_EQ (i.getName (), "user/%/%/%/test");
	ASSERT_EQ (ks.lookup ("user/%/%/%/test").getString (), "5");
}


TYPED_TEST (test_contextual_basic, counting)
{
	using namespace kdb;

	std::shared_ptr<kdb::Layer> l = std::make_shared<CountingLayer> ();
	KeySet ks;
	TypeParam c = this->context;
	c.template with<CountingLayer> () ([&] { ASSERT_EQ (c["counting"], "0"); });
	// is it a specification error to have counting
	// two times?
	Value<int, ContextPolicyIs<TypeParam>> i (
		ks, c, Key ("/%counting%/%counting%", KEY_CASCADING_NAME, KEY_META, "default", s_value, KEY_END));

	ASSERT_EQ ((*l) (), "0");
	ASSERT_EQ ((*l) (), "1");
	c.withl (l, [&] {
		ASSERT_EQ (c["counting"], "4");
		ASSERT_EQ (c["counting"], "5");
	});
	ASSERT_EQ ((*l) (), "6");
	c.template with<CountingLayer> () ([&] {
		ASSERT_EQ (c["counting"], "2");
		ASSERT_EQ (c["counting"], "3");
	});
	ASSERT_TRUE (c["counting"].empty ());
	c.template with<CountingLayer> () ([&] {
		ASSERT_EQ (c["counting"], "2");
		ASSERT_EQ (c["counting"], "3");
	});
	ASSERT_TRUE (c["counting"].empty ());
	c.template activate<CountingLayer> ();
	ASSERT_EQ (c["counting"], "2");
	ASSERT_EQ (c["counting"], "3");
	c.template deactivate<CountingLayer> ();
	ASSERT_TRUE (c["counting"].empty ());
}

TYPED_TEST (test_contextual_basic, groups)
{
	using namespace kdb;

	KeySet ks;
	TypeParam c = this->context;
	Value<int, ContextPolicyIs<TypeParam>> i (
		ks, c, Key ("/%application%/%version profile thread module%/%manufacturer type family model%/serial_number",
			    KEY_CASCADING_NAME, KEY_META, "default", s_value, KEY_END));
	ASSERT_EQ (i.getName (), "/%/%/%/serial_number");
	c.template activate<MainApplicationLayer> ();
	String s (ks, c, Key ("/%x%", KEY_CASCADING_NAME, KEY_META, "default", "anonymous", KEY_END));
	c.template activate<ProfileLayer> (s);
	ASSERT_EQ (i.getName (), "/main/%/%/serial_number");
	c.activate ("version", "1");
	ASSERT_EQ (i.getName (), "/main/%1%anonymous/%/serial_number");
	c.activate ("module", "M1");
	ASSERT_EQ (i.getName (), "/main/%1%anonymous/%/serial_number");
	c.activate ("manufacturer", "hp");
	ASSERT_EQ (i.getName (), "/main/%1%anonymous/%hp/serial_number");
	c.activate ("family", "EliteBook");
	ASSERT_EQ (i.getName (), "/main/%1%anonymous/%hp/serial_number");
	c.template activate<KeyValueLayer> ("type", "MobileWorkstation");
	ASSERT_EQ (i.getName (), "/main/%1%anonymous/%hp%MobileWorkstation%EliteBook/serial_number");
	c.template activate<KeyValueLayer> ("model", "8570");
	ASSERT_EQ (i.getName (), "/main/%1%anonymous/%hp%MobileWorkstation%EliteBook%8570/serial_number");
	c.template activate<KeyValueLayer> ("thread", "40");
	ASSERT_EQ (i.getName (), "/main/%1%anonymous%40%M1/%hp%MobileWorkstation%EliteBook%8570/serial_number");
	c.template deactivate<KeyValueLayer> ("version", "");
	ASSERT_EQ (i.getName (), "/main/%/%hp%MobileWorkstation%EliteBook%8570/serial_number");
	c.template activate<KeyValueLayer> ("version", "4");
	ASSERT_EQ (i.getName (), "/main/%4%anonymous%40%M1/%hp%MobileWorkstation%EliteBook%8570/serial_number");
	c.template deactivate<KeyValueLayer> ("manufacturer", "");
	ASSERT_EQ (i.getName (), "/main/%4%anonymous%40%M1/%/serial_number");
	c.template activate<KeyValueLayer> ("manufacturer", "HP");
	ASSERT_EQ (i.getName (), "/main/%4%anonymous%40%M1/%HP%MobileWorkstation%EliteBook%8570/serial_number");
	c.template deactivate<KeyValueLayer> ("type", "");
	ASSERT_EQ (i.getName (), "/main/%4%anonymous%40%M1/%HP/serial_number");
	c.template with<KeyValueLayer> ("type", "Notebook") ([&] {
		ASSERT_EQ (i.getName (), "/main/%4%anonymous%40%M1/%HP%Notebook%EliteBook%8570/serial_number");
		c.template without<KeyValueLayer> ("type",
						   "") ([&] { ASSERT_EQ (i.getName (), "/main/%4%anonymous%40%M1/%HP/serial_number"); });
		ASSERT_EQ (i.getName (), "/main/%4%anonymous%40%M1/%HP%Notebook%EliteBook%8570/serial_number");
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
	ASSERT_EQ (i.getName (), "/%/key");
	c.activate (myId ());
	ASSERT_EQ (i.getName (), "/my/key");
}


TYPED_TEST (test_contextual_basic, cvWrapped)
{
	using namespace kdb;

	KeySet ks;
	TypeParam c = this->context;
	Value<std::string, ContextPolicyIs<TypeParam>> i (ks, c, Key ("/ignore/id", KEY_META, "default", "my", KEY_END));

	Value<int, ContextPolicyIs<TypeParam>> x (ks, c, Key ("/%id%/key", KEY_META, "default", s_value, KEY_END));

	ASSERT_EQ (x.getName (), "/%/key");
	ASSERT_TRUE (ks.lookup ("/%/key"));
	c.activate (i);
	ASSERT_EQ (x.getName (), "/my/key");
	ASSERT_TRUE (ks.lookup ("/my/key"));

	ks.append (Key ("/other/key", KEY_VALUE, "88", KEY_END));
	i = "other";
	c.activate (i);
	ASSERT_EQ (x.getName (), "/other/key");
	ASSERT_TRUE (ks.lookup ("/other/key"));
	ASSERT_EQ (x, 88);
	ASSERT_EQ (ks.lookup ("/other/key").getString (), "88");

	ks.append (Key ("/other/key", KEY_VALUE, "100", KEY_END));
	ASSERT_EQ (ks.lookup ("/other/key").getString (), "100");
	ASSERT_EQ (x, 88) << "updated from KeySet?";
}


TYPED_TEST (test_contextual_basic, cvWrappedInt)
{
	using namespace kdb;

	KeySet ks;
	TypeParam c = this->context;
	Value<int, ContextPolicyIs<TypeParam>> i (ks, c, Key ("/ignore/id", KEY_META, "default", "88", KEY_END));

	Value<int, ContextPolicyIs<TypeParam>> x (ks, c, Key ("/%id%/key", KEY_META, "default", s_value, KEY_END));

	ASSERT_EQ (x.getName (), "/%/key");
	c.activate (i);
	ASSERT_EQ (x.getName (), "/88/key");
}


TEST (test_contextual_basic, integer_copy)
{
	using namespace kdb;
	KeySet ks;
	Context c;
	ASSERT_TRUE (!ks.lookup ("/%/%/%/test"));
	Integer i (ks, c, Key ("/%language%/%country%/%dialect%/test", KEY_CASCADING_NAME, KEY_META, "default", s_value, KEY_END));
	ASSERT_EQ (i, i_value);
	ASSERT_TRUE (ks.lookup ("/%/%/%/test"));
	i = 5;
	ASSERT_EQ (i, 5);
	ASSERT_EQ (i.getName (), "user/%/%/%/test");
	i.syncKeySet ();
	ASSERT_EQ (ks.lookup ("/%/%/%/test").getString (), "5");
}

TEST (test_contextual_basic, evaluate)
{
	using namespace kdb;
	kdb::Context c;
	ASSERT_EQ (c["language"], "");
	ASSERT_EQ (c["country"], "");
	ASSERT_EQ (c["dialect"], "");
	ASSERT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/%/%/%/test");

	c.activate<LanguageGermanLayer> ();
	ASSERT_EQ (c["language"], "german");
	ASSERT_EQ (c["country"], "");
	ASSERT_EQ (c["dialect"], "");
	ASSERT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/german/%/%/test");

	c.deactivate<LanguageGermanLayer> ();
	ASSERT_EQ (c["language"], "");
	ASSERT_EQ (c["country"], "");
	ASSERT_EQ (c["dialect"], "");
	ASSERT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/%/%/%/test");

	c.with<LanguageGermanLayer> () ([&]() {
		ASSERT_EQ (c["language"], "german");
		ASSERT_EQ (c["country"], "");
		ASSERT_EQ (c["dialect"], "");
		ASSERT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/german/%/%/test");
		c.without<LanguageGermanLayer> () ([&]() {
			ASSERT_EQ (c["language"], "");
			ASSERT_EQ (c["country"], "");
			ASSERT_EQ (c["dialect"], "");
			ASSERT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/%/%/%/test");
		});
		ASSERT_EQ (c["language"], "german");
		ASSERT_EQ (c["country"], "");
		ASSERT_EQ (c["dialect"], "");
		ASSERT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/german/%/%/test");
	});
	ASSERT_EQ (c["language"], "");
	ASSERT_EQ (c["country"], "");
	ASSERT_EQ (c["dialect"], "");
	ASSERT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/%/%/%/test");

	c.with<LanguageGermanLayer> ().with<CountryGermanyLayer> () ([&]() {
		ASSERT_EQ (c["language"], "german");
		ASSERT_EQ (c["country"], "germany");
		ASSERT_EQ (c["dialect"], "");
		ASSERT_EQ (c.size (), 2);
		ASSERT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/german/germany/%/test");
		c.with<CountryGPSLayer> () ([&]() {
			ASSERT_EQ (c["language"], "german");
			ASSERT_EQ (c["country"], "austria");
			ASSERT_EQ (c["dialect"], "");
			ASSERT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/german/austria/%/test");
			c.without<CountryGPSLayer> () ([&]() {
				ASSERT_EQ (c["language"], "german");
				ASSERT_EQ (c["country"], "");
				ASSERT_EQ (c["dialect"], "");
				ASSERT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/german/%/%/test");
			});
		});
		ASSERT_EQ (c["language"], "german");
		ASSERT_EQ (c["country"], "germany");
		ASSERT_EQ (c["dialect"], "");
		ASSERT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/german/germany/%/test");
	});
	ASSERT_EQ (c["language"], "");
	ASSERT_EQ (c["country"], "");
	ASSERT_EQ (c["dialect"], "");
	ASSERT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/%/%/%/test");

	c.with<LanguageGermanLayer> ().with<CountryGermanyLayer> () ([&] {
		ASSERT_EQ (c["language"], "german");
		ASSERT_EQ (c["country"], "germany");
		ASSERT_EQ (c["dialect"], "");
		ASSERT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/german/germany/%/test");
	});
	ASSERT_EQ (c["language"], "");
	ASSERT_EQ (c["country"], "");
	ASSERT_EQ (c["dialect"], "");
	ASSERT_EQ (c.evaluate ("/%language%/%country%/%dialect%/test"), "/%/%/%/test");

	KeySet ks;
	Integer i (ks, c, Key ("/%application%/%version%/%profile%/%thread%/%module%/%manufacturer%/%type%/%family%/%model%/serial_number",
			       KEY_CASCADING_NAME, KEY_META, "default", s_value, KEY_END));
	ASSERT_EQ (i.getName (), "/%/%/%/%/%/%/%/%/%/serial_number");
	c.activate<MainApplicationLayer> ();
	ASSERT_EQ (i.getName (), "/main/%/%/%/%/%/%/%/%/serial_number");
	String s (ks, c, Key ("/%x%", KEY_CASCADING_NAME, KEY_META, "default", "anonymous", KEY_END));
	c.activate<ProfileLayer> (s);
	ASSERT_EQ (i.getName (), "/main/%/anonymous/%/%/%/%/%/%/serial_number");
	c.activate<KeyValueLayer> ("module", "M1");
	ASSERT_EQ (i.getName (), "/main/%/anonymous/%/M1/%/%/%/%/serial_number");
	c.activate<KeyValueLayer> ("manufacturer", "hp");
	ASSERT_EQ (i.getName (), "/main/%/anonymous/%/M1/hp/%/%/%/serial_number");
	c.activate<KeyValueLayer> ("family", "EliteBook");
	ASSERT_EQ (i.getName (), "/main/%/anonymous/%/M1/hp/%/EliteBook/%/serial_number");
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
	ASSERT_EQ (o1.counter, 0);
	ASSERT_EQ (o2.counter, 0);
	c.notifyByEvents ({ "event1" });
	ASSERT_EQ (o1.counter, 1);
	ASSERT_EQ (o2.counter, 1);
	c.notifyByEvents ({ "event2" });
	ASSERT_EQ (o1.counter, 2);
	ASSERT_EQ (o2.counter, 1);
	c.notifyByEvents ({ "event3" });
	ASSERT_EQ (o1.counter, 2);
	ASSERT_EQ (o2.counter, 2);
	c.notifyByEvents ({ "event4" });
	ASSERT_EQ (o1.counter, 2);
	ASSERT_EQ (o2.counter, 2);
	c.notifyByEvents ({ "event1", "event2" });
	ASSERT_EQ (o1.counter, 3);
	ASSERT_EQ (o2.counter, 3);
	c.notifyByEvents ({ "event1", "event3" });
	ASSERT_EQ (o1.counter, 4);
	ASSERT_EQ (o2.counter, 4);
	ASSERT_EQ (o3.counter, 0);
	c.notifyAllEvents ();
	ASSERT_EQ (o1.counter, 5);
	ASSERT_EQ (o2.counter, 5);
	ASSERT_EQ (o3.counter, 1);
}


bool fooFirst = true;

//{foo}
void foo (kdb::Integer & e)
{
	e.context ().with<SelectedPrinterLayer> () ([&]() {
		if (fooFirst)
			ASSERT_EQ (e, i_value);
		else
			ASSERT_EQ (e, 20);
		e = 20;
		ASSERT_EQ (e, 20);
	});
	ASSERT_EQ (e, 12);
	fooFirst = false;
}
//{end}

bool barFirst = true;

void bar (kdb::Integer const & e)
{
	e.context ().with<ThreadLayer> ().with<SelectedPrinterLayer> () ([&]() {
		if (barFirst)
			ASSERT_EQ (e, 20);
		else
			ASSERT_EQ (e, i_value);

		e.context ().without<ThreadLayer> () ([&]() { ASSERT_EQ (e, 20); });
	});
	ASSERT_EQ (e, 12);
	barFirst = false;
}

TEST (test_contextual_basic, threads)
{
	using namespace kdb;

	KeySet ks;
	Context c;
	kdb::Integer n (ks, c, Key ("/%thread%/%printer%/test", KEY_CASCADING_NAME, KEY_META, "default", s_value, KEY_END));


	n.context ().activate<MainApplicationLayer> ();
	ASSERT_EQ (n, i_value);

	n = 18;
	ASSERT_EQ (n, 18);

	Integer & d = n;

	d = i_value;
	ASSERT_EQ (d, i_value);

	d = 12;
	ASSERT_EQ (d, 12);

	foo (d);
	ASSERT_EQ (d, 12);

	foo (d);
	ASSERT_EQ (d, 12);

	bar (d);
	ASSERT_EQ (d, 12);

	std::thread t1 (bar, std::cref (d));
	t1.join ();
	ASSERT_EQ (d, 12);

	std::thread t2 (foo, std::ref (d));
	t2.join ();
	ASSERT_EQ (d, 12);
}

TEST (test_contextual_basic, nocontext)
{
	using namespace kdb;
	KeySet ks;
	NoContext c;
	kdb::Value<int> n (ks, c, Key ("/test", KEY_CASCADING_NAME, KEY_META, "default", s_value, KEY_END));
	ASSERT_EQ (n, i_value);

	n = 18;
	ASSERT_EQ (n, 18);
}

TEST (test_contextual_basic, operators)
{
	using namespace kdb;
	KeySet ks;
	NoContext c;
	kdb::Value<int> n (ks, c, Key ("/test/n", KEY_CASCADING_NAME, KEY_META, "default", s_value, KEY_END));
	kdb::Value<int> m (ks, c, Key ("/test/m", KEY_CASCADING_NAME, KEY_META, "default", s_value, KEY_END));
	ASSERT_EQ (n, i_value);
	ASSERT_EQ (m, i_value);

	n = 18;
	ASSERT_EQ (n, 18);
	ASSERT_EQ (18, n);
	ASSERT_EQ (n, n);
	ASSERT_EQ (!n, 0);
	ASSERT_EQ (0, !n);
	ASSERT_EQ (~n, ~18);
	ASSERT_EQ (~18, ~n);

	ASSERT_NE (n, 19);
	ASSERT_NE (19, n);
	ASSERT_NE (!n, n);
	ASSERT_NE (~n, n);

	ASSERT_LT (n, 19);
	ASSERT_GT (n, 17);
	ASSERT_LE (n, 19);
	ASSERT_GE (n, 17);

	ASSERT_LE (n, 18);
	ASSERT_GE (n, 18);

	n = 18;
	m = 18;

	ASSERT_EQ (n, m);
	ASSERT_EQ (m, n);

	n += 3;
	m += 3;

	ASSERT_EQ (n, m);
	ASSERT_EQ (m, n);

	m += n;
	ASSERT_EQ (n, 21);
	ASSERT_EQ (m, 42);

	ASSERT_EQ (n + n, m);
	ASSERT_EQ (m, n + n);

	n--;
	ASSERT_EQ (n, 20);

	ASSERT_EQ (n && n, true);

	n -= 10;
	ASSERT_EQ (n, 10);

	n *= 2;
	ASSERT_EQ (n, 20);

	n /= 2;
	ASSERT_EQ (n, 10);

	n %= 12;
	ASSERT_EQ (n, 10 % 12);

	n = 4 | 8;
	n |= 16;
	ASSERT_EQ (n, 4 | 8 | 16);

	n = 8;
	n = n;
	m = n;
	ASSERT_EQ (n, 8);
	ASSERT_EQ (m, 8);
	ASSERT_EQ (8, n);
	ASSERT_EQ (8, m);

	n = -8;
	m = 8;
	ASSERT_EQ (n, -m);
	ASSERT_EQ (-n, m);
}
