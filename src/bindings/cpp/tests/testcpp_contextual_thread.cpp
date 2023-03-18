/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbthread.hpp>

#include <internal/kdbprivate.h>

#include <gtest/gtest.h>

using namespace kdb;

void foo1 (Coordinator & gc, KeySet & ks)
{
	Key specKey ("/hello", KEY_END);

	ThreadContext c1 (gc);
	ThreadValue<int> v1 (ks, c1, specKey);
	ASSERT_EQ (v1, 8);
	ASSERT_EQ (ks.lookup ("user:/hello").getString (), "8");

	v1 = 5;
	ASSERT_EQ (v1, 5);
	ASSERT_EQ (ks.lookup ("user:/hello").getString (), "5");

	std::this_thread::sleep_for (std::chrono::milliseconds (100));
	ASSERT_EQ (v1, 5);
}

void foo2 (Coordinator & gc, KeySet & ks)
{
	Key specKey ("/hello", KEY_END);

	ThreadContext c2 (gc);
	ThreadValue<int> v2 (ks, c2, specKey);
	ASSERT_EQ (v2, 5);

	std::this_thread::sleep_for (std::chrono::milliseconds (100));
	c2.syncLayers ();
	ASSERT_EQ (v2, 5);

	v2 = 12;
	ASSERT_EQ (v2, 12);
}

TEST (test_contextual_thread, instanciation)
{
	Key specKey ("/hello", KEY_END);

	KeySet ks;
	ks.append (Key ("user:/hello", KEY_VALUE, "22", KEY_END));

	Coordinator gc;
	ThreadContext c (gc);
	ThreadValue<int> v (ks, c, specKey);
	ASSERT_EQ (v, 22);
	ASSERT_EQ (ks.lookup ("user:/hello").getString (), "22");

	v = 8;
	ASSERT_EQ (v, 8);
	ASSERT_EQ (ks.lookup ("user:/hello").getString (), "8");

	std::thread t1 (foo1, std::ref (gc), std::ref (ks));

	std::this_thread::sleep_for (std::chrono::milliseconds (50));
	ASSERT_EQ (ks.lookup ("user:/hello").getString (), "5");
	c.syncLayers ();
	ASSERT_TRUE (ks.lookup ("user:/hello"));
	ASSERT_EQ (ks.lookup ("user:/hello").getString (), "5");
	ASSERT_EQ (v.getName (), "user:/hello");
	ASSERT_EQ (ks.size (), 1);
	ASSERT_EQ (v, 5);

	std::thread t2 (foo2, std::ref (gc), std::ref (ks));
	t1.join ();
	t2.join ();

	c.syncLayers ();
	ASSERT_EQ (v.getName (), "user:/hello");
	ASSERT_EQ (ks.lookup ("user:/hello").getString (), "12");
	ASSERT_EQ (ks.size (), 1);
	ASSERT_EQ (v, 12);
}

class Other : public kdb::Layer
{
public:
	std::string id () const override
	{
		return "other";
	}
	std::string operator() () const override
	{
		return "notused";
	}
};

class Activate : public kdb::Layer
{
public:
	std::string id () const override
	{
		return "activate";
	}
	std::string operator() () const override
	{
		return "active";
	}
};


bool g_toggle = false;

void toggleOn ()
{
	g_toggle = true;
}

void toggleOff ()
{
	g_toggle = false;
}

void activate1 (Coordinator & gc, KeySet & ks)
{
	Key specKey ("/act/%activate%", KEY_END);

	ThreadContext c1 (gc);
	ThreadValue<int> v1 (ks, c1, specKey);
	gc.onLayerActivation<Activate> ([] () { toggleOn (); });
	gc.onLayerDeactivation<Activate> ([] () { toggleOff (); });
	ASSERT_EQ (v1, 10);
	c1.activate<Activate> ();
	ASSERT_TRUE (g_toggle);
	ASSERT_EQ (v1, 22);
	std::this_thread::sleep_for (std::chrono::milliseconds (200));
	ASSERT_EQ (v1, 22);
	c1.deactivate<Activate> ();
	ASSERT_FALSE (g_toggle);
	ASSERT_EQ (v1, 10);
}

#if defined(__APPLE__)
TEST (DISABLED_test_contextual_thread, activate)
#else
TEST (test_contextual_thread, activate)
#endif
{
	Key specKey ("/act/%activate%", KEY_END);

	KeySet ks;
	ks.append (Key ("user:/act/%", KEY_VALUE, "10", KEY_END)); // not active layer
	ks.append (Key ("user:/act/active", KEY_VALUE, "22", KEY_END));

	Coordinator gc;
	ThreadContext c (gc);
	ThreadValue<int> v (ks, c, specKey);
	ASSERT_EQ (v, 10);
	ASSERT_EQ (c.size (), 0);
	ASSERT_EQ (c["other"], "");
	ASSERT_EQ (c["activate"], "");

	std::thread t1 (activate1, std::ref (gc), std::ref (ks));
	ASSERT_EQ (v, 10);
	std::this_thread::sleep_for (std::chrono::milliseconds (100));
	ASSERT_TRUE (g_toggle);
	c.syncLayers ();
	ASSERT_EQ (v, 22);
	t1.join ();
	ASSERT_FALSE (g_toggle);
	ASSERT_EQ (v, 22);
	c.activate<Other> (); // also fetches updates
	ASSERT_EQ (c.size (), 1);
	ASSERT_EQ (c["other"], "notused");
	ASSERT_EQ (c["activate"], "");
	ASSERT_EQ (v, 10);
}

const uint32_t i_value = 55;
const char * s_value = "55";

TEST (test_contextual_thread, ThreadNoContext)
{
	using namespace kdb;
	KeySet ks;
	ThreadNoContext c;
	const char * name = "/%language%/%country%/%dialect%/test";
	ASSERT_TRUE (!ks.lookup (name));
	Value<int, ContextPolicyIs<ThreadNoContext>> i (ks, c, Key (name, KEY_META, "default", s_value, KEY_END));
	ASSERT_EQ (i, i_value);
	ASSERT_TRUE (ks.lookup (name));
	i = 5;
	ASSERT_EQ (i, 5);
	ASSERT_EQ (i.getSpec ().getName (), name);
	i.syncKeySet ();
	ASSERT_EQ (ks.lookup (name).getString (), "5");
	i = 10;
	ASSERT_EQ (i, 10);
	ASSERT_EQ (ks.lookup (name).getString (), "10");
}


/**
 * @brief Dependent on layer Activate
 */
class Dep : public kdb::Layer
{
public:
	explicit Dep (ThreadValue<int> const & i) : m_i (std::to_string (static_cast<int> (i)))
	{
		// capture current value of contextual value here
	}
	std::string id () const override
	{
		return "dep";
	}
	std::string operator() () const override
	{
		return m_i;
	}
	std::string m_i;
};


TEST (test_contextual_thread, activateNoDependency)
{
	Key specKey ("/act/%activate%", KEY_END);

	KeySet ks;
	ks.append (Key ("user:/act/%", KEY_VALUE, "10", KEY_END)); // not active layer
	ks.append (Key ("user:/act/active", KEY_VALUE, "22", KEY_END));

	Coordinator gc;
	ThreadContext c1 (gc);
	ThreadContext c2 (gc);
	ThreadValue<int> v (ks, c1, specKey);
	ASSERT_EQ (v, 10);
	ASSERT_EQ (c1.size (), 0);
	ASSERT_EQ (c1["other"], "");
	ASSERT_EQ (c1["activate"], "");

	c2.activate<Dep> (v);
	ASSERT_EQ (c2.size (), 1);
	ASSERT_EQ (c2["dep"], "10");
	c2.activate<Activate> ();
	ASSERT_EQ (c2.size (), 2);
	ASSERT_EQ (c2["dep"], "10");
	ASSERT_EQ (c2["activate"], "active");

	c1.syncLayers ();
	ASSERT_EQ (c1["dep"], "10");
	ASSERT_EQ (c2["activate"], "active");
	ASSERT_EQ (v.getName (), "user:/act/active");
	ASSERT_EQ (v, 22);
}


TEST (test_contextual_thread, activateWithDependency)
{
	Key specKey ("/act/%activate%", KEY_END);

	KeySet ks;
	ks.append (Key ("user:/act/%", KEY_VALUE, "10", KEY_END)); // not active layer
	ks.append (Key ("user:/act/active", KEY_VALUE, "22", KEY_END));

	Coordinator gc;
	ThreadContext c1 (gc);
	ThreadContext c2 (gc);
	ThreadValue<int> v1 (ks, c1, specKey);
	ThreadValue<int> v2 (ks, c2, specKey);
	ASSERT_EQ (v1, 10);
	ASSERT_EQ (v2, 10);

	ASSERT_EQ (c1.size (), 0);
	ASSERT_EQ (c1["other"], "");
	ASSERT_EQ (c1["activate"], "");

	ASSERT_EQ (c2.size (), 0);
	ASSERT_EQ (c2["other"], "");
	ASSERT_EQ (c2["activate"], "");

	c2.activate<Activate> ();
	ASSERT_EQ (c2.size (), 1);
	ASSERT_EQ (c2["activate"], "active");

	ASSERT_EQ (v1, 10);
	ASSERT_EQ (v2, 22);

	c1.activate<Dep> (v1);
	ASSERT_EQ (c1.size (), 2);
	ASSERT_EQ (c1["dep"], "22") << "dependency not correct, activate does not do syncLayer";
	ASSERT_EQ (c1["activate"], "active");
	ASSERT_EQ (c2["activate"], "active");
}

class StrDep : public kdb::Layer
{
public:
	explicit StrDep (ThreadValue<std::string> const & i) : m_i (i)
	{
	}
	std::string id () const override
	{
		return "dep";
	}
	std::string operator() () const override
	{
		return m_i;
	}
	std::string m_i;
};


TEST (test_contextual_thread, activateWithDirectDependency)
{
	Key specKey ("/act/%activate%", KEY_END);

	KeySet ks;
	ks.append (Key ("user:/act/%", KEY_VALUE, "inactive", KEY_END));
	ks.append (Key ("user:/act/active", KEY_VALUE, "active", KEY_END));

	Coordinator gc;
	ThreadContext c1 (gc);
	ThreadContext c2 (gc);
	ThreadValue<std::string> v1 (ks, c1, specKey);
	ThreadValue<std::string> v2 (ks, c2, specKey);
	ThreadValue<std::string> vd (ks, c2, specKey);
	ASSERT_EQ (std::string (v1), "inactive");
	ASSERT_EQ (std::string (v2), "inactive");
	ASSERT_EQ (std::string (vd), "inactive");

	c1.activate<Activate> ();
	ASSERT_EQ (std::string (v1), "active");
	ASSERT_EQ (std::string (v2), "inactive");
	ASSERT_EQ (std::string (vd), "inactive");

	c2.activate<StrDep> (vd);
	ASSERT_EQ (std::string (v1), "active");
	ASSERT_EQ (std::string (v2), "active");
	ASSERT_EQ (std::string (vd), "active");
}


TEST (test_contextual_thread, syncInWith)
{
	Key specKey ("/act/%activate%", KEY_END);

	KeySet ks;
	ks.append (Key ("user:/act/%", KEY_VALUE, "10", KEY_END)); // not active layer
	ks.append (Key ("user:/act/active", KEY_VALUE, "22", KEY_END));

	Coordinator gc;
	ThreadContext c1 (gc);
	ThreadContext c2 (gc);
	ThreadValue<int> v (ks, c1, specKey);
	ASSERT_EQ (v, 10);
	ASSERT_EQ (c1.size (), 0);
	ASSERT_EQ (c1["other"], "");
	ASSERT_EQ (c1["activate"], "");

	c2.activate<Activate> ();
	ASSERT_EQ (c2.size (), 1);
	ASSERT_EQ (c2["activate"], "active");

	c1.with<Other> () ([&] () {
		ASSERT_EQ (c1.size (), 1);
		ASSERT_EQ (c1["other"], "notused");
		ASSERT_EQ (c1["activate"], "");
		ASSERT_EQ (v.getName (), "user:/act/%");
		ASSERT_EQ (v, 10);

		c1.syncLayers ();

		ASSERT_EQ (c1.size (), 2);
		ASSERT_EQ (c1["other"], "notused");
		ASSERT_EQ (c1["activate"], "active");
		ASSERT_EQ (v.getName (), "user:/act/active");
		ASSERT_EQ (v, 22);
	});

	ASSERT_EQ (c1.size (), 1);
	ASSERT_EQ (c1["other"], "");
	ASSERT_EQ (c1["activate"], "active");
	ASSERT_EQ (v.getName (), "user:/act/active");
	ASSERT_EQ (v, 22);
}


TEST (test_contextual_thread, syncBeforeWith)
{
	Key specKey ("/act/%activate%", KEY_END);

	KeySet ks;
	ks.append (Key ("user:/act/%", KEY_VALUE, "10", KEY_END)); // not active layer
	ks.append (Key ("user:/act/active", KEY_VALUE, "22", KEY_END));

	Coordinator gc;
	ThreadContext c1 (gc);
	ThreadContext c2 (gc);
	ThreadValue<int> v (ks, c1, specKey);
	ASSERT_EQ (v, 10);
	ASSERT_EQ (c1.size (), 0);
	ASSERT_EQ (c1["other"], "");
	ASSERT_EQ (c1["activate"], "");

	c2.activate<Activate> ();
	ASSERT_EQ (c2.size (), 1);
	ASSERT_EQ (c2["activate"], "active");

	c1.syncLayers ();
	ASSERT_EQ (c1.size (), 1);
	ASSERT_EQ (c1["other"], "");
	ASSERT_EQ (c1["activate"], "active");
	ASSERT_EQ (v.getName (), "user:/act/active");
	ASSERT_EQ (v, 22);

	c1.with<Other> () ([&] () {
		ASSERT_EQ (c1.size (), 2);
		ASSERT_EQ (c1["other"], "notused");
		ASSERT_EQ (c1["activate"], "active");
		ASSERT_EQ (v.getName (), "user:/act/active");
		ASSERT_EQ (v, 22);
	});

	ASSERT_EQ (c1.size (), 1);
	ASSERT_EQ (c1["other"], "");
	ASSERT_EQ (c1["activate"], "active");
	ASSERT_EQ (v.getName (), "user:/act/active");
	ASSERT_EQ (v, 22);
}
