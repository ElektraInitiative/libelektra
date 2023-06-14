/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbcontext.hpp>

#include <internal/macros/attributes.h>

#include <gtest/gtest.h>


class MyStaticGetPolicy
{
public:
	static kdb::Key get (ELEKTRA_UNUSED kdb::KeySet & ks, ELEKTRA_UNUSED kdb::Key const & spec)
	{
		return kdb::Key ("user:/something", KEY_VALUE, "23", KEY_END);
	}
};


TEST (test_contextual_policy, staticGetPolicy)
{
	using namespace kdb;
	KeySet ks;
	Context c;
	// clang-format off
	ContextualValue<int, GetPolicyIs<MyStaticGetPolicy>> cv
		(ks, c, Key("/test",
			KEY_VALUE, "/test",
			KEY_META, "default", "88",
			KEY_END));
	// clang-format on
	EXPECT_EQ (cv, 23);
	EXPECT_EQ (cv, cv);
	cv = 40;
	EXPECT_EQ (cv, 40);

	ContextualValue<int, GetPolicyIs<MyStaticGetPolicy>> cv2 (cv);
	EXPECT_EQ (cv, cv2);
}

template <typename T, typename PolicySetter1 = kdb::DefaultPolicyArgs, typename PolicySetter2 = kdb::DefaultPolicyArgs,
	  typename PolicySetter3 = kdb::DefaultPolicyArgs, typename PolicySetter4 = kdb::DefaultPolicyArgs,
	  typename PolicySetter5 = kdb::GetPolicyIs<MyStaticGetPolicy>,
	  typename PolicySetter6 = kdb::DefaultPolicyArgs // unused, for policySelector
	  >
class ValueWrapper : public kdb::ContextualValue<T, PolicySetter1, PolicySetter2, PolicySetter3, PolicySetter4, PolicySetter5>
{
public:
	typedef kdb::PolicySelector<PolicySetter1, PolicySetter2, PolicySetter3, PolicySetter4, PolicySetter5, PolicySetter6> Policies;

	ValueWrapper<T, PolicySetter1, PolicySetter2, PolicySetter3, PolicySetter4, PolicySetter5, PolicySetter6> (kdb::KeySet & ks,
														   kdb::Context & context_,
														   kdb::Key spec)
	: kdb::ContextualValue<T, PolicySetter1, PolicySetter2, PolicySetter3, PolicySetter4, PolicySetter5> (ks, context_, spec),
	  value (ks, context_, spec)
	{
	}

	kdb::ContextualValue<T, PolicySetter1, PolicySetter2, PolicySetter3, PolicySetter4, PolicySetter5> value;

	using kdb::ContextualValue<T, PolicySetter1, PolicySetter2, PolicySetter3, PolicySetter4, PolicySetter5>::operator=;
};

TEST (test_contextual_policy, ValueWrapper)
{
	using namespace kdb;
	KeySet ks;
	Context c;
	// clang-format off
	ValueWrapper<int, WritePolicyIs<DefaultWritePolicy>> cv
		(ks, c, Key("/test",
			KEY_VALUE, "/test",
			KEY_META, "default", "88",
			KEY_END));
	// clang-format on
	EXPECT_EQ (cv.value, 23);
	EXPECT_EQ (cv.value, cv.value);
	cv.value = 40;
	EXPECT_EQ (cv.value, 40);

	EXPECT_EQ (cv, 23);
	EXPECT_EQ (cv, cv);
	cv = 40;
	EXPECT_EQ (cv, 40);
}

template <typename T>
class MySetPolicy
{
public:
	typedef T type;
	static kdb::Key set (kdb::KeySet & ks, kdb::Key const & spec)
	{
		return kdb::DefaultSetPolicy::setWithNamespace (ks, spec, "dir:");
	}
};

TEST (test_contextual_policy, setPolicy)
{
	using namespace kdb;
	KeySet ks;
	Context c;
	// clang-format off
	ContextualValue<int, SetPolicyIs<MySetPolicy<int>>> cv
		(ks, c, Key("/test",
			KEY_VALUE, "/test",
			KEY_META, "default", "88",
			KEY_END));
	// clang-format on
	EXPECT_EQ (cv, 88);
	EXPECT_EQ (cv, 88);
	EXPECT_TRUE (ks.lookup ("/test")) << "did not find /test";
	EXPECT_FALSE (ks.lookup ("dir:/test")) << "found dir:/test wrongly";
	cv = 40;
	EXPECT_EQ (cv, 40);
	cv.syncKeySet ();
	EXPECT_EQ (cv, 40);
	EXPECT_TRUE (ks.lookup ("/test")) << "did not find /test";
	EXPECT_TRUE (ks.lookup ("dir:/test")) << "could not find dir:/test";
}

TEST (test_contextual_policy, readonlyPolicy)
{
	using namespace kdb;
	KeySet ks;
	Context c;
	ContextualValue<int, WritePolicyIs<ReadOnlyPolicy>> cv (ks, c,
								Key ("/test", KEY_VALUE, "/test", KEY_META, "default", "88", KEY_END));
	EXPECT_EQ (cv, 88);
	EXPECT_EQ (cv, 88);
	// cv = 40; // read only, so this is a compile error
}


class MyDynamicGetPolicy
{
public:
	static kdb::Key get (ELEKTRA_UNUSED kdb::KeySet & ks, kdb::Key const & spec)
	{
		return ksLookup (ks.getKeySet (), *spec, ckdb::KDB_O_SPEC);
	}
};

TEST (test_contextual_policy, dynamicGetPolicy)
{
	using namespace kdb;
	KeySet ks;
	ks.append (Key ("user:/available", KEY_VALUE, "12", KEY_END));
	Context c;
	// clang-format off
	ContextualValue<int, GetPolicyIs<MyDynamicGetPolicy>> cv
		(ks, c, Key("/test",
			KEY_META, "default", "88",
			KEY_META, "override/#0", "user:/available",
			KEY_END));
	// clang-format on

	EXPECT_EQ (cv, 12);
	EXPECT_EQ (cv, cv);
	cv = 40;
	EXPECT_EQ (cv, 40);

	ContextualValue<int, GetPolicyIs<MyDynamicGetPolicy>> cv2 (cv);
	EXPECT_EQ (cv, cv2);
}

struct RootLayer : kdb::Layer
{
	std::string id () const override
	{
		return "root";
	}
	std::string operator() () const override
	{
		return "root";
	}
};

TEST (test_contextual_policy, root)
{
	using namespace kdb;
	KeySet ks;
	ks.append (Key ("user:/available", KEY_VALUE, "12", KEY_END));
	Context c;
	// clang-format off
	ContextualValue<int, GetPolicyIs<MyDynamicGetPolicy>> cv
		(ks, c, Key("/",
			KEY_META, "default", "88",
			KEY_META, "override/#0", "user:/available",
			KEY_END));
	// clang-format on

	EXPECT_EQ (cv, 12);
	EXPECT_EQ (cv, cv);
	cv = 40;
	EXPECT_EQ (cv, 40);
	c.activate<RootLayer> ();
	EXPECT_EQ (cv, 40);

	ContextualValue<int, GetPolicyIs<MyDynamicGetPolicy>> cv2 (cv);
	EXPECT_EQ (cv, cv2);
}


template <typename T, typename P = kdb::GetPolicyIs<MyStaticGetPolicy>>
class MyCV : public kdb::ContextualValue<T, P>
{
public:
	MyCV<T, P> (kdb::KeySet & ks_, kdb::Context & context_) : kdb::ContextualValue<T, P> (ks_, context_, kdb::Key ("/", KEY_END))
	{
	}
};

TEST (test_contextual_policy, myCVRoot)
{
	using namespace kdb;
	KeySet ks;
	Context c;

	MyCV<int> m (ks, c);

	EXPECT_EQ (m, 23);
}

class MyNoneGetPolicy
{
public:
	static kdb::Key get (ELEKTRA_UNUSED kdb::KeySet & ks, ELEKTRA_UNUSED kdb::Key const & spec)
	{
		return kdb::Key ();
	}
};

class MyCV2 : public kdb::ContextualValue<kdb::none_t, kdb::GetPolicyIs<MyNoneGetPolicy>>
{
public:
	MyCV2 (kdb::KeySet & ks_, kdb::Context & context_)
	: kdb::ContextualValue<kdb::none_t, kdb::GetPolicyIs<MyNoneGetPolicy>> (ks_, context_, kdb::Key ("/", KEY_END)), m_m (ks_, context_)
	{
	}

	MyCV<int> m_m;
};

TEST (test_contextual_policy, myCV2Root)
{
	using namespace kdb;
	KeySet ks;
	Context c;

	MyCV2 m (ks, c);
}
