#include <contextual.hpp>

#include <gtest/gtest.h>

class MyStaticGetPolicy
{
public:
	static kdb::Key get(ELEKTRA_UNUSED kdb::KeySet & ks,
		ELEKTRA_UNUSED kdb::Key const& spec)
	{
		kdb::Key k("user/test",
			KEY_VALUE, "23",
			KEY_END);
		return k;
	}
};


TEST(test_contextual_policy, staticGetPolicy)
{
	using namespace kdb;
	KeySet ks;
	Context c;
	ContextualValue<int, GetPolicyIs<MyStaticGetPolicy>> cv
		(ks, c, Key("/test",
			ckdb::KDB_O_CASCADING_NAME,
			KEY_VALUE, "/test",
			KEY_META, "default", "88",
			KEY_END));
	EXPECT_EQ(cv, 23);
	EXPECT_EQ(cv, cv);
	cv = 40;
	EXPECT_EQ(cv, 40);

	ContextualValue<int, GetPolicyIs<MyStaticGetPolicy>> cv2(cv);
	EXPECT_EQ(cv, cv2);

}

class MyDynamicGetPolicy
{
public:
	static kdb::Key get(ELEKTRA_UNUSED kdb::KeySet & ks,
		ELEKTRA_UNUSED kdb::Key const& spec)
	{
		return ksLookupBySpec(ks.getKeySet(), *spec);
	}
};

TEST(test_contextual_policy, dynamicGetPolicy)
{
	using namespace kdb;
	KeySet ks;
	ks.append(Key("user/available",
				KEY_VALUE, "12",
				KEY_END));
	Context c;
	ContextualValue<int, GetPolicyIs<MyDynamicGetPolicy>> cv
		(ks, c, Key("/test",
			ckdb::KDB_O_CASCADING_NAME,
			KEY_META, "default", "88",
			KEY_META, "override/#0", "user/available",
			KEY_END));

	EXPECT_EQ(cv, 12);
	EXPECT_EQ(cv, cv);
	cv = 40;
	EXPECT_EQ(cv, 40);

	ContextualValue<int, GetPolicyIs<MyDynamicGetPolicy>> cv2(cv);
	EXPECT_EQ(cv, cv2);

}

struct RootLayer : kdb::Layer
{
	std::string id() const override
	{
		return "root";
	}
	std::string operator()() const override
	{
		return "root";
	}
};

TEST(test_contextual_policy, root)
{
	using namespace kdb;
	KeySet ks;
	ks.append(Key("user/available",
				KEY_VALUE, "12",
				KEY_END));
	Context c;
	ContextualValue<int, GetPolicyIs<MyDynamicGetPolicy>> cv
		(ks, c, Key("/",
			ckdb::KDB_O_CASCADING_NAME,
			KEY_META, "default", "88",
			KEY_META, "override/#0", "user/available",
			KEY_END));

	EXPECT_EQ(cv, 12);
	EXPECT_EQ(cv, cv);
	cv = 40;
	EXPECT_EQ(cv, 40);
	c.activate<RootLayer>();
	EXPECT_EQ(cv, 40);

	ContextualValue<int, GetPolicyIs<MyDynamicGetPolicy>> cv2(cv);
	EXPECT_EQ(cv, cv2);
}

class MyCV : public kdb::ContextualValue<int, kdb::GetPolicyIs<MyStaticGetPolicy>>
{
public:
	MyCV(kdb::KeySet & ks_, kdb::Context & context_)
		: ContextualValue<int, kdb::GetPolicyIs<MyStaticGetPolicy>>(ks_,
			context_,
			kdb::Key(
				"/",
				ckdb::KDB_O_CASCADING_NAME,
				KEY_END))
	{}
};

TEST(test_contextual_policy, myCVRoot)
{
	using namespace kdb;
	KeySet ks;
	Context c;

	MyCV m(ks, c);

	EXPECT_EQ(m, 23);
}

class none_t
{};

namespace kdb {

template <>
inline void Key::set(none_t)
{}

template <>
inline none_t Key::get() const
{
	none_t ret;
	return ret;
}

}

class MyCV2 : public kdb::ContextualValue<none_t, kdb::GetPolicyIs<MyStaticGetPolicy>>
{
public:
	MyCV2(kdb::KeySet & ks_, kdb::Context & context_)
		: ContextualValue<none_t, kdb::GetPolicyIs<MyStaticGetPolicy>>(ks_,
			context_,
			kdb::Key(
				"/",
				ckdb::KDB_O_CASCADING_NAME,
				KEY_END)),
		m_m(ks_, context_)
	{}

	MyCV m_m;
};

TEST(test_contextual_policy, myCV2Root)
{
	using namespace kdb;
	KeySet ks;
	Context c;

	MyCV2 m(ks, c);
}
