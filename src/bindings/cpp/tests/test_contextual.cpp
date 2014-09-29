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


TEST(test_contextual, staticGetPolicy)
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

TEST(test_contextual, dynamicGetPolicy)
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

TEST(test_contextual, root)
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
}

