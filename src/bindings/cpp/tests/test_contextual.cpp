#include <contextual.hpp>

#include <gtest/gtest.h>

class MyTestGetPolicy
{
public:
	static kdb::Key get(ELEKTRA_UNUSED kdb::KeySet &ks,
		ELEKTRA_UNUSED std::string const & name)
	{
		kdb::Key k("user/test",
			KEY_VALUE, "23",
			KEY_END);
		return k;
	}
};


TEST(test_contextual, policy)
{
	using namespace kdb;
	KeySet ks;
	Context c;
	ContextualValue<int, GetPolicyIs<MyTestGetPolicy>> cv
		(ks, c, Key("",
			KEY_VALUE, "/test",
			KEY_META, "default", "88",
			KEY_END));

	EXPECT_EQ(cv, 23);
	EXPECT_EQ(cv, cv);
	cv = 40;
	EXPECT_EQ(cv, 40);

}
