#include <tests.hpp>

#include <vector>
#include <algorithm>

#include <gtest/gtest.h>

TEST(test_iter_name, forward)
{
	Key k("user/user\\/key4\\/1/user\\/key4\\/2/user\\/key4\\/3",  KEY_END);

	Key::iterator it = k.begin();
	ASSERT_EQ((*it) ,  "user"    ) <<  "name wrong";
	++it;

	ASSERT_EQ((*it) ,  "user/key4/1"    ) <<  "name wrong";
	++it;
	ASSERT_EQ((*it) ,  "user/key4/2"    ) <<  "name wrong";
	++it;
	ASSERT_EQ((*it) ,  "user/key4/3"    ) <<  "name wrong";
	++it;
	ASSERT_EQ(it ,  k.end()) <<  "not at end";

	--it;
	ASSERT_EQ((*it) ,  "user/key4/3"    ) <<  "name wrong";
	--it;
	ASSERT_EQ((*it) ,  "user/key4/2"    ) <<  "name wrong";
	--it;
	ASSERT_EQ((*it) ,  "user/key4/1"    ) <<  "name wrong";
	--it;
	ASSERT_EQ((*it) ,  "user"    ) <<  "name wrong";
	ASSERT_EQ(it ,  k.begin()) <<  "not at begin";

	it++;
	ASSERT_EQ((*it) ,  "user/key4/1"    ) <<  "name wrong";
	it++;
	ASSERT_EQ((*it) ,  "user/key4/2"    ) <<  "name wrong";
	it++;
	ASSERT_EQ((*it) ,  "user/key4/3"    ) <<  "name wrong";
	it++;
	ASSERT_EQ(it ,  k.end()) <<  "not at end";

	it--;
	ASSERT_EQ((*it) ,  "user/key4/3"    ) <<  "name wrong";
	it--;
	ASSERT_EQ((*it) ,  "user/key4/2"    ) <<  "name wrong";
	it--;
}

TEST(test_iter_name, reverse)
{
	Key k("user/user\\/key4\\/1/user\\/key4\\/2/user\\/key4\\/3",  KEY_END);

	Key::reverse_iterator it = k.rend();
	ASSERT_EQ((*it) ,  ""    ) <<  "name wrong";
	--it;
	ASSERT_EQ((*it) ,  "user"    ) <<  "name wrong";
	--it;

	ASSERT_EQ((*it) ,  "user/key4/1"    ) <<  "name wrong";
	--it;
	ASSERT_EQ((*it) ,  "user/key4/2"    ) <<  "name wrong";
	--it;
	ASSERT_EQ((*it) ,  "user/key4/3"    ) <<  "name wrong";
	// --it; // Misusage, do not go past begin
	ASSERT_EQ(it ,  k.rbegin()) <<  "not at end";

	++it;
	ASSERT_EQ((*it) ,  "user/key4/2"    ) <<  "name wrong";
	++it;
	ASSERT_EQ((*it) ,  "user/key4/1"    ) <<  "name wrong";
	++it;
	ASSERT_EQ((*it) ,  "user"    ) <<  "name wrong";
	++it;
	ASSERT_EQ(it ,  k.rend()) <<  "not at begin";

	it--;
	ASSERT_EQ((*it) ,  "user"    ) <<  "name wrong";
	it--;
	ASSERT_EQ((*it) ,  "user/key4/1"    ) <<  "name wrong";
	it--;
	ASSERT_EQ((*it) ,  "user/key4/2"    ) <<  "name wrong";
	it--;
	ASSERT_EQ((*it) ,  "user/key4/3"    ) <<  "name wrong";
	// it--; // Misusage, do not go past begin
	ASSERT_EQ(it ,  k.rbegin()) <<  "not at end";

	it++;
	ASSERT_EQ((*it) ,  "user/key4/2"    ) <<  "name wrong";
	it++;
	ASSERT_EQ((*it) ,  "user/key4/1"    ) <<  "name wrong";
}
