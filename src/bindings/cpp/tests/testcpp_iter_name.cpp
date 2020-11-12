/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests.hpp>

#include <algorithm>
#include <vector>

#include <gtest/gtest.h>

TEST (test_iter_name, forward)
{
	Key k ("user:/user\\/key4\\/1/user\\/key4\\/2/user\\/key4\\/3", KEY_END);

	Key::iterator it = k.begin ();
	EXPECT_EQ ((*it), std::string{ KEY_NS_USER }) << "name wrong";
	++it;

	EXPECT_EQ ((*it), "user/key4/1") << "name wrong";
	++it;
	EXPECT_EQ ((*it), "user/key4/2") << "name wrong";
	++it;
	EXPECT_EQ ((*it), "user/key4/3") << "name wrong";
	++it;
	EXPECT_EQ (it, k.end ()) << "not at end";

	--it;
	EXPECT_EQ ((*it), "user/key4/3") << "name wrong";
	--it;
	EXPECT_EQ ((*it), "user/key4/2") << "name wrong";
	--it;
	EXPECT_EQ ((*it), "user/key4/1") << "name wrong";
	--it;
	EXPECT_EQ ((*it), std::string{ KEY_NS_USER }) << "name wrong";
	EXPECT_EQ (it, k.begin ()) << "not at begin";

	it++;
	EXPECT_EQ ((*it), "user/key4/1") << "name wrong";
	it++;
	EXPECT_EQ ((*it), "user/key4/2") << "name wrong";
	it++;
	EXPECT_EQ ((*it), "user/key4/3") << "name wrong";
	it++;
	EXPECT_EQ (it, k.end ()) << "not at end";

	it--;
	EXPECT_EQ ((*it), "user/key4/3") << "name wrong";
	it--;
	EXPECT_EQ ((*it), "user/key4/2") << "name wrong";
	it--;
}

TEST (test_iter_name, reverse)
{
	Key k ("user:/user\\/key4\\/1/user\\/key4\\/2/user\\/key4\\/3", KEY_END);

	Key::reverse_iterator it = k.rend ();
	EXPECT_EQ ((*it), "") << "name wrong";
	--it;
	EXPECT_EQ ((*it), std::string{ KEY_NS_USER }) << "name wrong";
	--it;

	EXPECT_EQ ((*it), "user/key4/1") << "name wrong";
	--it;
	EXPECT_EQ ((*it), "user/key4/2") << "name wrong";
	--it;
	EXPECT_EQ ((*it), "user/key4/3") << "name wrong";
	// --it; // Misusage, do not go past begin
	EXPECT_EQ (it, k.rbegin ()) << "not at end";

	++it;
	EXPECT_EQ ((*it), "user/key4/2") << "name wrong";
	++it;
	EXPECT_EQ ((*it), "user/key4/1") << "name wrong";
	++it;
	EXPECT_EQ ((*it), std::string{ KEY_NS_USER }) << "name wrong";
	++it;
	EXPECT_EQ (it, k.rend ()) << "not at begin";

	it--;
	EXPECT_EQ ((*it), std::string{ KEY_NS_USER }) << "name wrong";
	it--;
	EXPECT_EQ ((*it), "user/key4/1") << "name wrong";
	it--;
	EXPECT_EQ ((*it), "user/key4/2") << "name wrong";
	it--;
	EXPECT_EQ ((*it), "user/key4/3") << "name wrong";
	// it--; // Misusage, do not go past begin
	EXPECT_EQ (it, k.rbegin ()) << "not at end";

	it++;
	EXPECT_EQ ((*it), "user/key4/2") << "name wrong";
	it++;
	EXPECT_EQ ((*it), "user/key4/1") << "name wrong";
}

TEST (iterNameCascading, forward)
{
	Key k ("/\\/key4\\/1/\\/key4\\/2/\\/key4\\/3", KEY_END);

	Key::iterator it = k.begin ();
	EXPECT_EQ ((*it), std::string{ KEY_NS_CASCADING }) << "cascading name wrong";
	++it;

	EXPECT_EQ ((*it), "/key4/1") << "name wrong";
	++it;
	EXPECT_EQ ((*it), "/key4/2") << "name wrong";
	++it;
	EXPECT_EQ ((*it), "/key4/3") << "name wrong";
	++it;
	EXPECT_EQ (it, k.end ()) << "not at end";

	--it;
	EXPECT_EQ ((*it), "/key4/3") << "name wrong";
	--it;
	EXPECT_EQ ((*it), "/key4/2") << "name wrong";
	--it;
	EXPECT_EQ ((*it), "/key4/1") << "name wrong";
	--it;
	EXPECT_EQ ((*it), std::string{ KEY_NS_CASCADING }) << "name wrong";
	EXPECT_EQ (it, k.begin ()) << "not at begin";

	it++;
	EXPECT_EQ ((*it), "/key4/1") << "name wrong";
	it++;
	EXPECT_EQ ((*it), "/key4/2") << "name wrong";
	it++;
	EXPECT_EQ ((*it), "/key4/3") << "name wrong";
	it++;
	EXPECT_EQ (it, k.end ()) << "not at end";

	it--;
	EXPECT_EQ ((*it), "/key4/3") << "name wrong";
	it--;
	EXPECT_EQ ((*it), "/key4/2") << "name wrong";
	it--;
}

TEST (iterNameCascading, reverse)
{
	Key k ("/\\/key4\\/1/\\/key4\\/2/\\/key4\\/3", KEY_END);

	Key::reverse_iterator it = k.rend ();
	EXPECT_EQ ((*it), "") << "name wrong";
	--it;
	EXPECT_EQ ((*it), std::string{ KEY_NS_CASCADING }) << "name wrong";
	--it;

	EXPECT_EQ ((*it), "/key4/1") << "name wrong";
	--it;
	EXPECT_EQ ((*it), "/key4/2") << "name wrong";
	--it;
	EXPECT_EQ ((*it), "/key4/3") << "name wrong";
	// --it; // Misusage, do not go past begin
	EXPECT_EQ (it, k.rbegin ()) << "not at end";

	++it;
	EXPECT_EQ ((*it), "/key4/2") << "name wrong";
	++it;
	EXPECT_EQ ((*it), "/key4/1") << "name wrong";
	++it;
	EXPECT_EQ ((*it), std::string{ KEY_NS_CASCADING }) << "name wrong";
	++it;
	EXPECT_EQ (it, k.rend ()) << "not at begin";

	it--;
	EXPECT_EQ ((*it), std::string{ KEY_NS_CASCADING }) << "name wrong";
	it--;
	EXPECT_EQ ((*it), "/key4/1") << "name wrong";
	it--;
	EXPECT_EQ ((*it), "/key4/2") << "name wrong";
	it--;
	EXPECT_EQ ((*it), "/key4/3") << "name wrong";
	// it--; // Misusage, do not go past begin
	EXPECT_EQ (it, k.rbegin ()) << "not at end";

	it++;
	EXPECT_EQ ((*it), "/key4/2") << "name wrong";
	it++;
	EXPECT_EQ ((*it), "/key4/1") << "name wrong";
}
