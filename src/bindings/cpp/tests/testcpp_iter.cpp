/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./tests.hpp"

#include <algorithm>
#include <vector>

#include <gtest/gtest.h>

TEST (test_iter, iterate)
{
	KeySet ks2 (5, *Key ("user:/key2/1", KEY_END), *Key ("user:/key2/2", KEY_END), *Key ("user:/key2/3", KEY_VALUE, "value", KEY_END),
		    KS_END);

	ASSERT_EQ ((*ks2.begin ()).getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ (ks2.begin ()->getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((ks2.begin ()[0])->getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((*(++ks2.begin ())).getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((++ks2.begin ())->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((*(ks2.begin () + 1)).getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((ks2.begin () + 1)->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((ks2.begin ()[1])->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((*(ks2.begin () + 2)).getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((ks2.begin () + 2)->getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((ks2.begin ()[2])->getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((ks2.begin () + 3), ks2.end ()) << "end iterator broken";

	ASSERT_EQ ((*ks2.rbegin ()).getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ (ks2.rbegin ()->getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((ks2.rbegin ()[0])->getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((*(++ks2.rbegin ())).getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((++ks2.rbegin ())->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((*(ks2.rbegin () + 1)).getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((ks2.rbegin () + 1)->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((ks2.rbegin ()[1])->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((*(ks2.rbegin () + 2)).getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((ks2.rbegin () + 2)->getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((ks2.rbegin ()[2])->getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((ks2.rbegin () + 3), ks2.rend ()) << "end iterator broken";

	ASSERT_EQ ((*ks2.cbegin ()).getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ (ks2.cbegin ()->getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((ks2.cbegin ()[0])->getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((*(++ks2.cbegin ())).getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((++ks2.cbegin ())->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((*(ks2.cbegin () + 1)).getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((ks2.cbegin () + 1)->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((ks2.cbegin ()[1])->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((*(ks2.cbegin () + 2)).getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((ks2.cbegin () + 2)->getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((ks2.cbegin ()[2])->getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((ks2.cbegin () + 3), ks2.cend ()) << "end iterator broken";

	ASSERT_EQ ((*ks2.crbegin ()).getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ (ks2.crbegin ()->getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((ks2.crbegin ()[0])->getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((*(++ks2.crbegin ())).getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((++ks2.crbegin ())->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((*(ks2.crbegin () + 1)).getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((ks2.crbegin () + 1)->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((ks2.crbegin ()[1])->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((*(ks2.crbegin () + 2)).getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((ks2.crbegin () + 2)->getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((ks2.crbegin ()[2])->getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((ks2.crbegin () + 3), ks2.crend ()) << "end iterator broken";
}

TEST (test_iter, const_iterate)
{
	// std::cout << "testing const iterate" << std::endl;
	const KeySet ks2 (5, *Key ("user:/key2/1", KEY_END), *Key ("user:/key2/2", KEY_END),
			  *Key ("user:/key2/3", KEY_VALUE, "value", KEY_END), KS_END);

	ASSERT_EQ ((*ks2.begin ()).getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ (ks2.begin ()->getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((ks2.begin ()[0])->getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((*(++ks2.begin ())).getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((++ks2.begin ())->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((*(ks2.begin () + 1)).getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((ks2.begin () + 1)->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((ks2.begin ()[1])->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((*(ks2.begin () + 2)).getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((ks2.begin () + 2)->getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((ks2.begin ()[2])->getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((ks2.begin () + 3), ks2.end ()) << "end iterator broken";

	ASSERT_EQ ((*ks2.rbegin ()).getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ (ks2.rbegin ()->getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((ks2.rbegin ()[0])->getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((*(++ks2.rbegin ())).getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((++ks2.rbegin ())->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((*(ks2.rbegin () + 1)).getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((ks2.rbegin () + 1)->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((ks2.rbegin ()[1])->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((*(ks2.rbegin () + 2)).getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((ks2.rbegin () + 2)->getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((ks2.rbegin ()[2])->getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((ks2.rbegin () + 3), ks2.rend ()) << "end iterator broken";

	ASSERT_EQ ((*ks2.cbegin ()).getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ (ks2.cbegin ()->getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((ks2.cbegin ()[0])->getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((*(++ks2.cbegin ())).getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((++ks2.cbegin ())->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((*(ks2.cbegin () + 1)).getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((ks2.cbegin () + 1)->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((ks2.cbegin ()[1])->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((*(ks2.cbegin () + 2)).getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((ks2.cbegin () + 2)->getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((ks2.cbegin ()[2])->getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((ks2.cbegin () + 3), ks2.cend ()) << "end iterator broken";

	ASSERT_EQ ((*ks2.crbegin ()).getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ (ks2.crbegin ()->getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((ks2.crbegin ()[0])->getName (), "user:/key2/3") << "name wrong";
	ASSERT_EQ ((*(++ks2.crbegin ())).getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((++ks2.crbegin ())->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((*(ks2.crbegin () + 1)).getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((ks2.crbegin () + 1)->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((ks2.crbegin ()[1])->getName (), "user:/key2/2") << "name wrong";
	ASSERT_EQ ((*(ks2.crbegin () + 2)).getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((ks2.crbegin () + 2)->getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((ks2.crbegin ()[2])->getName (), "user:/key2/1") << "name wrong";
	ASSERT_EQ ((ks2.crbegin () + 3), ks2.crend ()) << "end iterator broken";
}

TEST (test_iter, iterator)
{
	// std::cout << "test iterator" << std::endl;
	KeySet ks4 (5, *Key ("user:/key4/1", KEY_END), *Key ("user:/key4/2", KEY_END), *Key ("user:/key4/3", KEY_VALUE, "value", KEY_END),
		    KS_END);

	KeySet::iterator it = ks4.begin ();
	ASSERT_EQ (it->getName (), "user:/key4/1") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/1") << "name wrong";
	it += 1;
	ASSERT_EQ (it->getName (), "user:/key4/2") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/2") << "name wrong";
	it += 1;
	ASSERT_EQ (it->getName (), "user:/key4/3") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/3") << "name wrong";
	it += 1;
	ASSERT_EQ (it, ks4.end ()) << "not at end";
	it -= 1;
	ASSERT_EQ (it->getName (), "user:/key4/3") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/3") << "name wrong";
	it -= 1;
	ASSERT_EQ (it->getName (), "user:/key4/2") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/2") << "name wrong";
	it -= 1;
	ASSERT_EQ (it->getName (), "user:/key4/1") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/1") << "name wrong";
	ASSERT_EQ (it, ks4.begin ()) << "not at begin";


	ASSERT_EQ (it->getName (), "user:/key4/1") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/1") << "name wrong";
	it++;
	ASSERT_EQ (it->getName (), "user:/key4/2") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/2") << "name wrong";
	it++;
	ASSERT_EQ (it->getName (), "user:/key4/3") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/3") << "name wrong";
	it++;
	ASSERT_EQ (it, ks4.end ()) << "not at end";
	it--;
	ASSERT_EQ (it->getName (), "user:/key4/3") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/3") << "name wrong";
	it--;
	ASSERT_EQ (it->getName (), "user:/key4/2") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/2") << "name wrong";
	it--;
}

TEST (test_iter, const_iterator)
{
	// std::cout << "test const iterator" << std::endl;
	const KeySet ks4 (5, *Key ("user:/key4/1", KEY_END), *Key ("user:/key4/2", KEY_END),
			  *Key ("user:/key4/3", KEY_VALUE, "value", KEY_END), KS_END);

	KeySet::iterator it = ks4.begin ();
	ASSERT_EQ (it->getName (), "user:/key4/1") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/1") << "name wrong";
	it += 1;
	ASSERT_EQ (it->getName (), "user:/key4/2") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/2") << "name wrong";
	it += 1;
	ASSERT_EQ (it->getName (), "user:/key4/3") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/3") << "name wrong";
	it += 1;
	ASSERT_EQ (it, ks4.end ()) << "not at end";
	it -= 1;
	ASSERT_EQ (it->getName (), "user:/key4/3") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/3") << "name wrong";
	it -= 1;
	ASSERT_EQ (it->getName (), "user:/key4/2") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/2") << "name wrong";
	it -= 1;
	ASSERT_EQ (it->getName (), "user:/key4/1") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/1") << "name wrong";
	ASSERT_EQ (it, ks4.begin ()) << "not at begin";


	ASSERT_EQ (it->getName (), "user:/key4/1") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/1") << "name wrong";
	it++;
	ASSERT_EQ (it->getName (), "user:/key4/2") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/2") << "name wrong";
	it++;
	ASSERT_EQ (it->getName (), "user:/key4/3") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/3") << "name wrong";
	it++;
	ASSERT_EQ (it, ks4.end ()) << "not at end";
	it--;
	ASSERT_EQ (it->getName (), "user:/key4/3") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/3") << "name wrong";
	it--;
	ASSERT_EQ (it->getName (), "user:/key4/2") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/2") << "name wrong";
	it--;

	ASSERT_EQ (it->getName (), "user:/key4/1") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/1") << "name wrong";
	++it;
	ASSERT_EQ (it->getName (), "user:/key4/2") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/2") << "name wrong";
	++it;
	ASSERT_EQ (it->getName (), "user:/key4/3") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/3") << "name wrong";
	++it;
	ASSERT_EQ (it, ks4.end ()) << "not at end";
	--it;
	ASSERT_EQ (it->getName (), "user:/key4/3") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/3") << "name wrong";
	--it;
	ASSERT_EQ (it->getName (), "user:/key4/2") << "name wrong";
	ASSERT_EQ ((*it).getName (), "user:/key4/2") << "name wrong";
	--it;
}
