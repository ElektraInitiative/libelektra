#include <tests.hpp>

#include <vector>
#include <algorithm>

#include <gtest/gtest.h>

TEST(test_iter, iterate)
{
	KeySet ks2 (5,
		*Key ("user/key2/1", KEY_END),
		*Key ("user/key2/2", KEY_END),
		*Key ("user/key2/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	ASSERT_EQ((*ks2.begin()).getName() ,  "user/key2/1"    ) <<  "name wrong";
	ASSERT_EQ(ks2.begin()->getName() ,  "user/key2/1"      ) <<  "name wrong";
	ASSERT_EQ((ks2.begin()[0])->getName() ,  "user/key2/1"  ) << "name wrong";
	ASSERT_EQ((*(++ks2.begin())).getName() ,  "user/key2/2") <<  "name wrong";
	ASSERT_EQ((++ks2.begin())->getName() ,  "user/key2/2"  ) <<  "name wrong";
	ASSERT_EQ((*(ks2.begin()+1)).getName() ,  "user/key2/2") <<  "name wrong";
	ASSERT_EQ((ks2.begin()+1)->getName() ,  "user/key2/2"  ) <<  "name wrong";
	ASSERT_EQ((ks2.begin()[1])->getName() ,  "user/key2/2"  ) << "name wrong";
	ASSERT_EQ((*(ks2.begin()+2)).getName() ,  "user/key2/3") <<  "name wrong";
	ASSERT_EQ((ks2.begin()+2)->getName() ,  "user/key2/3"  ) <<  "name wrong";
	ASSERT_EQ((ks2.begin()[2])->getName() ,  "user/key2/3"  ) << "name wrong";
	ASSERT_EQ((ks2.begin()+3) ,  ks2.end()  ) <<  "end iterator broken";

	ASSERT_EQ((*ks2.rbegin()).getName() ,  "user/key2/3"    ) <<  "name wrong";
	ASSERT_EQ(ks2.rbegin()->getName() ,  "user/key2/3"      ) <<  "name wrong";
	ASSERT_EQ((ks2.rbegin()[0])->getName() ,  "user/key2/3"  ) << "name wrong";
	ASSERT_EQ((*(++ks2.rbegin())).getName() ,  "user/key2/2") <<  "name wrong";
	ASSERT_EQ((++ks2.rbegin())->getName() ,  "user/key2/2"  ) <<  "name wrong";
	ASSERT_EQ((*(ks2.rbegin()+1)).getName() ,  "user/key2/2") <<  "name wrong";
	ASSERT_EQ((ks2.rbegin()+1)->getName() ,  "user/key2/2"  ) <<  "name wrong";
	ASSERT_EQ((ks2.rbegin()[1])->getName() ,  "user/key2/2"  ) << "name wrong";
	ASSERT_EQ((*(ks2.rbegin()+2)).getName() ,  "user/key2/1") <<  "name wrong";
	ASSERT_EQ((ks2.rbegin()+2)->getName() ,  "user/key2/1"  ) <<  "name wrong";
	ASSERT_EQ((ks2.rbegin()[2])->getName() ,  "user/key2/1"  ) << "name wrong";
	ASSERT_EQ((ks2.rbegin()+3) ,  ks2.rend()  ) <<  "end iterator broken";

#if __cplusplus > 199711L
	ASSERT_EQ((*ks2.cbegin()).getName() ,  "user/key2/1"    ) <<  "name wrong";
	ASSERT_EQ(ks2.cbegin()->getName() ,  "user/key2/1"      ) <<  "name wrong";
	ASSERT_EQ((ks2.cbegin()[0])->getName() ,  "user/key2/1"  ) << "name wrong";
	ASSERT_EQ((*(++ks2.cbegin())).getName() ,  "user/key2/2") <<  "name wrong";
	ASSERT_EQ((++ks2.cbegin())->getName() ,  "user/key2/2"  ) <<  "name wrong";
	ASSERT_EQ((*(ks2.cbegin()+1)).getName() ,  "user/key2/2") <<  "name wrong";
	ASSERT_EQ((ks2.cbegin()+1)->getName() ,  "user/key2/2"  ) <<  "name wrong";
	ASSERT_EQ((ks2.cbegin()[1])->getName() ,  "user/key2/2"  ) << "name wrong";
	ASSERT_EQ((*(ks2.cbegin()+2)).getName() ,  "user/key2/3") <<  "name wrong";
	ASSERT_EQ((ks2.cbegin()+2)->getName() ,  "user/key2/3"  ) <<  "name wrong";
	ASSERT_EQ((ks2.cbegin()[2])->getName() ,  "user/key2/3"  ) << "name wrong";
	ASSERT_EQ((ks2.cbegin()+3) ,  ks2.cend()  ) <<  "end iterator broken";

	ASSERT_EQ((*ks2.crbegin()).getName() ,  "user/key2/3"    ) <<  "name wrong";
	ASSERT_EQ(ks2.crbegin()->getName() ,  "user/key2/3"      ) <<  "name wrong";
	ASSERT_EQ((ks2.crbegin()[0])->getName() ,  "user/key2/3"  ) << "name wrong";
	ASSERT_EQ((*(++ks2.crbegin())).getName() ,  "user/key2/2") <<  "name wrong";
	ASSERT_EQ((++ks2.crbegin())->getName() ,  "user/key2/2"  ) <<  "name wrong";
	ASSERT_EQ((*(ks2.crbegin()+1)).getName() ,  "user/key2/2") <<  "name wrong";
	ASSERT_EQ((ks2.crbegin()+1)->getName() ,  "user/key2/2"  ) <<  "name wrong";
	ASSERT_EQ((ks2.crbegin()[1])->getName() ,  "user/key2/2"  ) << "name wrong";
	ASSERT_EQ((*(ks2.crbegin()+2)).getName() ,  "user/key2/1") <<  "name wrong";
	ASSERT_EQ((ks2.crbegin()+2)->getName() ,  "user/key2/1"  ) <<  "name wrong";
	ASSERT_EQ((ks2.crbegin()[2])->getName() ,  "user/key2/1"  ) << "name wrong";
	ASSERT_EQ((ks2.crbegin()+3) ,  ks2.crend()  ) <<  "end iterator broken";
#endif
}

TEST(test_iter, const_iterate)
{
	std::cout << "testing const iterate" << std::endl;
	const KeySet ks2 (5,
		*Key ("user/key2/1", KEY_END),
		*Key ("user/key2/2", KEY_END),
		*Key ("user/key2/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	ASSERT_EQ((*ks2.begin()).getName() ,  "user/key2/1"    ) <<  "name wrong";
	ASSERT_EQ(ks2.begin()->getName() ,  "user/key2/1"      ) <<  "name wrong";
	ASSERT_EQ((ks2.begin()[0])->getName() ,  "user/key2/1"  ) << "name wrong";
	ASSERT_EQ((*(++ks2.begin())).getName() ,  "user/key2/2") <<  "name wrong";
	ASSERT_EQ((++ks2.begin())->getName() ,  "user/key2/2"  ) <<  "name wrong";
	ASSERT_EQ((*(ks2.begin()+1)).getName() ,  "user/key2/2") <<  "name wrong";
	ASSERT_EQ((ks2.begin()+1)->getName() ,  "user/key2/2"  ) <<  "name wrong";
	ASSERT_EQ((ks2.begin()[1])->getName() ,  "user/key2/2"  ) << "name wrong";
	ASSERT_EQ((*(ks2.begin()+2)).getName() ,  "user/key2/3") <<  "name wrong";
	ASSERT_EQ((ks2.begin()+2)->getName() ,  "user/key2/3"  ) <<  "name wrong";
	ASSERT_EQ((ks2.begin()[2])->getName() ,  "user/key2/3"  ) << "name wrong";
	ASSERT_EQ((ks2.begin()+3) ,  ks2.end()  ) <<  "end iterator broken";

	ASSERT_EQ((*ks2.rbegin()).getName() ,  "user/key2/3"    ) <<  "name wrong";
	ASSERT_EQ(ks2.rbegin()->getName() ,  "user/key2/3"      ) <<  "name wrong";
	ASSERT_EQ((ks2.rbegin()[0])->getName() ,  "user/key2/3"  ) << "name wrong";
	ASSERT_EQ((*(++ks2.rbegin())).getName() ,  "user/key2/2") <<  "name wrong";
	ASSERT_EQ((++ks2.rbegin())->getName() ,  "user/key2/2"  ) <<  "name wrong";
	ASSERT_EQ((*(ks2.rbegin()+1)).getName() ,  "user/key2/2") <<  "name wrong";
	ASSERT_EQ((ks2.rbegin()+1)->getName() ,  "user/key2/2"  ) <<  "name wrong";
	ASSERT_EQ((ks2.rbegin()[1])->getName() ,  "user/key2/2"  ) << "name wrong";
	ASSERT_EQ((*(ks2.rbegin()+2)).getName() ,  "user/key2/1") <<  "name wrong";
	ASSERT_EQ((ks2.rbegin()+2)->getName() ,  "user/key2/1"  ) <<  "name wrong";
	ASSERT_EQ((ks2.rbegin()[2])->getName() ,  "user/key2/1"  ) << "name wrong";
	ASSERT_EQ((ks2.rbegin()+3) ,  ks2.rend()  ) <<  "end iterator broken";

#if __cplusplus > 199711L
	ASSERT_EQ((*ks2.cbegin()).getName() ,  "user/key2/1"    ) <<  "name wrong";
	ASSERT_EQ(ks2.cbegin()->getName() ,  "user/key2/1"      ) <<  "name wrong";
	ASSERT_EQ((ks2.cbegin()[0])->getName() ,  "user/key2/1"  ) << "name wrong";
	ASSERT_EQ((*(++ks2.cbegin())).getName() ,  "user/key2/2") <<  "name wrong";
	ASSERT_EQ((++ks2.cbegin())->getName() ,  "user/key2/2"  ) <<  "name wrong";
	ASSERT_EQ((*(ks2.cbegin()+1)).getName() ,  "user/key2/2") <<  "name wrong";
	ASSERT_EQ((ks2.cbegin()+1)->getName() ,  "user/key2/2"  ) <<  "name wrong";
	ASSERT_EQ((ks2.cbegin()[1])->getName() ,  "user/key2/2"  ) << "name wrong";
	ASSERT_EQ((*(ks2.cbegin()+2)).getName() ,  "user/key2/3") <<  "name wrong";
	ASSERT_EQ((ks2.cbegin()+2)->getName() ,  "user/key2/3"  ) <<  "name wrong";
	ASSERT_EQ((ks2.cbegin()[2])->getName() ,  "user/key2/3"  ) << "name wrong";
	ASSERT_EQ((ks2.cbegin()+3) ,  ks2.cend()  ) <<  "end iterator broken";

	ASSERT_EQ((*ks2.crbegin()).getName() ,  "user/key2/3"    ) <<  "name wrong";
	ASSERT_EQ(ks2.crbegin()->getName() ,  "user/key2/3"      ) <<  "name wrong";
	ASSERT_EQ((ks2.crbegin()[0])->getName() ,  "user/key2/3"  ) << "name wrong";
	ASSERT_EQ((*(++ks2.crbegin())).getName() ,  "user/key2/2") <<  "name wrong";
	ASSERT_EQ((++ks2.crbegin())->getName() ,  "user/key2/2"  ) <<  "name wrong";
	ASSERT_EQ((*(ks2.crbegin()+1)).getName() ,  "user/key2/2") <<  "name wrong";
	ASSERT_EQ((ks2.crbegin()+1)->getName() ,  "user/key2/2"  ) <<  "name wrong";
	ASSERT_EQ((ks2.crbegin()[1])->getName() ,  "user/key2/2"  ) << "name wrong";
	ASSERT_EQ((*(ks2.crbegin()+2)).getName() ,  "user/key2/1") <<  "name wrong";
	ASSERT_EQ((ks2.crbegin()+2)->getName() ,  "user/key2/1"  ) <<  "name wrong";
	ASSERT_EQ((ks2.crbegin()[2])->getName() ,  "user/key2/1"  ) << "name wrong";
	ASSERT_EQ((ks2.crbegin()+3) ,  ks2.crend()  ) <<  "end iterator broken";
#endif
}

TEST(test_iter, iterator)
{
	std::cout << "test iterator" << std::endl;
	KeySet ks4 (5,
		*Key ("user/key4/1", KEY_END),
		*Key ("user/key4/2", KEY_END),
		*Key ("user/key4/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	KeySet::iterator it = ks4.begin();
	ASSERT_EQ(it->getName() ,  "user/key4/1"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/1"    ) <<  "name wrong";
	it+=1;
	ASSERT_EQ(it->getName() ,  "user/key4/2"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/2"    ) <<  "name wrong";
	it+=1;
	ASSERT_EQ(it->getName() ,  "user/key4/3"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/3"    ) <<  "name wrong";
	it+=1;
	ASSERT_EQ(it ,  ks4.end()    ) <<  "not at end";
	it-=1;
	ASSERT_EQ(it->getName() ,  "user/key4/3"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/3"    ) <<  "name wrong";
	it-=1;
	ASSERT_EQ(it->getName() ,  "user/key4/2"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/2"    ) <<  "name wrong";
	it-=1;
	ASSERT_EQ(it->getName() ,  "user/key4/1"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/1"    ) <<  "name wrong";
	ASSERT_EQ(it ,  ks4.begin()    ) <<  "not at begin";



	ASSERT_EQ(it->getName() ,  "user/key4/1"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/1"    ) <<  "name wrong";
	it++;
	ASSERT_EQ(it->getName() ,  "user/key4/2"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/2"    ) <<  "name wrong";
	it++;
	ASSERT_EQ(it->getName() ,  "user/key4/3"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/3"    ) <<  "name wrong";
	it++;
	ASSERT_EQ(it ,  ks4.end()    ) <<  "not at end";
	it--;
	ASSERT_EQ(it->getName() ,  "user/key4/3"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/3"    ) <<  "name wrong";
	it--;
	ASSERT_EQ(it->getName() ,  "user/key4/2"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/2"    ) <<  "name wrong";
	it--;
}

TEST(test_iter, const_iterator)
{
	std::cout << "test const iterator" << std::endl;
	const KeySet ks4 (5,
		*Key ("user/key4/1", KEY_END),
		*Key ("user/key4/2", KEY_END),
		*Key ("user/key4/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	KeySet::iterator it = ks4.begin();
	ASSERT_EQ(it->getName() ,  "user/key4/1"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/1"    ) <<  "name wrong";
	it+=1;
	ASSERT_EQ(it->getName() ,  "user/key4/2"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/2"    ) <<  "name wrong";
	it+=1;
	ASSERT_EQ(it->getName() ,  "user/key4/3"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/3"    ) <<  "name wrong";
	it+=1;
	ASSERT_EQ(it ,  ks4.end()    ) <<  "not at end";
	it-=1;
	ASSERT_EQ(it->getName() ,  "user/key4/3"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/3"    ) <<  "name wrong";
	it-=1;
	ASSERT_EQ(it->getName() ,  "user/key4/2"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/2"    ) <<  "name wrong";
	it-=1;
	ASSERT_EQ(it->getName() ,  "user/key4/1"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/1"    ) <<  "name wrong";
	ASSERT_EQ(it ,  ks4.begin()    ) <<  "not at begin";



	ASSERT_EQ(it->getName() ,  "user/key4/1"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/1"    ) <<  "name wrong";
	it++;
	ASSERT_EQ(it->getName() ,  "user/key4/2"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/2"    ) <<  "name wrong";
	it++;
	ASSERT_EQ(it->getName() ,  "user/key4/3"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/3"    ) <<  "name wrong";
	it++;
	ASSERT_EQ(it ,  ks4.end()    ) <<  "not at end";
	it--;
	ASSERT_EQ(it->getName() ,  "user/key4/3"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/3"    ) <<  "name wrong";
	it--;
	ASSERT_EQ(it->getName() ,  "user/key4/2"    ) <<  "name wrong";
	ASSERT_EQ((*it).getName() ,  "user/key4/2"    ) <<  "name wrong";
	it--;
}

// TODO: should be in example:
#if __cplusplus > 199711L
TEST(test_iter, for_loop)
{
	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	for (Key k:ks3)
	{
		std::cout << k.getName() << std::endl;
	}

	for (auto i = ks3.begin(); i != ks3.end(); ++i)
	{
		Key k(*i);
		std::cout << k.getName() << std::endl;
	}

	for (auto i = ks3.cbegin(); i != ks3.cend(); ++i)
	{
		Key k(*i);
		std::cout << k.getName() << std::endl;
	}

	for (auto i = ks3.rbegin(); i != ks3.rend(); ++i)
	{
		Key k(*i);
		std::cout << k.getName() << std::endl;
	}

	for (auto i = ks3.crbegin(); i != ks3.crend(); ++i)
	{
		Key k(*i);
		std::cout << k.getName() << std::endl;
	}
}
#endif
