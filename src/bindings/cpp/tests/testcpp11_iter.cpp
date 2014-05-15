#include <tests.hpp>

#include <vector>
#include <algorithm>

void test_iterate()
{
	std::cout << "testing iterate" << std::endl;
	KeySet ks2 (5,
		*Key ("user/key2/1", KEY_END),
		*Key ("user/key2/2", KEY_END),
		*Key ("user/key2/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	succeed_if((*ks2.begin()).getName() == "user/key2/1"    , "name wrong");
	succeed_if(ks2.begin()->getName() == "user/key2/1"      , "name wrong");
	succeed_if((ks2.begin()[0])->getName() == "user/key2/1"  ,"name wrong");
	succeed_if((*(++ks2.begin())).getName() == "user/key2/2", "name wrong");
	succeed_if((++ks2.begin())->getName() == "user/key2/2"  , "name wrong");
	succeed_if((*(ks2.begin()+1)).getName() == "user/key2/2", "name wrong");
	succeed_if((ks2.begin()+1)->getName() == "user/key2/2"  , "name wrong");
	succeed_if((ks2.begin()[1])->getName() == "user/key2/2"  ,"name wrong");
	succeed_if((*(ks2.begin()+2)).getName() == "user/key2/3", "name wrong");
	succeed_if((ks2.begin()+2)->getName() == "user/key2/3"  , "name wrong");
	succeed_if((ks2.begin()[2])->getName() == "user/key2/3"  ,"name wrong");
	succeed_if((ks2.begin()+3) == ks2.end()  , "end iterator broken");

	succeed_if((*ks2.rbegin()).getName() == "user/key2/3"    , "name wrong");
	succeed_if(ks2.rbegin()->getName() == "user/key2/3"      , "name wrong");
	succeed_if((ks2.rbegin()[0])->getName() == "user/key2/3"  ,"name wrong");
	succeed_if((*(++ks2.rbegin())).getName() == "user/key2/2", "name wrong");
	succeed_if((++ks2.rbegin())->getName() == "user/key2/2"  , "name wrong");
	succeed_if((*(ks2.rbegin()+1)).getName() == "user/key2/2", "name wrong");
	succeed_if((ks2.rbegin()+1)->getName() == "user/key2/2"  , "name wrong");
	succeed_if((ks2.rbegin()[1])->getName() == "user/key2/2"  ,"name wrong");
	succeed_if((*(ks2.rbegin()+2)).getName() == "user/key2/1", "name wrong");
	succeed_if((ks2.rbegin()+2)->getName() == "user/key2/1"  , "name wrong");
	succeed_if((ks2.rbegin()[2])->getName() == "user/key2/1"  ,"name wrong");
	succeed_if((ks2.rbegin()+3) == ks2.rend()  , "end iterator broken"); // TODO ???

#if __cplusplus > 199711L
	succeed_if((*ks2.cbegin()).getName() == "user/key2/1"    , "name wrong");
	succeed_if(ks2.cbegin()->getName() == "user/key2/1"      , "name wrong");
	succeed_if((ks2.cbegin()[0])->getName() == "user/key2/1"  ,"name wrong");
	succeed_if((*(++ks2.cbegin())).getName() == "user/key2/2", "name wrong");
	succeed_if((++ks2.cbegin())->getName() == "user/key2/2"  , "name wrong");
	succeed_if((*(ks2.cbegin()+1)).getName() == "user/key2/2", "name wrong");
	succeed_if((ks2.cbegin()+1)->getName() == "user/key2/2"  , "name wrong");
	succeed_if((ks2.cbegin()[1])->getName() == "user/key2/2"  ,"name wrong");
	succeed_if((*(ks2.cbegin()+2)).getName() == "user/key2/3", "name wrong");
	succeed_if((ks2.cbegin()+2)->getName() == "user/key2/3"  , "name wrong");
	succeed_if((ks2.cbegin()[2])->getName() == "user/key2/3"  ,"name wrong");
	succeed_if((ks2.cbegin()+3) == ks2.cend()  , "end iterator broken");

	succeed_if((*ks2.crbegin()).getName() == "user/key2/3"    , "name wrong");
	succeed_if(ks2.crbegin()->getName() == "user/key2/3"      , "name wrong");
	succeed_if((ks2.crbegin()[0])->getName() == "user/key2/3"  ,"name wrong");
	succeed_if((*(++ks2.crbegin())).getName() == "user/key2/2", "name wrong");
	succeed_if((++ks2.crbegin())->getName() == "user/key2/2"  , "name wrong");
	succeed_if((*(ks2.crbegin()+1)).getName() == "user/key2/2", "name wrong");
	succeed_if((ks2.crbegin()+1)->getName() == "user/key2/2"  , "name wrong");
	succeed_if((ks2.crbegin()[1])->getName() == "user/key2/2"  ,"name wrong");
	succeed_if((*(ks2.crbegin()+2)).getName() == "user/key2/1", "name wrong");
	succeed_if((ks2.crbegin()+2)->getName() == "user/key2/1"  , "name wrong");
	succeed_if((ks2.crbegin()[2])->getName() == "user/key2/1"  ,"name wrong");
	succeed_if((ks2.crbegin()+3) == ks2.crend()  , "end iterator broken");
#endif
}

void test_const_iterate()
{
	std::cout << "testing const iterate" << std::endl;
	const KeySet ks2 (5,
		*Key ("user/key2/1", KEY_END),
		*Key ("user/key2/2", KEY_END),
		*Key ("user/key2/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	succeed_if((*ks2.begin()).getName() == "user/key2/1"    , "name wrong");
	succeed_if(ks2.begin()->getName() == "user/key2/1"      , "name wrong");
	succeed_if((ks2.begin()[0])->getName() == "user/key2/1"  ,"name wrong");
	succeed_if((*(++ks2.begin())).getName() == "user/key2/2", "name wrong");
	succeed_if((++ks2.begin())->getName() == "user/key2/2"  , "name wrong");
	succeed_if((*(ks2.begin()+1)).getName() == "user/key2/2", "name wrong");
	succeed_if((ks2.begin()+1)->getName() == "user/key2/2"  , "name wrong");
	succeed_if((ks2.begin()[1])->getName() == "user/key2/2"  ,"name wrong");
	succeed_if((*(ks2.begin()+2)).getName() == "user/key2/3", "name wrong");
	succeed_if((ks2.begin()+2)->getName() == "user/key2/3"  , "name wrong");
	succeed_if((ks2.begin()[2])->getName() == "user/key2/3"  ,"name wrong");
	succeed_if((ks2.begin()+3) == ks2.end()  , "end iterator broken");

	succeed_if((*ks2.rbegin()).getName() == "user/key2/3"    , "name wrong");
	succeed_if(ks2.rbegin()->getName() == "user/key2/3"      , "name wrong");
	succeed_if((ks2.rbegin()[0])->getName() == "user/key2/3"  ,"name wrong");
	succeed_if((*(++ks2.rbegin())).getName() == "user/key2/2", "name wrong");
	succeed_if((++ks2.rbegin())->getName() == "user/key2/2"  , "name wrong");
	succeed_if((*(ks2.rbegin()+1)).getName() == "user/key2/2", "name wrong");
	succeed_if((ks2.rbegin()+1)->getName() == "user/key2/2"  , "name wrong");
	succeed_if((ks2.rbegin()[1])->getName() == "user/key2/2"  ,"name wrong");
	succeed_if((*(ks2.rbegin()+2)).getName() == "user/key2/1", "name wrong");
	succeed_if((ks2.rbegin()+2)->getName() == "user/key2/1"  , "name wrong");
	succeed_if((ks2.rbegin()[2])->getName() == "user/key2/1"  ,"name wrong");
	succeed_if((ks2.rbegin()+3) == ks2.rend()  , "end iterator broken"); // TODO ???

#if __cplusplus > 199711L
	succeed_if((*ks2.cbegin()).getName() == "user/key2/1"    , "name wrong");
	succeed_if(ks2.cbegin()->getName() == "user/key2/1"      , "name wrong");
	succeed_if((ks2.cbegin()[0])->getName() == "user/key2/1"  ,"name wrong");
	succeed_if((*(++ks2.cbegin())).getName() == "user/key2/2", "name wrong");
	succeed_if((++ks2.cbegin())->getName() == "user/key2/2"  , "name wrong");
	succeed_if((*(ks2.cbegin()+1)).getName() == "user/key2/2", "name wrong");
	succeed_if((ks2.cbegin()+1)->getName() == "user/key2/2"  , "name wrong");
	succeed_if((ks2.cbegin()[1])->getName() == "user/key2/2"  ,"name wrong");
	succeed_if((*(ks2.cbegin()+2)).getName() == "user/key2/3", "name wrong");
	succeed_if((ks2.cbegin()+2)->getName() == "user/key2/3"  , "name wrong");
	succeed_if((ks2.cbegin()[2])->getName() == "user/key2/3"  ,"name wrong");
	succeed_if((ks2.cbegin()+3) == ks2.cend()  , "end iterator broken");

	succeed_if((*ks2.crbegin()).getName() == "user/key2/3"    , "name wrong");
	succeed_if(ks2.crbegin()->getName() == "user/key2/3"      , "name wrong");
	succeed_if((ks2.crbegin()[0])->getName() == "user/key2/3"  ,"name wrong");
	succeed_if((*(++ks2.crbegin())).getName() == "user/key2/2", "name wrong");
	succeed_if((++ks2.crbegin())->getName() == "user/key2/2"  , "name wrong");
	succeed_if((*(ks2.crbegin()+1)).getName() == "user/key2/2", "name wrong");
	succeed_if((ks2.crbegin()+1)->getName() == "user/key2/2"  , "name wrong");
	succeed_if((ks2.crbegin()[1])->getName() == "user/key2/2"  ,"name wrong");
	succeed_if((*(ks2.crbegin()+2)).getName() == "user/key2/1", "name wrong");
	succeed_if((ks2.crbegin()+2)->getName() == "user/key2/1"  , "name wrong");
	succeed_if((ks2.crbegin()[2])->getName() == "user/key2/1"  ,"name wrong");
	succeed_if((ks2.crbegin()+3) == ks2.crend()  , "end iterator broken");
#endif
}

void test_iterator()
{
	std::cout << "test iterator" << std::endl;
	KeySet ks4 (5,
		*Key ("user/key4/1", KEY_END),
		*Key ("user/key4/2", KEY_END),
		*Key ("user/key4/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	KeySet::iterator it = ks4.begin();
	succeed_if(it->getName() == "user/key4/1"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/1"    , "name wrong");
	it+=1;
	succeed_if(it->getName() == "user/key4/2"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/2"    , "name wrong");
	it+=1;
	succeed_if(it->getName() == "user/key4/3"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/3"    , "name wrong");
	it+=1;
	succeed_if(it == ks4.end()    , "not at end");
	it-=1;
	succeed_if(it->getName() == "user/key4/3"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/3"    , "name wrong");
	it-=1;
	succeed_if(it->getName() == "user/key4/2"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/2"    , "name wrong");
	it-=1;
	succeed_if(it->getName() == "user/key4/1"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/1"    , "name wrong");
	succeed_if(it == ks4.begin()    , "not at begin");



	succeed_if(it->getName() == "user/key4/1"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/1"    , "name wrong");
	it++;
	succeed_if(it->getName() == "user/key4/2"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/2"    , "name wrong");
	it++;
	succeed_if(it->getName() == "user/key4/3"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/3"    , "name wrong");
	it++;
	succeed_if(it == ks4.end()    , "not at end");
	it--;
	succeed_if(it->getName() == "user/key4/3"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/3"    , "name wrong");
	it--;
	succeed_if(it->getName() == "user/key4/2"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/2"    , "name wrong");
	it--;
}

void test_const_iterator()
{
	std::cout << "test const iterator" << std::endl;
	const KeySet ks4 (5,
		*Key ("user/key4/1", KEY_END),
		*Key ("user/key4/2", KEY_END),
		*Key ("user/key4/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	KeySet::iterator it = ks4.begin();
	succeed_if(it->getName() == "user/key4/1"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/1"    , "name wrong");
	it+=1;
	succeed_if(it->getName() == "user/key4/2"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/2"    , "name wrong");
	it+=1;
	succeed_if(it->getName() == "user/key4/3"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/3"    , "name wrong");
	it+=1;
	succeed_if(it == ks4.end()    , "not at end");
	it-=1;
	succeed_if(it->getName() == "user/key4/3"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/3"    , "name wrong");
	it-=1;
	succeed_if(it->getName() == "user/key4/2"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/2"    , "name wrong");
	it-=1;
	succeed_if(it->getName() == "user/key4/1"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/1"    , "name wrong");
	succeed_if(it == ks4.begin()    , "not at begin");



	succeed_if(it->getName() == "user/key4/1"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/1"    , "name wrong");
	it++;
	succeed_if(it->getName() == "user/key4/2"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/2"    , "name wrong");
	it++;
	succeed_if(it->getName() == "user/key4/3"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/3"    , "name wrong");
	it++;
	succeed_if(it == ks4.end()    , "not at end");
	it--;
	succeed_if(it->getName() == "user/key4/3"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/3"    , "name wrong");
	it--;
	succeed_if(it->getName() == "user/key4/2"    , "name wrong");
	succeed_if((*it).getName() == "user/key4/2"    , "name wrong");
	it--;
}

// TODO: should be in example:
#if __cplusplus > 199711L
void test_for_loop()
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


int main()
{
	cout << "KEYSET CLASS TESTS" << endl;
	cout << "==================" << endl << endl;

	test_iterate();
	test_const_iterate();
	test_iterator();
	test_const_iterator();
	// test_for_loop();

	cout << endl;
	cout << "test_key RESULTS: " << nbTest << " test(s) done. " << nbError << " error(s)." << endl;
	return nbError;
}
