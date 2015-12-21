/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <tests.hpp>

#include <vector>
#include <algorithm>

KeySet fun(size_t alloc, ...)
{
	va_list vl;

	va_start (vl, alloc);
	KeySet ks (va, alloc, vl);
	va_end (vl);
	return ks;
}

TEST(ks, new)
{
	KeySet ks1;

	KeySet ks2 (5,
		ckdb::keyNew ("user/key2", KEY_END),
		KS_END);

	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);
	// ks3.toStream(stdout, 0);

	Key k1("user/key4/1", KEY_END);
	Key k2("user/key4/2", KEY_END);
	Key k3("user/key4/3", KEY_VALUE, "value", KEY_END);
	KeySet ks4 (5,
		*k1, // k1 will lose its key and pass it to keyset
		*k2,
		*k3,
		KS_END);
	// ks4.toStream(stdout, 0);

	Key k4("user/key5/1", KEY_END);
	Key k5("user/key5/2", KEY_END);
	Key k6("user/key5/3", KEY_VALUE, "value", KEY_END);
	KeySet ks5 (5,
		k4.dup(),
		k5.dup(),
		k6.dup(),
		KS_END);
	// ks5.toStream(stdout, 0);
	// k4, k5, k6 can still be used

	KeySet ks6 = fun(5,
		k4.dup(),
		k5.dup(),
		k6.dup(),
		KS_END);
}


TEST(ks, dup)
{
	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);
	succeed_if (ks3.lookup("user/key3/1"), "could not find key");
	succeed_if (ks3.lookup("user/key3/2"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3").getString() == "value", "value not correct");
	succeed_if (ks3.size() == 3, "size not correct");

	KeySet ks4 (ks3.dup());
	succeed_if (ks4.lookup("user/key3/1"), "could not find key");
	succeed_if (ks4.lookup("user/key3/2"), "could not find key");
	succeed_if (ks4.lookup("user/key3/3"), "could not find key");
	succeed_if (ks4.lookup("user/key3/3").getString() == "value", "value not correct");
	succeed_if (ks4.size() == 3, "size not correct");

	// ks3.toStream(stdout, 0);
	// ks4.toStream(stdout, 0);
}

TEST(ks, copy)
{
	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	KeySet ks4 (ks3);
	succeed_if (ks3.size() == 3, "size not correct");
	succeed_if (ks4.size() == 3, "size not correct");

	KeySet ks5;
	ks5.copy(ks4);
	succeed_if (ks4.size() == 3, "size not correct");
	succeed_if (ks5.size() == 3, "size not correct");

	ks5.clear();
	succeed_if (ks5.size() == 0, "size not correct");



	// ks3.toStream(stdout, 0);
	// ks4.toStream(stdout, 0);
}

TEST(ks, iterate)
{
	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	ks3.rewind();

	Key k1 = ks3.next();
	succeed_if (k1.getName() == "user/key3/1", "wrong keyname");
	succeed_if (k1 == ks3.head(), "first key not head key");
	Key k2 = ks3.next();
	succeed_if (k2.getName() == "user/key3/2", "wrong keyname");
	Key k3 = ks3.next();
	succeed_if (k3.getName() == "user/key3/3", "wrong keyname");
	succeed_if (k3.getString() == "value", "wrong value");
	succeed_if (k3 == ks3.tail(), "last key not tail key");
	succeed_if (!ks3.next(), "no more key");
	succeed_if (!ks3.next(), "no more key");
	succeed_if (!ks3.next(), "no more key");
	succeed_if (!ks3.next(), "no more key");

	Key null = static_cast<ckdb::Key*>(nullptr);
	succeed_if (!null, "null key");

	ks3.rewind();
	for (ssize_t i=0; i<ks3.size(); i++)
	{
		Key k = ks3.next();
		char str[] = "user/key3/X";

		str [10] = i+'1';
		succeed_if (k.getName() == str, "wrong keyname");
	}

	ks3.rewind();
	Key n;
	int j=0;
	while ((n=ks3.next()))
	{
		char str[] = "user/key3/X";

		str [10] = j+'1';
		succeed_if (n.getName() == str, "wrong keyname");
		j++;
	}

	j=0;
	ks3.rewind();
	while ((n=ks3.next()) == true)
	{
		char str[] = "user/key3/X";

		str [10] = j+'1';
		succeed_if (n.getName() == str, "wrong keyname");
		j++;
	}

	j=0;
	ks3.rewind();
	for (Key k; (k=ks3.next());)
	{
		char str[] = "user/key3/X";

		str [10] = j+'1';
		succeed_if (k.getName() == str, "wrong keyname");
		j++;
	}

	j=0;
	ks3.rewind();
	for (Key k=ks3.next(); k; (k=ks3.next()))
	{
		char str[] = "user/key3/X";

		str [10] = j+'1';
		succeed_if (k.getName() == str, "wrong keyname");
		j++;
	}
}

TEST(ks, cursor)
{
	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);
	cursor_t cursorTest = ks3.getCursor();

	ks3.rewind();
	for (ssize_t i=0; i<ks3.size(); i++)
	{
		Key k = ks3.next();
		if (i==0) cursorTest = ks3.getCursor();
	}

	ks3.setCursor (cursorTest);
	Key k1 = ks3.current();
	succeed_if (k1.getName() == "user/key3/1", "wrong keyname");
	succeed_if (k1 == ks3.head(), "first key not head key");
}

TEST(ks, pop)
{
	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	ks3.rewind();

	Key k3 = ks3.pop();
	succeed_if (k3.getName() == "user/key3/3", "wrong keyname");
	succeed_if (k3.getString() == "value", "wrong value");
	Key k2 = ks3.pop();
	succeed_if (k2.getName() == "user/key3/2", "wrong keyname");
	Key k1 = ks3.pop();
	succeed_if (k1.getName() == "user/key3/1", "wrong keyname");
	Key k0 = ks3.pop();
	succeed_if (!k0, "Out of Range, no more key");

	KeySet ks4 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	ks4.rewind();
	for (int i=ks4.size()-1; i>0; i--)
	{
		Key k = ks4.pop();
		char str[] = "user/key3/X";

		str [10] = i+'1';
		succeed_if (k.getName() == str, str);
	}
}

TEST(ks, lookup)
{
	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	Key k1 = ks3.lookup("user/key3/1");
	succeed_if (k1, "did not find key");
	succeed_if (k1.getName() == "user/key3/1", "wrong keyname");

	Key k2 = ks3.lookup("user/key3/2");
	succeed_if (k2, "did not find key");
	succeed_if (k2.getName() == "user/key3/2", "wrong keyname");

	Key k3 = ks3.lookup("user/key3/3");
	succeed_if (k3, "did not find key");
	succeed_if (k3.getName() == "user/key3/3", "wrong keyname");
	succeed_if (k3.getString() == "value", "wrong value");

	Key k4 = ks3.lookup("user/key3/4");
	succeed_if (!k4, "Key does not exist");
}

TEST(ks, append)
{
	cout << "testing keyset append" << endl;

	KeySet ks1;

	KeySet ks2 (5,
		ckdb::keyNew ("user/key2", KEY_END),
		KS_END);
	ks1.append (ks2);

	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);
	ks2.append (ks3);
	ks1.append (ks3);
	ks3.append (ks2);

	Key k1("user/key4/1", KEY_END);
	Key k2("user/key4/2", KEY_END);
	Key k3("user/key4/3", KEY_VALUE, "value", KEY_END);
	ks1.append (k1); ks1.append (k2); ks1.append (k3);
	ks2.append (k1); ks2.append (k2); ks2.append (k3);
	ks3.append (k1); ks3.append (k2); ks3.append (k3);

	KeySet ks4 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	KeySet ks5;
	std::vector<Key> v(3);
	ks5.append(v[1]=Key("user/s/2", KEY_END));
	ks5.append(v[0]=Key("user/s/1", KEY_END));
	ks5.append(v[2]=Key("user/s/3", KEY_END));

	ks5.rewind();
	for (ssize_t i=0; i<ks5.size(); ++i)
	{
		succeed_if (*ks5.next() == *v[i], "wrong order");
	}

	// ks1.toStream();
	// ks2.toStream();
	// ks3.toStream();
}

TEST(ks, permutations)
{
	cout << "testing keyset append with all permutations" << endl;

	vector <Key> solution;
	solution.push_back(Key("user/s/1", KEY_END));
	solution.push_back(Key("user/s/2", KEY_END));
	solution.push_back(Key("user/s/3", KEY_END));
	solution.push_back(Key("user/s/3/s", KEY_END));
	solution.push_back(Key("user/s/3-3", KEY_END));

	vector <Key> permutation(solution);

	do {
		KeySet ks;
		ks.append(permutation[0]);
		ks.append(permutation[1]);
		ks.append(permutation[2]);
		ks.append(permutation[3]);
		ks.append(permutation[4]);
		ks.rewind();
		for (ssize_t i=0; i<ks.size(); ++i)
		{
			succeed_if (*ks.next() == *solution[i], "wrong order ");
		}
	} while (next_permutation(permutation.begin(), permutation.end()));

	solution.push_back(Key("user/s/x", KEY_END));
	permutation.push_back(solution[4]); // need a copy of same key, otherwise name is not the same string
	sort(permutation.begin(), permutation.end());

	do {
		KeySet ks;
		ks.append(permutation[0]);
		ks.append(permutation[1]);
		ks.append(permutation[2]);
		ks.append(permutation[3]);
		ks.append(permutation[4]);
		ks.append(permutation[5]);
		ks.rewind();
		for (ssize_t i=0; i<ks.size(); ++i)
		{
			succeed_if (*ks.next() == *solution[i], "wrong order");
		}
	} while (next_permutation(permutation.begin(), permutation.end()));

	solution.push_back(Key("user/x/y", KEY_END));
	permutation.push_back(solution[5]);
	sort(permutation.begin(), permutation.end());

	do {
		KeySet ks;
		ks.append(permutation[0]);
		ks.append(permutation[1]);
		ks.append(permutation[2]);
		ks.append(permutation[3]);
		ks.append(permutation[4]);
		ks.append(permutation[5]);
		ks.append(permutation[6]);
		ks.rewind();
		for (ssize_t i=0; i<ks.size(); ++i)
		{
			// note: raw pointers check the identity! It needs to be the same reference
			succeed_if (*ks.next() == *solution[i], "wrong order");
		}
	} while (next_permutation(permutation.begin(), permutation.end()));

	solution.push_back(Key("user/x/y/z", KEY_END));
	permutation.push_back(solution[5]);
	sort(permutation.begin(), permutation.end());

	do {
		KeySet ks;
		ks.append(permutation[0]);
		ks.append(permutation[1]);
		ks.append(permutation[2]);
		ks.append(permutation[3]);
		ks.append(permutation[4]);
		ks.append(permutation[5]);
		ks.append(permutation[6]);
		ks.append(permutation[7]);
		ks.rewind();
		for (ssize_t i=0; i<ks.size(); ++i)
		{
			succeed_if (*ks.next() == *solution[i], "wrong order");
		}
	} while (next_permutation(permutation.begin(), permutation.end()));
}

TEST(ks, appendOwner)
{
	KeySet ks;
	std::vector<Key> v(3);
	ks.append(v[1]=Key("user/s/1", KEY_OWNER, "markus", KEY_END));
	ks.append(v[0]=Key("user/s/1", KEY_END));
	ks.append(v[2]=Key("user/s/1", KEY_OWNER, "max", KEY_END));

	ks.rewind();
	for (ssize_t i=0; i<ks.size(); ++i)
	{
		succeed_if (*ks.next() == *v[i], "wrong order");
	}
}

TEST(ks, permutateOwner)
{
	vector <Key> solution;
	solution.push_back(Key("user/s", KEY_END));
	solution.push_back(Key("user/s", KEY_OWNER, "albert", KEY_END));
	solution.push_back(Key("user/s", KEY_OWNER, "barbara", KEY_END));

	vector <Key> permutation(solution);

	do {
		KeySet ks;
		ks.append(permutation[0]);
		ks.append(permutation[1]);
		ks.append(permutation[2]);
		ks.rewind();
		for (ssize_t i=0; i<ks.size(); ++i)
		{
			succeed_if(*ks.next() == *solution[i], "wrong order");
		}
	} while (next_permutation(permutation.begin(), permutation.end()));

	solution.push_back(Key("user/s", KEY_OWNER, "markus", KEY_END));
	permutation.push_back(solution[3]); // need a copy of same key, otherwise name is not the same string
	sort(permutation.begin(), permutation.end());

	do {
		KeySet ks;
		ks.append(permutation[0]);
		ks.append(permutation[1]);
		ks.append(permutation[2]);
		ks.append(permutation[3]);
		ks.rewind();
		for (ssize_t i=0; i<ks.size(); ++i)
		{
			succeed_if (*ks.next() == *solution[i], "wrong order");
		}
	} while (next_permutation(permutation.begin(), permutation.end()));

	solution.push_back(Key("user/s", KEY_OWNER, "max", KEY_END));
	permutation.push_back(solution[4]);
	sort(permutation.begin(), permutation.end());

	do {
		KeySet ks;
		ks.append(permutation[0]);
		ks.append(permutation[1]);
		ks.append(permutation[2]);
		ks.append(permutation[3]);
		ks.append(permutation[4]);
		ks.rewind();
		for (ssize_t i=0; i<ks.size(); ++i)
		{
			succeed_if (*ks.next() == *solution[i], "wrong order");
		}
	} while (next_permutation(permutation.begin(), permutation.end()));

	solution.push_back(Key("user/s", KEY_OWNER, "patrick", KEY_END));
	permutation.push_back(solution[5]);
	sort(permutation.begin(), permutation.end());

	do {
		KeySet ks;
		ks.append(permutation[0]);
		ks.append(permutation[1]);
		ks.append(permutation[2]);
		ks.append(permutation[3]);
		ks.append(permutation[4]);
		ks.append(permutation[5]);
		ks.rewind();
		for (ssize_t i=0; i<ks.size(); ++i)
		{
			succeed_if (*ks.next() == *solution[i], "wrong order");
		}
	} while (next_permutation(permutation.begin(), permutation.end()));

}

TEST(ks, comparision)
{
	Key ke1, ke2;

	succeed_if (ke1 == ke2, "two empty keys are not the same?");
	succeed_if (!(ke1 != ke2), "two empty keys are not the same?");

	Key k1("user/a", KEY_END), k2("user/b", KEY_END);

	succeed_if (ke1 < k1, "compare empty key with user/a");
	succeed_if (ke1 <= k1, "compare empty key with user/a");
	succeed_if (!(ke1 > k1), "compare empty key with user/a");
	succeed_if (!(ke1 >= k1), "compare empty key with user/a");

	succeed_if (ke1 < k2, "compare empty key with user/b");
	succeed_if (ke1 <= k2, "compare empty key with user/b");
	succeed_if (!(ke1 > k2), "compare empty key with user/b");
	succeed_if (!(ke1 >= k2), "compare empty key with user/b");

	succeed_if (k1 < k2, "compare key user/a with user/b");
	succeed_if (k1 <= k2, "compare key user/a with user/b");
	succeed_if (!(k1 > k2), "compare key user/a with user/b");
	succeed_if (!(k1 >= k2), "compare key user/a with user/b");
	succeed_if (k1 != k2, "compare key user/a with user/b");
	succeed_if (!(k1 == k2), "compare key user/a with user/b");

	Key ko1("user/a", KEY_OWNER, "markus", KEY_END), ko2("user/b", KEY_OWNER, "max", KEY_END);

	succeed_if (ko1 > k1, "compare key with user/a");
	succeed_if (ko1 >= k1, "compare key with user/a");
	succeed_if (!(ko1 < k1), "compare key with user/a");
	succeed_if (!(ko1 <= k1), "compare key with user/a");

	succeed_if (ko2 > k2, "compare key with user/b");
	succeed_if (ko2 >= k2, "compare key with user/b");
	succeed_if (!(ko2 < k2), "compare key with user/b");
	succeed_if (!(ko2 <= k2), "compare key with user/b");

	Key ko ("user/a", KEY_OWNER, "max", KEY_END);

	succeed_if (ko1 < ko, "compare key with user/b");
	succeed_if (ko1 <= ko, "compare key with user/b");
	succeed_if (!(ko1 > ko), "compare key with user/b");
	succeed_if (!(ko1 >= ko), "compare key with user/b");

	succeed_if (ko1 < ko2, "compare key user/a with     user/a owner max");
	succeed_if (ko1 <= ko2, "compare key user/a with    user/a owner max");
	succeed_if (!(ko1 > ko2), "compare key user/a with  user/a owner max");
	succeed_if (!(ko1 >= ko2), "compare key user/a with user/a owner max");
	succeed_if (ko1 != ko2, "compare key user/a with    user/a owner max");
	succeed_if (!(ko1 == ko2), "compare key user/a with user/a owner max");
}

void call (KeySet ks3)
{
	succeed_if (ks3.lookup("user/key3/1"), "could not find key");
	succeed_if (ks3.lookup("user/key3/2"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3").getString() == "value", "value not correct");
	succeed_if (ks3.size() == 3, "size not correct");
}

void refcall (KeySet &ks3)
{
	succeed_if (ks3.lookup("user/key3/1"), "could not find key");
	succeed_if (ks3.lookup("user/key3/2"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3").getString() == "value", "value not correct");
	succeed_if (ks3.size() == 3, "size not correct");
}

TEST(ks, call)
{
	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);
	succeed_if (ks3.lookup("user/key3/1"), "could not find key");
	succeed_if (ks3.lookup("user/key3/2"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3").getString() == "value", "value not correct");
	succeed_if (ks3.size() == 3, "size not correct");

	call(ks3);
	succeed_if (ks3.lookup("user/key3/1"), "could not find key");
	succeed_if (ks3.lookup("user/key3/2"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3").getString() == "value", "value not correct");
	succeed_if (ks3.size() == 3, "size not correct");

	refcall(ks3);
	succeed_if (ks3.lookup("user/key3/1"), "could not find key");
	succeed_if (ks3.lookup("user/key3/2"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3").getString() == "value", "value not correct");
	succeed_if (ks3.size() == 3, "size not correct");
}


void ccall (KeySet ks3)
{
	succeed_if (ks3.lookup("user/key3/1"), "could not find key");
	succeed_if (ks3.lookup("user/key3/2", KDB_O_POP), "could not find key");
	succeed_if (ks3.lookup("user/key3/3"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3").getString() == "value", "value not correct");
	succeed_if (ks3.size() == 2, "size not correct");

	ks3.lookup("user/key3/1").setString("will change");
	ks3.append(Key("user/key3/ccall", KEY_END));
}

void refccall (KeySet &ks3)
{
	succeed_if (ks3.lookup("user/key3/1"), "could not find key");
	succeed_if (ks3.lookup("user/key3/2", KDB_O_POP), "could not find key");
	succeed_if (ks3.lookup("user/key3/3"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3").getString() == "value", "value not correct");
	succeed_if (ks3.size() == 2, "size not correct");

	ks3.append(Key("user/key3/refccall", KEY_END));
	ks3.lookup("user/key3/1").setString("will change again");
}

TEST(ks, call2)
{
	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);
	succeed_if (ks3.lookup("user/key3/1"), "could not find key");
	succeed_if (ks3.lookup("user/key3/2"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3").getString() == "value", "value not correct");
	succeed_if (ks3.size() == 3, "size not correct");

	ccall(ks3);
	succeed_if (ks3.lookup("user/key3/1"), "could not find key");
	succeed_if (ks3.lookup("user/key3/1").getString() == "will change", "value did not change");
	succeed_if (ks3.lookup("user/key3/2"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3").getString() == "value", "value not correct");
	succeed_if (ks3.size() == 3, "size not correct");
	succeed_if (!ks3.lookup("user/key3/ccall"), "key should not be there");

	refccall(ks3);
	succeed_if (ks3.lookup("user/key3/1"), "could not find key");
	succeed_if (ks3.lookup("user/key3/1").getString() == "will change again", "value did not change");
	succeed_if (!ks3.lookup("user/key3/2"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3").getString() == "value", "value not correct");
	succeed_if (ks3.lookup("user/key3/refccall"), "could not find key");
	succeed_if (ks3.size() == 3, "size not correct");
}


void rcopycall(KeySet ks)
{
	// do something with keyset
	// (wont be one hierarchy higher)
	ks.append(Key("user/yyy", KEY_END));
}

void rrefcall(KeySet & ks)
{
	// do something with keyset
	ks.append(Key("user/xxx", KEY_END));
}

/*Calling conventions: user need to free the keyset */
void rcall(KeySet ks)
{
	// do something with ks

	rrefcall (ks);
	succeed_if (ks.lookup("user/xxx"), "could not find key");


	rcopycall(ks);
	succeed_if (ks.lookup("user/xxx"), "could not find key");
	succeed_if (!ks.lookup("user/yyy"), "could not find key");

	// don't destroy ks
	ks.release();
}

TEST(ks, release)
{
	KeySet ks1;

	ckdb::KeySet *ks = ks1.release();
	ckdb::ksDel (ks);

	KeySet ks2 (5,
		ckdb::keyNew ("user/key2", KEY_END),
		KS_END);

	ks = ks2.release();
	ckdb::ksDel (ks);

	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	ks = ks3.release();
	ckdb::ksDel (ks);

	ks = ckdb::ksNew (5, ckdb::keyNew("user/abc", KEY_END), KS_END);
	rcall (ks);
	succeed_if (ckdb::ksLookupByName(ks, "user/xxx", 0) != nullptr, "could not find key");
	ckdb::ksDel (ks);
}

TEST(ks, lookupPop)
{
	cout << "testing lookup pop" << endl;

	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	succeed_if (ks3.size() == 3, "size not correct");

	Key k3 = ks3.lookup("user/key3/3", KDB_O_POP);
	succeed_if (k3.getName() == "user/key3/3", "wrong keyname");
	succeed_if (k3.getString() == "value", "wrong value");
	succeed_if (ks3.size() == 2, "size not correct");

	Key k1 = ks3.lookup("user/key3/1", KDB_O_POP);
	succeed_if (k1.getName() == "user/key3/1", "wrong keyname");
	succeed_if (ks3.size() == 1, "size not correct");

	Key k2 = ks3.lookup("user/key3/2", KDB_O_POP);
	succeed_if (k2.getName() == "user/key3/2", "wrong keyname");
	succeed_if (ks3.size() == 0, "size not correct");

	Key k0 = ks3.lookup("user/key3/2", KDB_O_POP);
	succeed_if (!k0, "Out of Range, no more key");
	succeed_if (ks3.size() == 0, "size not correct");

	Key kn = ks3.lookup("user/key3/n", KDB_O_POP);
	succeed_if (!kn, "key was never in set");
	succeed_if (ks3.size() == 0, "size not correct");

	KeySet ks4 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	for (ssize_t i=ks4.size(); i>0; i--)
	{
		char str[] = "user/key3/X";

		str [10] = i+'0';
		succeed_if (ks4.size() == i, "size not correct");
		Key k = ks4.lookup(str, KDB_O_POP);
		succeed_if (k, "there should be a key");

		succeed_if (k.getName() == str, str);
		succeed_if (ks4.size() == i-1, "size not correct");
	}

	KeySet ks5 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	for (ssize_t i=ks5.size(); i>0; i--)
	{
		char str[] = "user/key3/X";

		str [10] = i+'0';
		Key searchKey (str, KEY_END);
		succeed_if (ks5.size() == i, "size not correct");

		Key k = ks5.lookup(searchKey, KDB_O_POP);
		succeed_if (k, "there should be a key");

		succeed_if (k.getName() == str, str);
		succeed_if (ks5.size() == i-1, "size not correct");
	}
}

TEST(ks, duplicate)
{
	cout << "testing ksdup" << endl;

	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);
	succeed_if (ks3.lookup("user/key3/1"), "could not find key");
	succeed_if (ks3.lookup("user/key3/2"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3"), "could not find key");
	succeed_if (ks3.lookup("user/key3/3").getString() == "value", "value not correct");
	succeed_if (ks3.size() == 3, "size not correct");

	KeySet ks4;
	ks4 = ks3;
	succeed_if (ks4.lookup("user/key3/1"), "could not find key");
	succeed_if (ks4.lookup("user/key3/2"), "could not find key");
	succeed_if (ks4.lookup("user/key3/3"), "could not find key");
	succeed_if (ks4.lookup("user/key3/3").getString() == "value", "value not correct");
	succeed_if (ks4.size() == 3, "size not correct");

	// ks3.toStream(stdout, 0);
	// ks4.toStream(stdout, 0);
}

KeySet fill_vaargs(size_t size, ...)
{
	va_list ap;
	va_start(ap, size);
	KeySet ks(va, size, ap);
	va_end(ap);
	return ks;
}

TEST(ks, vaargs)
{
	cout << "testing vaargs" << endl;

	KeySet ks = fill_vaargs(20,
			*Key("user/a", KEY_END),
			*Key("user/b", KEY_END),
			KS_END);
	succeed_if (ks.lookup("user/a"), "could not find key");
	succeed_if (ks.lookup("user/b"), "could not find key");
}

TEST(ks, get)
{
	KeySet ks (5,
		*Key ("user/map", KEY_END),
		*Key ("user/map/a", KEY_END),
		*Key ("user/map/b", KEY_END),
		*Key ("user/map/c", KEY_VALUE, "value", KEY_END),
		KS_END);
	EXPECT_EQ (ks.get<std::string>("user/map/c"), "value");
	EXPECT_THROW (ks.get<std::string>("user/map/x"), KeyTypeConversion);
	typedef std::map<std::string, std::string> map;
	map m = ks.get<map>("user/map");
	EXPECT_EQ (m.size(), 3);
	EXPECT_EQ (m["a"], "");
	EXPECT_EQ (m["b"], "");
	EXPECT_EQ (m["c"], "value");
	EXPECT_EQ (m.size(), 3) << "we inserted a value during test";
}

TEST(ks, getCascading)
{
	KeySet ks (5,
		*Key ("user/map", KEY_END),
		*Key ("user/map/a", KEY_END),
		*Key ("user/map/b", KEY_END),
		*Key ("user/map/c", KEY_VALUE, "winvalue", KEY_END),
		*Key ("user/map/d", KEY_VALUE, "dvalue", KEY_END),
		*Key ("system/map", KEY_END),
		*Key ("system/map/a", KEY_END),
		*Key ("system/map/b", KEY_END),
		*Key ("system/map/c", KEY_VALUE, "value", KEY_END),
		*Key ("system/map/e", KEY_VALUE, "evalue", KEY_END),
		KS_END);
	EXPECT_EQ (ks.get<std::string>("/map/c"), "winvalue");
	typedef std::map<std::string, std::string> map;
	map m = ks.get<map>("/map");
	EXPECT_EQ (m.size(), 5);
	EXPECT_EQ (m["a"], "");
	EXPECT_EQ (m["b"], "");
	EXPECT_EQ (m["c"], "winvalue");
	EXPECT_EQ (m["d"], "dvalue");
	EXPECT_EQ (m["e"], "evalue");
	EXPECT_EQ (m.size(), 5);
}
