/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests.hpp>

#include <memory>

#include <algorithm>
#include <vector>

KeySet fun (size_t alloc, ...)
{
	va_list vl;

	va_start (vl, alloc);
	KeySet ks (VaAlloc (alloc), vl);
	va_end (vl);
	return ks;
}

TEST (ks, new)
{
	// would fail to compile with: error: call to ‘kdb::KeySet::KeySet’ declared with attribute error: wrong usage of API
	//    or error: call to deleted constructor of 'kdb::KeySet'
	// KeySet(Key("user:/", KEY_END), KS_END);

	KeySet ks1;

	KeySet ks2 (5, ckdb::keyNew ("user:/key2", KEY_END), KS_END);

	KeySet ks3 (5, *Key ("user:/key3/1", KEY_END), *Key ("user:/key3/2", KEY_END), *Key ("user:/key3/3", KEY_VALUE, "value", KEY_END),
		    KS_END);
	// ks3.toStream(stdout, 0);

	Key k1 ("user:/key4/1", KEY_END);
	Key k2 ("user:/key4/2", KEY_END);
	Key k3 ("user:/key4/3", KEY_VALUE, "value", KEY_END);
	KeySet ks4 (5,
		    *k1, // k1 will lose its key and pass it to keyset
		    *k2, *k3, KS_END);
	// ks4.toStream(stdout, 0);

	Key k4 ("user:/key5/1", KEY_END);
	Key k5 ("user:/key5/2", KEY_END);
	Key k6 ("user:/key5/3", KEY_VALUE, "value", KEY_END);
	KeySet ks5 (5, k4.dup (), k5.dup (), k6.dup (), KS_END);
	// ks5.toStream(stdout, 0);
	// k4, k5, k6 can still be used

	KeySet ks6 = fun (5, k4.dup (), k5.dup (), k6.dup (), KS_END);
}


TEST (ks, dup)
{
	KeySet ks3 (5, *Key ("user:/key3/1", KEY_END), *Key ("user:/key3/2", KEY_END), *Key ("user:/key3/3", KEY_VALUE, "value", KEY_END),
		    KS_END);
	succeed_if (ks3.lookup ("user:/key3/1"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/2"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3").getString () == "value", "value not correct");
	succeed_if (ks3.size () == 3, "size not correct");

	KeySet ks4 (ks3.dup ());
	succeed_if (ks4.lookup ("user:/key3/1"), "could not find key");
	succeed_if (ks4.lookup ("user:/key3/2"), "could not find key");
	succeed_if (ks4.lookup ("user:/key3/3"), "could not find key");
	succeed_if (ks4.lookup ("user:/key3/3").getString () == "value", "value not correct");
	succeed_if (ks4.size () == 3, "size not correct");

	// ks3.toStream(stdout, 0);
	// ks4.toStream(stdout, 0);
}

TEST (ks, copy)
{
	KeySet ks3 (5, *Key ("user:/key3/1", KEY_END), *Key ("user:/key3/2", KEY_END), *Key ("user:/key3/3", KEY_VALUE, "value", KEY_END),
		    KS_END);

	KeySet ks4 (ks3);
	succeed_if (ks3.size () == 3, "size not correct");
	succeed_if (ks4.size () == 3, "size not correct");

	KeySet ks5;
	ks5.copy (ks4);
	succeed_if (ks4.size () == 3, "size not correct");
	succeed_if (ks5.size () == 3, "size not correct");

	ks5.clear ();
	succeed_if (ks5.size () == 0, "size not correct");


	// ks3.toStream(stdout, 0);
	// ks4.toStream(stdout, 0);
}

TEST (ks, iterate)
{
	KeySet ks3 (5, *Key ("user:/key3/1", KEY_END), *Key ("user:/key3/2", KEY_END), *Key ("user:/key3/3", KEY_VALUE, "value", KEY_END),
		    KS_END);

	Key k1 = ks3.at (0);
	succeed_if (k1.getName () == "user:/key3/1", "wrong keyname");
	succeed_if (k1 == ks3.at (0), "first key not head key");
	Key k2 = ks3.at (1);
	succeed_if (k2.getName () == "user:/key3/2", "wrong keyname");
	Key k3 = ks3.at (2);
	succeed_if (k3.getName () == "user:/key3/3", "wrong keyname");
	succeed_if (k3.getString () == "value", "wrong value");
	succeed_if (k3 == ks3.at (ks3.size () - 1), "last key not tail key");
	succeed_if (!ks3.at (ks3.size ()), "no more key");

	Key null = static_cast<ckdb::Key *> (nullptr);
	succeed_if (!null, "null key");

	for (ssize_t i = 0; i < ks3.size (); i++)
	{
		Key k = ks3.at (i);
		char str[] = "user:/key3/X";

		str[11] = i + '1';
		succeed_if (k.getName () == str, "wrong keyname");
	}

	Key n;
	int j = 0;
	while (j < ks3.size ())
	{
		n = ks3.at (j);
		char str[] = "user:/key3/X";

		str[11] = j + '1';
		succeed_if (n.getName () == str, "wrong keyname");
		j++;
	}

	j = 0;
	while (j < ks3.size ())
	{
		n = ks3.at (j);
		char str[] = "user:/key3/X";

		str[11] = j + '1';
		succeed_if (n.getName () == str, "wrong keyname");
		j++;
	}

	j = 0;
	for (Key k; (k = ks3.at (j));)
	{
		char str[] = "user:/key3/X";

		str[11] = j + '1';
		succeed_if (k.getName () == str, "wrong keyname");
		j++;
	}

	j = 0;
	for (Key k = ks3.at (j); k; (k = ks3.at (j)))
	{
		char str[] = "user:/key3/X";

		str[11] = j + '1';
		succeed_if (k.getName () == str, "wrong keyname");
		j++;
	}
}

TEST (ks, cursor)
{
	KeySet ks3 (5, *Key ("user:/key3/1", KEY_END), *Key ("user:/key3/2", KEY_END), *Key ("user:/key3/3", KEY_VALUE, "value", KEY_END),
		    KS_END);

	for (ssize_t i = 0; i < ks3.size (); i++)
	{
		Key k = ks3.at (i);
	}

	Key k1 = ks3.at (0);
	succeed_if (k1.getName () == "user:/key3/1", "wrong keyname");
	succeed_if (k1 == ks3.at (0), "first key not head key");
}

TEST (ks, pop)
{
	KeySet ks3 (5, *Key ("user:/key3/1", KEY_END), *Key ("user:/key3/2", KEY_END), *Key ("user:/key3/3", KEY_VALUE, "value", KEY_END),
		    KS_END);

	Key k3 = ks3.pop ();
	succeed_if (k3.getName () == "user:/key3/3", "wrong keyname");
	succeed_if (k3.getString () == "value", "wrong value");
	Key k2 = ks3.pop ();
	succeed_if (k2.getName () == "user:/key3/2", "wrong keyname");
	Key k1 = ks3.pop ();
	succeed_if (k1.getName () == "user:/key3/1", "wrong keyname");
	Key k0 = ks3.pop ();
	succeed_if (!k0, "Out of Range, no more key");

	KeySet ks4 (5, *Key ("user:/key3/1", KEY_END), *Key ("user:/key3/2", KEY_END), *Key ("user:/key3/3", KEY_VALUE, "value", KEY_END),
		    KS_END);

	for (int i = ks4.size () - 1; i > 0; i--)
	{
		Key k = ks4.pop ();
		char str[] = "user:/key3/X";

		str[11] = i + '1';
		succeed_if (k.getName () == str, str);
	}
}

TEST (ks, lookup)
{
	KeySet ks3 (5, *Key ("user:/key3/1", KEY_END), *Key ("user:/key3/2", KEY_END), *Key ("user:/key3/3", KEY_VALUE, "value", KEY_END),
		    KS_END);

	Key k1 = ks3.lookup ("user:/key3/1");
	succeed_if (k1, "did not find key");
	succeed_if (k1.getName () == "user:/key3/1", "wrong keyname");

	Key k2 = ks3.lookup ("user:/key3/2");
	succeed_if (k2, "did not find key");
	succeed_if (k2.getName () == "user:/key3/2", "wrong keyname");

	Key k3 = ks3.lookup ("user:/key3/3");
	succeed_if (k3, "did not find key");
	succeed_if (k3.getName () == "user:/key3/3", "wrong keyname");
	succeed_if (k3.getString () == "value", "wrong value");

	Key k4 = ks3.lookup ("user:/key3/4");
	succeed_if (!k4, "Key does not exist");
}

TEST (ks, append)
{
	KeySet ks1;

	KeySet ks2 (5, ckdb::keyNew ("user:/key2", KEY_END), KS_END);
	ks1.append (ks2);

	KeySet ks3 (5, *Key ("user:/key3/1", KEY_END), *Key ("user:/key3/2", KEY_END), *Key ("user:/key3/3", KEY_VALUE, "value", KEY_END),
		    KS_END);
	ks2.append (ks3);
	ks1.append (ks3);
	ks3.append (ks2);

	Key k1 ("user:/key4/1", KEY_END);
	Key k2 ("user:/key4/2", KEY_END);
	Key k3 ("user:/key4/3", KEY_VALUE, "value", KEY_END);
	ks1.append (k1);
	ks1.append (k2);
	ks1.append (k3);
	ks2.append (k1);
	ks2.append (k2);
	ks2.append (k3);
	ks3.append (k1);
	ks3.append (k2);
	ks3.append (k3);

	KeySet ks4 (5, *Key ("user:/key3/1", KEY_END), *Key ("user:/key3/2", KEY_END), *Key ("user:/key3/3", KEY_VALUE, "value", KEY_END),
		    KS_END);

	KeySet ks5;
	std::vector<Key> v (3);
	ks5.append (v[1] = Key ("user:/s/2", KEY_END));
	ks5.append (v[0] = Key ("user:/s/1", KEY_END));
	ks5.append (v[2] = Key ("user:/s/3", KEY_END));

	for (ssize_t i = 0; i < ks5.size (); ++i)
	{
		succeed_if (*ks5.at (i) == *v[i], "wrong order");
	}

	// ks1.toStream();
	// ks2.toStream();
	// ks3.toStream();
}

TEST (ks, permutations)
{
	vector<Key> solution;
	solution.push_back (Key ("user:/s/1", KEY_END));
	solution.push_back (Key ("user:/s/2", KEY_END));
	solution.push_back (Key ("user:/s/3", KEY_END));
	solution.push_back (Key ("user:/s/3/s", KEY_END));
	solution.push_back (Key ("user:/s/3-3", KEY_END));

	vector<Key> permutation (solution);

	do
	{
		KeySet ks;
		ks.append (permutation[0]);
		ks.append (permutation[1]);
		ks.append (permutation[2]);
		ks.append (permutation[3]);
		ks.append (permutation[4]);
		for (ssize_t i = 0; i < ks.size (); ++i)
		{
			succeed_if (*ks.at (i) == *solution[i], "wrong order ");
		}
	} while (next_permutation (permutation.begin (), permutation.end ()));

	solution.push_back (Key ("user:/s/x", KEY_END));
	permutation.push_back (solution[4]); // need a copy of same key, otherwise name is not the same string
	sort (permutation.begin (), permutation.end ());

	do
	{
		KeySet ks;
		ks.append (permutation[0]);
		ks.append (permutation[1]);
		ks.append (permutation[2]);
		ks.append (permutation[3]);
		ks.append (permutation[4]);
		ks.append (permutation[5]);
		for (ssize_t i = 0; i < ks.size (); ++i)
		{
			succeed_if (*ks.at (i) == *solution[i], "wrong order");
		}
	} while (next_permutation (permutation.begin (), permutation.end ()));

	solution.push_back (Key ("user:/x/y", KEY_END));
	permutation.push_back (solution[5]);
	sort (permutation.begin (), permutation.end ());

	do
	{
		KeySet ks;
		ks.append (permutation[0]);
		ks.append (permutation[1]);
		ks.append (permutation[2]);
		ks.append (permutation[3]);
		ks.append (permutation[4]);
		ks.append (permutation[5]);
		ks.append (permutation[6]);
		for (ssize_t i = 0; i < ks.size (); ++i)
		{
			// note: raw pointers check the identity! It needs to be the same reference
			succeed_if (*ks.at (i) == *solution[i], "wrong order");
		}
	} while (next_permutation (permutation.begin (), permutation.end ()));

	solution.push_back (Key ("user:/x/y/z", KEY_END));
	permutation.push_back (solution[5]);
	sort (permutation.begin (), permutation.end ());

	do
	{
		KeySet ks;
		ks.append (permutation[0]);
		ks.append (permutation[1]);
		ks.append (permutation[2]);
		ks.append (permutation[3]);
		ks.append (permutation[4]);
		ks.append (permutation[5]);
		ks.append (permutation[6]);
		ks.append (permutation[7]);
		for (ssize_t i = 0; i < ks.size (); ++i)
		{
			succeed_if (*ks.at (i) == *solution[i], "wrong order");
		}
	} while (next_permutation (permutation.begin (), permutation.end ()));
}

TEST (ks, comparision)
{
	KeySet ks0 (5, KS_END);
	KeySet ks00 (5, KS_END);
	KeySet ks1 (5, *Key ("user:/a", KEY_END), *Key ("user:/b", KEY_END), KS_END);
	KeySet ks11 (5, *Key ("user:/a", KEY_END), *Key ("user:/b", KEY_END), KS_END);
	KeySet ks2 (5, *Key ("user:/a", KEY_END), *Key ("user:/bb", KEY_END), KS_END);
	KeySet ks3 (5, *Key ("user:/aa", KEY_END), *Key ("user:/b", KEY_END), KS_END);
	KeySet ks4 (5, *Key ("user:/aa", KEY_END), *Key ("user:/bb", KEY_END), KS_END);

	EXPECT_EQ (ks0, ks0);
	EXPECT_EQ (ks0, ks00);
	EXPECT_EQ (ks00, ks0);
	EXPECT_EQ (ks00, ks00);

	EXPECT_EQ (ks1, ks1);
	EXPECT_EQ (ks1, ks11);
	EXPECT_EQ (ks11, ks1);
	EXPECT_EQ (ks11, ks11);

	EXPECT_EQ (ks1, ks1);
	EXPECT_NE (ks1, ks0);
	EXPECT_NE (ks1, ks2);
	EXPECT_NE (ks1, ks3);
	EXPECT_NE (ks1, ks4);

	EXPECT_EQ (ks2, ks2);
	EXPECT_NE (ks2, ks0);
	EXPECT_NE (ks2, ks1);
	EXPECT_NE (ks2, ks3);
	EXPECT_NE (ks2, ks4);

	EXPECT_EQ (ks3, ks3);
	EXPECT_NE (ks3, ks0);
	EXPECT_NE (ks3, ks1);
	EXPECT_NE (ks3, ks2);
	EXPECT_NE (ks3, ks4);

	EXPECT_EQ (ks4, ks4);
	EXPECT_NE (ks4, ks0);
	EXPECT_NE (ks4, ks1);
	EXPECT_NE (ks4, ks2);
	EXPECT_NE (ks4, ks3);


	EXPECT_EQ (ks1, ks1);
	EXPECT_NE (ks0, ks1);
	EXPECT_NE (ks2, ks1);
	EXPECT_NE (ks3, ks1);
	EXPECT_NE (ks4, ks1);

	EXPECT_EQ (ks2, ks2);
	EXPECT_NE (ks0, ks2);
	EXPECT_NE (ks1, ks2);
	EXPECT_NE (ks3, ks2);
	EXPECT_NE (ks4, ks2);

	EXPECT_EQ (ks3, ks3);
	EXPECT_NE (ks0, ks3);
	EXPECT_NE (ks1, ks3);
	EXPECT_NE (ks2, ks3);
	EXPECT_NE (ks4, ks3);

	EXPECT_EQ (ks4, ks4);
	EXPECT_NE (ks0, ks4);
	EXPECT_NE (ks1, ks4);
	EXPECT_NE (ks2, ks4);
	EXPECT_NE (ks3, ks4);
}

void call (KeySet ks3)
{
	succeed_if (ks3.lookup ("user:/key3/1"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/2"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3").getString () == "value", "value not correct");
	succeed_if (ks3.size () == 3, "size not correct");
}

void refcall (KeySet & ks3)
{
	succeed_if (ks3.lookup ("user:/key3/1"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/2"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3").getString () == "value", "value not correct");
	succeed_if (ks3.size () == 3, "size not correct");
}

TEST (ks, call)
{
	KeySet ks3 (5, *Key ("user:/key3/1", KEY_END), *Key ("user:/key3/2", KEY_END), *Key ("user:/key3/3", KEY_VALUE, "value", KEY_END),
		    KS_END);
	succeed_if (ks3.lookup ("user:/key3/1"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/2"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3").getString () == "value", "value not correct");
	succeed_if (ks3.size () == 3, "size not correct");

	call (ks3);
	succeed_if (ks3.lookup ("user:/key3/1"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/2"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3").getString () == "value", "value not correct");
	succeed_if (ks3.size () == 3, "size not correct");

	refcall (ks3);
	succeed_if (ks3.lookup ("user:/key3/1"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/2"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3").getString () == "value", "value not correct");
	succeed_if (ks3.size () == 3, "size not correct");
}


void ccall (KeySet ks3)
{
	succeed_if (ks3.lookup ("user:/key3/1"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/2", KDB_O_POP), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3").getString () == "value", "value not correct");
	succeed_if (ks3.size () == 2, "size not correct");

	ks3.lookup ("user:/key3/1").setString ("will change");
	ks3.append (Key ("user:/key3/ccall", KEY_END));
}

void refccall (KeySet & ks3)
{
	succeed_if (ks3.lookup ("user:/key3/1"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/2", KDB_O_POP), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3").getString () == "value", "value not correct");
	succeed_if (ks3.size () == 2, "size not correct");

	ks3.append (Key ("user:/key3/refccall", KEY_END));
	ks3.lookup ("user:/key3/1").setString ("will change again");
}

TEST (ks, call2)
{
	KeySet ks3 (5, *Key ("user:/key3/1", KEY_END), *Key ("user:/key3/2", KEY_END), *Key ("user:/key3/3", KEY_VALUE, "value", KEY_END),
		    KS_END);
	succeed_if (ks3.lookup ("user:/key3/1"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/2"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3").getString () == "value", "value not correct");
	succeed_if (ks3.size () == 3, "size not correct");

	ccall (ks3);
	succeed_if (ks3.lookup ("user:/key3/1"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/1").getString () == "will change", "value did not change");
	succeed_if (ks3.lookup ("user:/key3/2"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3").getString () == "value", "value not correct");
	succeed_if (ks3.size () == 3, "size not correct");
	succeed_if (!ks3.lookup ("user:/key3/ccall"), "key should not be there");

	refccall (ks3);
	succeed_if (ks3.lookup ("user:/key3/1"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/1").getString () == "will change again", "value did not change");
	succeed_if (!ks3.lookup ("user:/key3/2"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3").getString () == "value", "value not correct");
	succeed_if (ks3.lookup ("user:/key3/refccall"), "could not find key");
	succeed_if (ks3.size () == 3, "size not correct");
}


void rcopycall (KeySet ks)
{
	// do something with keyset
	// (wont be one hierarchy higher)
	ks.append (Key ("user:/yyy", KEY_END));
}

void rrefcall (KeySet & ks)
{
	// do something with keyset
	ks.append (Key ("user:/xxx", KEY_END));
}

/*Calling conventions: user need to free the keyset */
void rcall (KeySet ks)
{
	// do something with ks

	rrefcall (ks);
	succeed_if (ks.lookup ("user:/xxx"), "could not find key");


	rcopycall (ks);
	succeed_if (ks.lookup ("user:/xxx"), "could not find key");
	succeed_if (!ks.lookup ("user:/yyy"), "could not find key");

	// don't destroy ks
	ks.release ();
}

TEST (ks, release)
{
	KeySet ks1;

	ckdb::KeySet * ks = ks1.release ();
	ckdb::ksDel (ks);

	KeySet ks2 (5, ckdb::keyNew ("user:/key2", KEY_END), KS_END);

	ks = ks2.release ();
	ckdb::ksDel (ks);

	KeySet ks3 (5, *Key ("user:/key3/1", KEY_END), *Key ("user:/key3/2", KEY_END), *Key ("user:/key3/3", KEY_VALUE, "value", KEY_END),
		    KS_END);

	ks = ks3.release ();
	ckdb::ksDel (ks);

	ks = ckdb::ksNew (5, ckdb::keyNew ("user:/abc", KEY_END), KS_END);
	rcall (ks);
	succeed_if (ckdb::ksLookupByName (ks, "user:/xxx", 0) != nullptr, "could not find key");
	ckdb::ksDel (ks);
}

TEST (ks, lookupPop)
{
	KeySet ks3 (5, *Key ("user:/key3/1", KEY_END), *Key ("user:/key3/2", KEY_END), *Key ("user:/key3/3", KEY_VALUE, "value", KEY_END),
		    KS_END);

	succeed_if (ks3.size () == 3, "size not correct");

	Key k3 = ks3.lookup ("user:/key3/3", KDB_O_POP);
	succeed_if (k3.getName () == "user:/key3/3", "wrong keyname");
	succeed_if (k3.getString () == "value", "wrong value");
	succeed_if (ks3.size () == 2, "size not correct");

	Key k1 = ks3.lookup ("user:/key3/1", KDB_O_POP);
	succeed_if (k1.getName () == "user:/key3/1", "wrong keyname");
	succeed_if (ks3.size () == 1, "size not correct");

	Key k2 = ks3.lookup ("user:/key3/2", KDB_O_POP);
	succeed_if (k2.getName () == "user:/key3/2", "wrong keyname");
	succeed_if (ks3.size () == 0, "size not correct");

	Key k0 = ks3.lookup ("user:/key3/2", KDB_O_POP);
	succeed_if (!k0, "Out of Range, no more key");
	succeed_if (ks3.size () == 0, "size not correct");

	Key kn = ks3.lookup ("user:/key3/n", KDB_O_POP);
	succeed_if (!kn, "key was never in set");
	succeed_if (ks3.size () == 0, "size not correct");

	KeySet ks4 (5, *Key ("user:/key3/1", KEY_END), *Key ("user:/key3/2", KEY_END), *Key ("user:/key3/3", KEY_VALUE, "value", KEY_END),
		    KS_END);

	for (ssize_t i = ks4.size (); i > 0; i--)
	{
		char str[] = "user:/key3/X";

		str[11] = i + '0';
		succeed_if (ks4.size () == i, "size not correct");
		Key k = ks4.lookup (str, KDB_O_POP);
		succeed_if (k, "there should be a key");

		succeed_if (k.getName () == str, str);
		succeed_if (ks4.size () == i - 1, "size not correct");
	}

	KeySet ks5 (5, *Key ("user:/key3/1", KEY_END), *Key ("user:/key3/2", KEY_END), *Key ("user:/key3/3", KEY_VALUE, "value", KEY_END),
		    KS_END);

	for (ssize_t i = ks5.size (); i > 0; i--)
	{
		char str[] = "user:/key3/X";

		str[11] = i + '0';
		Key searchKey (str, KEY_END);
		succeed_if (ks5.size () == i, "size not correct");

		Key k = ks5.lookup (searchKey, KDB_O_POP);
		succeed_if (k, "there should be a key");

		succeed_if (k.getName () == str, str);
		succeed_if (ks5.size () == i - 1, "size not correct");
	}
}

TEST (ks, duplicate)
{
	KeySet ks3 (5, *Key ("user:/key3/1", KEY_END), *Key ("user:/key3/2", KEY_END), *Key ("user:/key3/3", KEY_VALUE, "value", KEY_END),
		    KS_END);
	succeed_if (ks3.lookup ("user:/key3/1"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/2"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3"), "could not find key");
	succeed_if (ks3.lookup ("user:/key3/3").getString () == "value", "value not correct");
	succeed_if (ks3.size () == 3, "size not correct");

	KeySet ks4;
	ks4 = ks3;
	succeed_if (ks4.lookup ("user:/key3/1"), "could not find key");
	succeed_if (ks4.lookup ("user:/key3/2"), "could not find key");
	succeed_if (ks4.lookup ("user:/key3/3"), "could not find key");
	succeed_if (ks4.lookup ("user:/key3/3").getString () == "value", "value not correct");
	succeed_if (ks4.size () == 3, "size not correct");

	// ks3.toStream(stdout, 0);
	// ks4.toStream(stdout, 0);
}

KeySet fill_vaargs (size_t size, ...)
{
	va_list ap;
	va_start (ap, size);
	KeySet ks (VaAlloc (size), ap);
	va_end (ap);
	return ks;
}

struct C
{
	KeySet ks;
};

TEST (ks, move)
{

	std::unique_ptr<KeySet> u1 (new KeySet (5, *Key ("user:/key3/1", KEY_END), *Key ("user:/key3/2", KEY_END),
						*Key ("user:/key3/3", KEY_VALUE, "value", KEY_END), KS_END));
	std::unique_ptr<KeySet> u2 (std::move (u1));
	std::unique_ptr<KeySet> u3 = std::move (u1);

	std::unique_ptr<C> c1 (new C);
	std::unique_ptr<C> c2 (std::move (c1));
	std::unique_ptr<C> c3 = std::move (c1);
	c3->ks = *u3;
}

TEST (ks, vaargs)
{
	KeySet ks = fill_vaargs (20, *Key ("user:/a", KEY_END), *Key ("user:/b", KEY_END), KS_END);
	succeed_if (ks.lookup ("user:/a"), "could not find key");
	succeed_if (ks.lookup ("user:/b"), "could not find key");
}

/* check the wrappers for underlying c-functions with a key that was added to a KeySet
 * exceptions are thrown if the underlying c-functions return error codes (-1 or NULL) */
TEST (ks, cErrosKeySet)
{
	Key k ("user:/key", KEY_VALUE, "testkey", KEY_END);
	KeySet ks;

	ks.append (k);
	EXPECT_THROW (k.addName ("test"), KeyInvalidName);
	EXPECT_THROW (k.setName ("test"), KeyInvalidName);
	EXPECT_THROW (k.addBaseName ("test"), KeyInvalidName);
	EXPECT_THROW (k.setBaseName ("test"), KeyInvalidName);
	EXPECT_THROW (k.delBaseName (), KeyInvalidName);

	EXPECT_NO_THROW (k.set ("test"));

	/* Key::copy tests */
	Key k1 ("user:/key1", KEY_VALUE, "testkey1", KEY_END);
	EXPECT_THROW (k.copy (k), KeyException);
	EXPECT_NO_THROW (k1.copy (k1));
	EXPECT_EQ (k1, k1);

	/* copying k1 to k should not work */
	EXPECT_THROW (k.copy (k1), KeyException);

	/* copying k to k1 should work */
	EXPECT_NE (k1, k);
	EXPECT_NO_THROW (k1.copy (k));
	EXPECT_EQ (k1, k);

	EXPECT_NO_THROW (k.setString ("newValue"));
	EXPECT_EQ (k.getString (), "newValue");

	EXPECT_EQ (k.getReferenceCounter (), 2);

	ks.clear ();
	EXPECT_EQ (k.getReferenceCounter (), 1);

	/* should only fail on null key */
	EXPECT_NO_THROW (k--);
	EXPECT_EQ (k.getReferenceCounter (), 0);

	/* should only fail on null key */
	EXPECT_NO_THROW (k.clear ());
}
