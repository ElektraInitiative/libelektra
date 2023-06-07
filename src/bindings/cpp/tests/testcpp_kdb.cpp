/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests.hpp>


TEST (kdb, get_set)
{
	// cout << "testing kdbSet() and kdbGet()" << endl;

	{
		KeySet ks_set (5, *Key ("user:/tests/key3", KEY_END), *Key ("user:/tests/key3/1", KEY_END),
			       *Key ("user:/tests/key3/2", KEY_END), *Key ("user:/tests/key3/3", KEY_VALUE, "value", KEY_END), KS_END);
		KeySet ks;
		KDB kdb;
		kdb.get (ks, "user:/tests/key3");
		ks.append (ks_set);
		kdb.set (ks, "user:/tests/key3");
	}

	// check if they were written
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, "user:/tests/key3");
		exit_if_fail (ks.lookup ("user:/tests/key3/3"), "could not find previously written key");
		succeed_if (ks.lookup ("user:/tests/key3/3").get<std::string> () == "value", "could not get value");
	}

	// now remove keys (cleanup)
	{
		KeySet ks;
		KDB kdb;
		kdb.get (ks, "user:/tests/key3");
		ks.cut (Key ("user:/tests/key3", KEY_END));
		kdb.set (ks, "user:/tests/key3");
	}

	// check if its gone now
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, "user:/tests/key3");
		succeed_if (!ks.lookup ("user:/tests/key3/3"), "key was not removed");
	}
}

TEST (kdb, calculateChanges)
{
	KeySet ks;
	KDB kdb;
	kdb.get (ks, "user:/tests/difftest");
	ks.append (*Key ("user:/tests/difftest/added", KEY_END));


	Key parent;
	auto diff = kdb.calculateChanges (ks, parent);
	succeed_if (diff.isEmpty () == false, "diff should not be empty");

	auto diff2 = kdb.calculateChanges (ks, "user:/tests");
	succeed_if (diff.isEmpty () == false, "diff should not be empty");
}
