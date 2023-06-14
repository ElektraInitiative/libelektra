/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./tests.hpp"

#include <memory>
#include <stdexcept>
#include <string>
#include <vector>


TEST (key, null)
{
	Key key0 (static_cast<ckdb::Key *> (nullptr));
	succeed_if (!key0, "key should evaluate to false");
	succeed_if (key0.isNull (), "key should evaluate to false");

	key0 = static_cast<ckdb::Key *> (nullptr);
	succeed_if (!key0, "key should evaluate to false");
	succeed_if (key0.isNull (), "key should evaluate to false");

	key0.release ();
	succeed_if (!key0, "key should evaluate to false");
	succeed_if (key0.isNull (), "key should evaluate to false");
}


TEST (key, typebool)
{
	Key k ("user:/key", KEY_VALUE, "testkey", KEY_END);
	EXPECT_THROW (k.get<bool> (), KeyTypeConversion);
	k.setString ("O");
	EXPECT_THROW (k.get<bool> (), KeyTypeConversion);
	k.setString ("0a");
	EXPECT_THROW (k.get<bool> (), KeyTypeConversion);
	k.setString ("true");
	EXPECT_THROW (k.get<bool> (), KeyTypeConversion);
	k.setString ("false");
	EXPECT_THROW (k.get<bool> (), KeyTypeConversion);
	k.setString ("f");
	EXPECT_THROW (k.get<bool> (), KeyTypeConversion);
	k.setString ("t");
	EXPECT_THROW (k.get<bool> (), KeyTypeConversion);
	k.setString ("ON");
	EXPECT_THROW (k.get<bool> (), KeyTypeConversion);
	k.setString ("OFF");
	EXPECT_THROW (k.get<bool> (), KeyTypeConversion);
	k.setString ("on");
	EXPECT_THROW (k.get<bool> (), KeyTypeConversion);
	k.setString ("off");
	EXPECT_THROW (k.get<bool> (), KeyTypeConversion);

	k.setString ("0");
	ASSERT_TRUE (!k.get<bool> ());
	k.setString ("1");
	ASSERT_TRUE (k.get<bool> ());
}


TEST (key, keynew)
{
	char array[] = "here is some data stored";

	Key key0;
	succeed_if (key0, "key should evaluate to true");
	succeed_if (key0.getName () == "/", "key0 has wrong name");

	// Key with name
	Key key2 ("system:/sw/test", KEY_END);
	succeed_if (key2.getBaseName () == "test", "wrong base name");
	succeed_if (key2.getName () == "system:/sw/test", "key2 has wrong name");
	// succeed_if (key2.getDirName() == "system:/sw", "wrong dir name");
	key2.copy (key0);
	succeed_if (key2.getName () == "/", "key0 has wrong name");
	succeed_if (key2.getBaseName () == "", "wrong base name");
	// succeed_if (key2.getDirName() == "", "wrong dir name");


	// Key with name
	Key key3 ("system:/sw/test", KEY_END);
	succeed_if (key3.getName () == "system:/sw/test", "key3 has wrong name");
	succeed_if (key3.getBaseName () == "test", "wrong base name");
	// succeed_if (key3.getDirName() == "system:/sw", "wrong dir name");
	key3.setName ("system:/other/name");
	succeed_if (key3.getName () == "system:/other/name", "key3 has wrong name");
	succeed_if (key3.getBaseName () == "name", "wrong base name");
	// succeed_if (key3.getDirName() == "system:/other", "wrong dir name");
	key3.addBaseName ("base");
	succeed_if (key3.getName () == "system:/other/name/base", "key3 has wrong name");
	succeed_if (key3.getBaseName () == "base", "wrong base name");
	// succeed_if (key3.getDirName() == "system:/other/name", "wrong dir name");
	key3.setBaseName ("name");
	succeed_if (key3.getName () == "system:/other/name/name", "key3 has wrong name");
	succeed_if (key3.getBaseName () == "name", "wrong base name");
	// succeed_if (key3.getDirName() == "system:/other/name", "wrong dir name");
	key3.setName ("system:/name");
	succeed_if (key3.getName () == "system:/name", "key3 has wrong name");
	succeed_if (key3.getBaseName () == "name", "wrong base name");
	// succeed_if (key3.getDirName() == "system", "wrong dir name");

	// Key with slash in name name
	key3.setName ("system:/name\\/slash");
	succeed_if (key3.getName () == "system:/name\\/slash", "key3 has wrong name");
	succeed_if (key3.getBaseName () == "name/slash", "wrong base name");
	// succeed_if (key3.getDirName() == "system", "wrong dir name");

	key3.setName ("system:/name/with\\/slash");
	succeed_if (key3.getName () == "system:/name/with\\/slash", "key3 has wrong name");
	succeed_if (key3.getBaseName () == "with/slash", "wrong base name");
	// succeed_if (key3.getDirName() == "system:/name", "wrong dir name");

	key3.setName ("system:/name");
	key3.addName ("some\\/more");
	succeed_if (key3.getName () == "system:/name/some\\/more", "key3 has wrong name");
	succeed_if (key3.getBaseName () == "some/more", "wrong base name");
	// succeed_if (key3.getDirName() == "system:/name", "wrong dir name");

	key3.setName ("/name");
	succeed_if (key3.getName () == "/name", "key3 has wrong name");
	// succeed_if(key3.getBaseName() == "name", "wrong base name");

	key3.setName ("/name");
	key3.addName ("some\\/more");
	succeed_if (key3.getName () == "/name/some\\/more", "key3 has wrong name");
	succeed_if (key3.getBaseName () == "some/more", "wrong base name");
	// succeed_if (key3.getDirName() == "/name", "wrong dir name");

	// Key with name + value
	Key key4 ("system:/sw/test", KEY_VALUE, "test", KEY_END);
	succeed_if (key4.getName () == "system:/sw/test", "key4 has wrong name");
	succeed_if (key4.getString () == "test", "key4 has wrong value");
	succeed_if (key4.get<string> () == "test", "key4 has wrong value");
	succeed_if (key4.getStringSize () == 5, "key4 has wrong value size");
	key4.setString ("test");
	succeed_if (key4.getString () == "test", "key4 has wrong value");
	succeed_if (key4.get<string> () == "test", "key4 has wrong value");
	succeed_if (key4.getStringSize () == 5, "key4 has wrong value size");
	try
	{
		key4.getBinary ();
		succeed_if (false, "string key did not throw after getting binary");
	}
	catch (KeyTypeMismatch const & ktm)
	{
		succeed_if (true, "string key did not throw after getting binary");
	}

	try
	{
		key4.get<int> ();
		succeed_if (false, "string key did not throw after int");
	}
	catch (KeyTypeConversion const & ktm)
	{
		succeed_if (true, "string key did not throw after getting binary");
	}

	key4.setString ("1.1");
	try
	{
		key4.get<int> ();
		succeed_if (false, "string key did not throw after ip");
	}
	catch (KeyTypeConversion const & ktm)
	{
		succeed_if (true, "string key did not throw after getting ip");
	}

	key4.setString ("");
	succeed_if (key4.getString () == "", "key4 has wrong value");
	succeed_if (key4.isString (), "key4 is not string");
	succeed_if (!key4.isBinary (), "key4 is not string");
	succeed_if (key4.get<string> () == "", "key4 has wrong value");
	succeed_if (key4.getStringSize () == 1, "key4 has wrong value size");
	key4.setBinary ("abc", 3);
	succeed_if (key4.isBinary (), "key4 is not string");
	succeed_if (!key4.isString (), "key4 is not string");
	try
	{
		key4.getString ();
		succeed_if (false, "binary key did not throw after getting string");
	}
	catch (KeyTypeMismatch const & ktm)
	{
		succeed_if (true, "binary key did not throw after getting string");
	}
	std::string s ("abc");
	s.resize (3);
	succeed_if (key4.getBinary () == s, "key4 has wrong binary value");
	succeed_if (key4.getBinarySize () == 3, "key4 has wrong value size");
	s[1] = 0;
	key4.setBinary ("a\0c", 3);
	succeed_if (key4.getBinary () == s, "key4 has wrong binary value");
	succeed_if (key4.getBinarySize () == 3, "key4 has wrong value size");

	Key key8 ("system:/valid/there", KEY_BINARY, KEY_SIZE, sizeof (array), KEY_VALUE, array, KEY_END);
	succeed_if (key8.getName () == "system:/valid/there", "key8 has wrong name");
	succeed_if (key8.isBinary (), "Key should be binary");
	succeed_if (!key8.isString (), "Key should be binary");
	succeed_if (key8.getBinarySize () == sizeof (array), "Value size not correct");
	std::string getBack = key8.getBinary ();
	succeed_if (memcmp (&getBack[0], array, sizeof (array)) == 0, "could not get correct value with keyGetBinary");
	succeed_if (key8.getBaseName () == "there", "wrong base name");

	Key key9 ("system:/valid/.inactive", KEY_META, "comment/#0", "inactive key", KEY_END);
	succeed_if (key9.getMeta<std::string> ("comment/#0") == "inactive key", "comment failed");
	succeed_if (key9.getBaseName () == ".inactive", "wrong base name");

	std::string name = "system:/valid/name";
	Key keyA (name, KEY_END);
	succeed_if (keyA.getName () == "system:/valid/name", "keyA has wrong name");
	succeed_if (keyA.getBaseName () == "name", "keyA wrong base name");

	Key keyB ("/", KEY_END);
	keyB.setBinary (nullptr, 0);
	succeed_if (keyB.isBinary (), "should be binary");
	succeed_if (keyB.getBinary () == "", "Binary should be a nullpointer");
	succeed_if (keyB.getValue () == nullptr, "Binary should be a nullpointer");

	keyB.setBinary (nullptr, 1);
	succeed_if (keyB.isBinary (), "should be binary");
	succeed_if (keyB.getBinary () == "", "Binary should be a nullpointer");
	succeed_if (keyB.getValue () == nullptr, "Binary should be a nullpointer");
}

TEST (key, constructor)
{
	ckdb::Key * ck = ckdb::keyNew ("/", KEY_END);
	Key k = ck; // constructor with (ckdb::Key)

	/*
	cout << "ck:   " << (void*)ck << endl;
	cout << "k:    " << (void*)&k << endl;
	cout << "k.ck: " << (void*)k.getKey() << endl;
	*/

	k.set<int> (30);
	succeed_if (k.get<int> () == 30, "could not get same int");
}

TEST (key, set)
{
	ckdb::Key * ck;
	Key k;

	ck = ckdb::keyNew ("/", KEY_END);
	k = ck; // operator= alias for setKey()

	/*
	cout << "ck:   " << (void*)ck << endl;
	cout << "k:    " << (void*)&k << endl;
	cout << "k.ck: " << (void*)k.getKey() << endl;
	*/

	k.set<int> (30);
	succeed_if (k.get<int> () == 30, "could not get same int");
}

TEST (key, cast)
{
	ckdb::Key * ck;
	Key * k;

	ck = ckdb::keyNew ("/", KEY_END);
	k = reinterpret_cast<Key *> (&ck); // not copied on purpose

	/*
	cout << "&ck:  " << (void*)&ck << endl;
	cout << "k:    " << (void*)&k << endl;
	cout << "ck:   " << (void*)ck << endl;
	cout << "k.ck: " << (void*)k->getKey() << endl;
	*/

	k->set<int> (30);
	succeed_if (k->get<int> () == 30, "could not get same int");

	ckdb::keyDel (ck);
}

TEST (key, value)
{
	// cout << "testing value" << endl;
	Key test;
	succeed_if (test.getString () == "", "String should be empty");

	test.setString ("23.3");
	succeed_if (test.get<double> () >= 23.2, "could not get same double");
	succeed_if (test.get<double> () <= 23.4, "could not get same double");
	succeed_if (test.getBinarySize () == 5, "value size not correct");

	test.setString ("401");
	succeed_if (test.get<int> () == 401, "could not get same int");
	succeed_if (test.getBinarySize () == 4, "value size not correct");

	test.setString ("mystr");
	succeed_if (test.get<string> () == "mystr", "could not get same string");
	succeed_if (test.getBinarySize () == 6, "value size not correct");

	test.setString ("myoth");
	succeed_if (test.get<string> () == "myoth", "could not get same string");
	succeed_if (test.getBinarySize () == 6, "value size not correct");

	test.set<double> (23.3);
	succeed_if (test.getString () == "23.3", "could not get same double");
	succeed_if (test.getBinarySize () == 5, "value size not correct");

	test.set<int> (401);
	succeed_if (test.getString () == "401", "could not get same int");
	succeed_if (test.getBinarySize () == 4, "value size not correct");

	test.set<string> ("mystr");
	succeed_if (test.getString () == "mystr", "could not get same string");
	succeed_if (test.getBinarySize () == 6, "value size not correct");

	test.set<string> ("myoth");
	succeed_if (test.getString () == "myoth", "could not get same string");
	succeed_if (test.getBinarySize () == 6, "value size not correct");

	test.setMeta<std::string> ("comment/#0", "mycomment");
	succeed_if (test.getMeta<std::string> ("comment/#0") == "mycomment", "could not get same comment");
}

TEST (key, exceptions)
{
	Key test;

	try
	{
		test.setName ("no");
	}
	catch (kdb::KeyInvalidName const &)
	{
		succeed_if (test.getName () == "/", "name not unchanged after trying to set invalid name to key");
	}

	test.setName ("user:/name");
	succeed_if (test.getName () == "user:/name", "could not get same name");

	try
	{
		test.setName ("no");
	}
	catch (kdb::KeyInvalidName const &)
	{
		succeed_if (test.getName () == "user:/name", "name not unchanged after trying to set invalid name to key");
	}

	try
	{
		test.addName ("no");
	}
	catch (kdb::KeyInvalidName const &)
	{
		succeed_if (test.getName () == "user:/name", "name not unchanged after trying to add invalid name to key");
	}

	Key test1;
	EXPECT_THROW (test1.setBaseName ("no"), KeyInvalidName); // null keys should throw this when setBaseName is called

	Key test2;
	EXPECT_THROW (test2.delBaseName (), KeyInvalidName); // null keys should throw this when delBaseName is called
}

TEST (key, name)
{
	Key test;
	succeed_if (test.getName () == "/", "Name should be empty");

	test.setName ("user:/test");
	test.setMeta<std::string> ("owner", "gerald");
	succeed_if (test.getName () == "user:/test", "Wrong name");
	succeed_if (test.getMeta<std::string> ("owner") == "gerald", "Wrong owner");
	succeed_if (test.getNameSize () == 11, "wrong name size");
	succeed_if (!test.isCascading (), "key is cascading");
	succeed_if (!test.isSpec (), "key is spec");
	succeed_if (!test.isProc (), "key is proc");
	succeed_if (!test.isDir (), "key is dir");
	succeed_if (test.isUser (), "key is not user");
	succeed_if (!test.isSystem (), "key is system");

	test.setName ("system:/test");
	test.setMeta<std::string> ("owner", "markus");
	succeed_if (test.getName () == "system:/test", "Wrong name");
	succeed_if (test.getMeta<std::string> ("owner") == "markus", "Wrong owner");
	succeed_if (test.getMeta<std::string> ("owner") == "markus", "Wrong owner");
	succeed_if (test.getNameSize () == 13, "wrong name size");
	succeed_if (!test.isCascading (), "key is cascading");
	succeed_if (!test.isSpec (), "key is spec");
	succeed_if (!test.isProc (), "key is proc");
	succeed_if (!test.isDir (), "key is dir");
	succeed_if (!test.isUser (), "key is not user");
	succeed_if (test.isSystem (), "key is system");

	test.setName ("dir:/test");
	succeed_if (test.getName () == "dir:/test", "Wrong name");
	succeed_if (test.getNameSize () == 10, "wrong name size");
	succeed_if (!test.isCascading (), "key is cascading");
	succeed_if (!test.isSpec (), "key is spec");
	succeed_if (!test.isProc (), "key is proc");
	succeed_if (test.isDir (), "key is not dir");
	succeed_if (!test.isUser (), "key is user");
	succeed_if (!test.isSystem (), "key is system");

	test.setName ("proc:/test");
	succeed_if (test.getName () == "proc:/test", "Wrong name");
	succeed_if (test.getNameSize () == 11, "wrong name size");
	succeed_if (!test.isCascading (), "key is cascading");
	succeed_if (!test.isSpec (), "key is spec");
	succeed_if (test.isProc (), "key is not proc");
	succeed_if (!test.isDir (), "key is dir");
	succeed_if (!test.isUser (), "key is user");
	succeed_if (!test.isSystem (), "key is system");

	test.setName ("spec:/test");
	succeed_if (test.getName () == "spec:/test", "Wrong name");
	succeed_if (test.getNameSize () == 11, "wrong name size");
	succeed_if (!test.isCascading (), "key is cascading");
	succeed_if (test.isSpec (), "key is not spec");
	succeed_if (!test.isProc (), "key is proc");
	succeed_if (!test.isDir (), "key is dir");
	succeed_if (!test.isUser (), "key is user");
	succeed_if (!test.isSystem (), "key is system");

	test.setName ("/test");
	succeed_if (test.getName () == "/test", "Wrong name");
	succeed_if (test.getNameSize () == 6, "wrong name size");
	succeed_if (test.isCascading (), "key is not cascading");
	succeed_if (!test.isSpec (), "key is not spec");
	succeed_if (!test.isProc (), "key is proc");
	succeed_if (!test.isDir (), "key is dir");
	succeed_if (!test.isUser (), "key is user");
	succeed_if (!test.isSystem (), "key is system");


	test.setName ("user:/dir/test");
	test.setBaseName ("mykey");
	succeed_if (test.getName () == "user:/dir/mykey", "Basename did not work");
	test.setName (test.getName () + "/onedeeper");		     // add basename is trivial
	succeed_if (test.getName ().find ('/') == 5, "user length"); // keyGetRootNameSize trivial

	// so we finally got a name, lets test below
	succeed_if (test.getName () == "user:/dir/mykey/onedeeper", "Basename did not work");

	succeed_if (test.isBelow (Key ("user:/", KEY_END)), "key is below");
	succeed_if (test.isBelow (Key ("user:/dir", KEY_END)), "key is below");
	succeed_if (test.isBelow (Key ("user:/dir/mykey", KEY_END)), "key is below");
	succeed_if (!test.isBelow (Key ("user:/dir/mykey/onedeeper", KEY_END)), "key is not below (but same)");
	succeed_if (!test.isBelow (Key ("user:/otherdir", KEY_END)), "key is not below");

	succeed_if (test.isBelowOrSame (Key ("user:/", KEY_END)), "key is below");
	succeed_if (test.isBelowOrSame (Key ("user:/dir", KEY_END)), "key is below");
	succeed_if (test.isBelowOrSame (Key ("user:/dir/mykey", KEY_END)), "key is below");
	succeed_if (test.isBelowOrSame (Key ("user:/dir/mykey/onedeeper", KEY_END)), "key is same");
	succeed_if (!test.isBelowOrSame (Key ("user:/otherdir", KEY_END)), "key is not below");

	succeed_if (test.isDirectBelow (Key ("user:/dir/mykey", KEY_END)), "key is direct below");
	succeed_if (!test.isDirectBelow (Key ("user:/dir/test", KEY_END)), "key is not direct below");
	succeed_if (!test.isDirectBelow (Key ("user:/dir", KEY_END)), "key is not direct below");
	succeed_if (!test.isDirectBelow (Key ("user:/dir/otherdir", KEY_END)), "key is not direct below");
	succeed_if (!test.isDirectBelow (Key ("user:/otherdir", KEY_END)), "key is not direct below");
	succeed_if (!test.isDirectBelow (Key ("user:/", KEY_END)), "key is not direct below");

	test.setName ("system:/elektra");
	succeed_if (test.isBelow (Key ("system:/", KEY_END)), "system:/elektra is not below system");
	test.setName ("system:/");
	succeed_if (!test.isBelow (Key ("system:/elektra", KEY_END)), "system is below system:/elektra");
}

void f (Key)
{
	Key h ("user:/infunction", KEY_END);
}

TEST (key, ref)
{
	Key zgr1 ("user:/zgr1", KEY_END);
	{
		Key zgr2 ("user:/zgr2", KEY_END);
		Key zgr3 ("user:/zgr3", KEY_END);
		Key zgr4 ("user:/zgr4", KEY_END);
		Key zgr5 ("user:/zgr5", KEY_END);
		zgr2 = zgr1;
		zgr3 = zgr1;
		zgr4 = zgr1;
	}

	f (zgr1);
	f (Key ("user:/passed", KEY_END));

	Key test;
	test.setName ("user:/test");

	Key ref1;
	ref1 = test; // operator =
	succeed_if (*ref1 == *test, "should point to the same object");

	succeed_if (test.getName () == "user:/test", "wrong name");
	succeed_if (ref1.getName () == "user:/test", "ref key wrong name");

	Key ref2 = test; // copy constructor
	succeed_if (*ref2 == *test, "should point to the same object");

	succeed_if (test.getName () == "user:/test", "wrong name");
	succeed_if (ref2.getName () == "user:/test", "ref key wrong name");

	const Key consttest ("user:/test", KEY_END);
	Key ref3 = consttest; // const copy constructor
	succeed_if (*ref3 == *consttest, "should point to the same object");

	succeed_if (consttest.getName () == "user:/test", "wrong name");
	succeed_if (ref3.getName () == "user:/test", "ref key wrong name");
}

TEST (key, dup)
{
	Key test;
	test.setName ("user:/test");

	Key dup0 = test.dup (); // directly call of dup()

	succeed_if (test.getName () == "user:/test", "wrong name");
	succeed_if (dup0.getName () == "user:/test", "dup key wrong name");

	Key dup1 = test.dup (); // directly call of dup()
	succeed_if (dup1.getName () == "user:/test", "dup key wrong name");

	succeed_if (*test != *dup0, "should be other key");
	succeed_if (*test != *dup1, "should be other key");
}

TEST (key, valid)
{
	Key i1;
	succeed_if (i1.isValid (), "key should be valid");
	succeed_if (i1, "even though it is invalid, it is still not a null key");

	Key i2 ("/", KEY_END);
	succeed_if (i2.isValid (), "key should be valid");
	succeed_if (i2, "even though it is invalid, it is still not a null key");

	vector<string> invalid_names;
	// invalid_names.push_back ("/abc");
	// invalid_names.push_back ("/");
	invalid_names.push_back ("use");
	invalid_names.push_back ("users");
	invalid_names.push_back ("users:");
	invalid_names.push_back ("syste");
	invalid_names.push_back ("systems");
	invalid_names.push_back ("system:abc");
	invalid_names.push_back ("error/somthing");
	invalid_names.push_back (".");
	invalid_names.push_back ("..");

	for (auto & invalid_name : invalid_names)
	{
		EXPECT_THROW (Key (invalid_name, KEY_END), kdb::KeyInvalidName);
	}

	Key v1 ("user:/", KEY_END);
	succeed_if (v1.isValid (), "key should be valid");
	succeed_if (v1, "should be non-null too");

	Key v2 ("system:/", KEY_END);
	succeed_if (v2.isValid (), "key should be valid");
	succeed_if (v2, "should be non-null too");

	vector<string> valid_names;
	valid_names.push_back ("user:/abc");
	valid_names.push_back ("user:/s");
	valid_names.push_back ("system:/s");
	valid_names.push_back ("user:/error/somthing");
	valid_names.push_back ("system:/");
	valid_names.push_back ("user:/.");
	valid_names.push_back ("system:/abc/..");
	valid_names.push_back ("system:/abc/../more");

	for (auto & valid_name : valid_names)
	{
		Key v3 (valid_name, KEY_END);
		succeed_if (v3.isValid (), "key should be valid");
		succeed_if (v3, "should not be a null key");
	}
}

TEST (key, clear)
{
	Key k1 ("user:/", KEY_END);
	Key k2 = k1;
	Key k3 = k1;

	succeed_if (k1.isValid (), "key should be valid");
	succeed_if (k2.isValid (), "key should be valid");
	succeed_if (k3.isValid (), "key should be valid");

	succeed_if (k1.getName () == "user:/", "name should be user");
	succeed_if (k2.getName () == "user:/", "name should be user");
	succeed_if (k3.getName () == "user:/", "name should be user");


	k1.clear ();

	succeed_if (k1.isValid (), "key should be valid");
	succeed_if (k2.isValid (), "key should be valid");
	succeed_if (k3.isValid (), "key should be valid");

	succeed_if (k1.getName () == "/", "name should be empty");
	succeed_if (k2.getName () == "/", "name should be empty");
	succeed_if (k3.getName () == "/", "name should be empty");

	k1.setMeta ("test_meta", "meta_value");
	succeed_if (k1.getMeta<std::string> ("test_meta") == "meta_value", "metadata not set correctly");
	succeed_if (k2.getMeta<std::string> ("test_meta") == "meta_value", "metadata not set correctly");
	succeed_if (k3.getMeta<std::string> ("test_meta") == "meta_value", "metadata not set correctly");

	k2.clear ();

	succeed_if (!k1.getMeta<const Key> ("test_meta"), "metadata not set correctly");
	succeed_if (!k2.getMeta<const Key> ("test_meta"), "metadata not set correctly");
	succeed_if (!k3.getMeta<const Key> ("test_meta"), "metadata not set correctly");
}

TEST (key, conversation)
{
	Key k1 ("user:/", KEY_END);
	ckdb::Key * ck1 = k1.getKey ();
	succeed_if (!strcmp (ckdb::keyName (ck1), "user:/"), "c key does not have correct name");
	succeed_if (!strcmp (ckdb::keyName (*k1), "user:/"), "c key does not have correct name");

	ck1 = k1.release ();
	succeed_if (!strcmp (ckdb::keyName (ck1), "user:/"), "c key does not have correct name");
	ckdb::keyDel (ck1);
}

TEST (key, keynamespace)
{
	succeed_if (Key ("user:/", KEY_END).getNamespace () == ElektraNamespace::USER, "namespace wrong");
	succeed_if (Key ("user:/a", KEY_END).getNamespace () == ElektraNamespace::USER, "namespace wrong");
	// std::cout << Key ("user:/a", KEY_END).getNamespace () << std::endl;
	succeed_if (Key ("user:/a/b/c", KEY_END).getNamespace () == ElektraNamespace::USER, "namespace wrong");
	succeed_if (Key ("user:/a/..", KEY_END).getNamespace () == ElektraNamespace::USER, "namespace wrong");
	succeed_if (Key ("user:/a/../x/f/v", KEY_END).getNamespace () == ElektraNamespace::USER, "namespace wrong");

	succeed_if (Key ("dir:/", KEY_END).getNamespace () == ElektraNamespace::DIR, "namespace wrong");
	succeed_if (Key ("proc:/", KEY_END).getNamespace () == ElektraNamespace::PROC, "namespace wrong");
	succeed_if (Key ("spec:/", KEY_END).getNamespace () == ElektraNamespace::SPEC, "namespace wrong");
	succeed_if (Key ("system:/", KEY_END).getNamespace () == ElektraNamespace::SYSTEM, "namespace wrong");

	succeed_if (Key ("dir:/abc", KEY_END).getNamespace () == ElektraNamespace::DIR, "namespace wrong");
	succeed_if (Key ("proc:/abc", KEY_END).getNamespace () == ElektraNamespace::PROC, "namespace wrong");
	succeed_if (Key ("spec:/abc", KEY_END).getNamespace () == ElektraNamespace::SPEC, "namespace wrong");
	succeed_if (Key ("system:/abc", KEY_END).getNamespace () == ElektraNamespace::SYSTEM, "namespace wrong");

	succeed_if (Key ("/", KEY_END).getNamespace () == ElektraNamespace::CASCADING, "namespace wrong");
	succeed_if (Key ("/abc", KEY_END).getNamespace () == ElektraNamespace::CASCADING, "namespace wrong");
}

TEST (key, comparision)
{
	Key ke1, ke2;

	succeed_if (ke1 == ke2, "two empty keys are not the same?");
	succeed_if (!(ke1 != ke2), "two empty keys are not the same?");

	Key k1 ("user:/a", KEY_END), k2 ("user:/b", KEY_END);

	succeed_if (ke1 < k1, "compare empty key with user:/a");
	succeed_if (ke1 <= k1, "compare empty key with user:/a");
	succeed_if (!(ke1 > k1), "compare empty key with user:/a");
	succeed_if (!(ke1 >= k1), "compare empty key with user:/a");

	succeed_if (ke1 < k2, "compare empty key with user:/b");
	succeed_if (ke1 <= k2, "compare empty key with user:/b");
	succeed_if (!(ke1 > k2), "compare empty key with user:/b");
	succeed_if (!(ke1 >= k2), "compare empty key with user:/b");

	succeed_if (k1 < k2, "compare key user:/a with user:/b");
	succeed_if (k1 <= k2, "compare key user:/a with user:/b");
	succeed_if (!(k1 > k2), "compare key user:/a with user:/b");
	succeed_if (!(k1 >= k2), "compare key user:/a with user:/b");
	succeed_if (k1 != k2, "compare key user:/a with user:/b");
	succeed_if (!(k1 == k2), "compare key user:/a with user:/b");
}


struct C
{
	Key ks;
};

TEST (key, move)
{

	std::unique_ptr<Key> u1 (new Key ("user:/key3/1", KEY_END));
	std::unique_ptr<Key> u2 (std::move (u1));
	std::unique_ptr<Key> u3 = std::move (u1);

	std::unique_ptr<C> c1 (new C);
	std::unique_ptr<C> c2 (std::move (c1));
	std::unique_ptr<C> c3 = std::move (c1);
}

/* check the wrappers for underlying c-functions with a nullptr as key
 * exceptions are thrown if the underlying c-functions return error codes (-1 or NULL) */
TEST (key, cErrorsNull)
{
	Key x = nullptr;
	EXPECT_THROW (x.addName ("test"), KeyInvalidName);
	EXPECT_THROW (x.setName ("test"), KeyInvalidName);
	EXPECT_THROW (x.addBaseName ("test"), KeyInvalidName);
	EXPECT_THROW (x.setBaseName ("test"), KeyInvalidName);
	EXPECT_THROW (x.delBaseName (), KeyInvalidName);
	EXPECT_THROW (x.setMeta ("metaKey", "metaValue"), KeyException);
	EXPECT_THROW (x.delMeta ("metaKey"), KeyException);

	EXPECT_THROW (x.set (""), KeyException);
	EXPECT_THROW (x.copy (nullptr), KeyException);
	EXPECT_THROW (x.setCallback (nullptr), KeyException);
	EXPECT_THROW (x.setString (nullptr), KeyException);
	EXPECT_THROW (x.getString (), KeyException);
	EXPECT_THROW (x.getReferenceCounter (), KeyException);

	EXPECT_THROW (x--, KeyException);

	/* TODO: Currently not implemented, because otherwise a Key-object
	 * would not be settable to a nullptr which breaks some existing tests */
	/* EXPECT_THROW (x++, KeyException); */

	EXPECT_THROW (x.clear (), KeyException);
}

/* check the wrappers for underlying c-functions with a normal key
 * exceptions are thrown if the underlying c-functions return error codes (-1 or NULL) */
TEST (key, cErrorsParameters)
{
	Key k ("user:/key", KEY_VALUE, "testkey", KEY_END);
	KeySet ks;

	/* invalid name */
	EXPECT_THROW (k.addName ("\\"), KeyInvalidName);
	EXPECT_THROW (k.setName ("\\"), KeyInvalidName);

	EXPECT_NO_THROW (k.addBaseName ("\\"));
	EXPECT_NO_THROW (k.setBaseName ("\\"));
	EXPECT_NO_THROW (k.delBaseName ());
	EXPECT_NO_THROW (k.setMeta ("metaKey", "metaValue"));
	EXPECT_TRUE (k.hasMeta ("metaKey"));
	EXPECT_NO_THROW (k.delMeta ("metaKey"));
	EXPECT_FALSE (k.hasMeta ("metaKey"));

	EXPECT_EQ (k.getString (), "testkey");
	EXPECT_NO_THROW (k.set ("\\"));
	EXPECT_NO_THROW (k.copy (k));

	Key k1;
	EXPECT_NE (k, k1);
	EXPECT_NO_THROW (k.copy (k1));
	EXPECT_EQ (k, k1);

	EXPECT_NO_THROW (k.setCallback (nullptr));
	EXPECT_NO_THROW (k.setString ("\\"));
	EXPECT_EQ (k.getString (), "\\");
	EXPECT_EQ (k.getReferenceCounter (), 1);
	EXPECT_NO_THROW (k--);
	EXPECT_EQ (k.getReferenceCounter (), 0);
	EXPECT_NO_THROW (k.clear ());
}

/* tests for binary keys and functions operating on them or returning them */
TEST (key, binary)
{
	Key x = nullptr;
	Key b ("user:/keyBinary", KEY_VALUE, "", KEY_END);
	b.setBinary (nullptr, 0);

	EXPECT_THROW (x.getBinary (), KeyException);
	EXPECT_EQ (b.getString (), "");
}
