/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <tests_internal.h>

#include "kdbtools.h"
#include "xmltool.h"

#include <unistd.h>

void test_readwrite ()
{
	KeySet * ks;
	FILE * fout;

	printf ("Testing Read and write xml\n");

	fout = fopen (srcdir_file ("xmltool/key-gen.xml"), "w");
	ks = ksNew (0, KS_END);
	exit_if_fail (ksFromXMLfile (ks, srcdir_file ("xmltool/key.xml")) == 0, "ksFromXMLfile(key.xml) failed.");
	ksToStream (ks, fout, KDB_O_HEADER);
	fclose (fout);

	compare_files (srcdir_file ("xmltool/key.xml"));
	unlink (srcdir_file ("xmltool/key-gen.xml"));

	ksDel (ks);
}

void test_readwrite_hier ()
{
	KeySet * ks;
	FILE * fout;

	printf ("Testing Read and write xml\n");

	fout = fopen (srcdir_file ("xmltool/key-gen.xml"), "w");
	ks = ksNew (0, KS_END);
	exit_if_fail (ksFromXMLfile (ks, srcdir_file ("xmltool/key.xml")) == 0, "ksFromXMLfile(key.xml) failed.");
	ksToStream (ks, fout, KDB_O_HIER | KDB_O_HEADER);
	fclose (fout);

	compare_files (srcdir_file ("xmltool/key.xml"));
	unlink (srcdir_file ("xmltool/key-gen.xml"));

	ksDel (ks);
}


void test_key ()
{
	KeySet * ks;
	Key * cur;
	int counter;

	printf ("Testing Key from xml\n");

	ks = ksNew (0, KS_END);
	exit_if_fail (ksFromXMLfile (ks, srcdir_file ("xmltool/key.xml")) == 0, "ksFromXMLfile(key.xml) failed.");
	counter = 0;
	ksRewind (ks);
	while ((cur = ksNext (ks)))
	{
		counter++;
		/* Prepend key root */
		// snprintf(buf, sizeof(buf), "%s/%s", root, keyName(cur));
		// keySetName(cur, buf);

		/* Make tests ... */
		// printf ("counter: %d - name: %s - value: %s\n", counter, (char*) keyName(cur), (char*)keyValue(cur));
		switch (counter)
		{
		case 1:
			succeed_if (strcmp (keyName (cur), "user/tests/filesys/.HiddenBinaryKey") == 0, "wrong name");
			succeed_if (strcmp (keyValue (cur), "BinaryValue") == 0, "value not correct");
			succeed_if (strcmp (keyComment (cur), "Binary key with hidden name") == 0, "comment not correct");
			succeed_if (keyIsInactive (cur) == 1, "key is inactive");
			succeed_if (keyIsBinary (cur) == 1, "key is not binary");
			succeed_if (keyGetMode (cur) == 0440, "could not get mode");
			break;
		case 2:
			succeed_if (strcmp (keyName (cur), "user/tests/filesys/.HiddenDirectoryKey") == 0, "wrong name");
			succeed_if (strcmp (keyValue (cur), "DirectoryValue") == 0, "value not correct");
			succeed_if (strcmp (keyComment (cur), "Directory key with hidden name") == 0, "comment not correct");
			succeed_if (keyIsInactive (cur) == 1, "key is inactive");
			succeed_if (keyIsString (cur) == 1, "key is not string");
			break;
		case 3:
			succeed_if (strcmp (keyName (cur), "user/tests/filesys/.HiddenStringKey") == 0, "wrong name");
			succeed_if (strcmp (keyValue (cur), "StringValue") == 0, "value not correct");
			succeed_if (strcmp (keyComment (cur), "String key with hidden name") == 0, "comment not correct");
			succeed_if (keyIsInactive (cur) == 1, "key is inactive");
			succeed_if (keyIsString (cur) == 1, "key is not string");
			succeed_if (keyGetUID (cur) == 0, "could not get uid value");
			succeed_if (keyGetGID (cur) == 20, "could not get gid value");
			break;
		case 4:
			succeed_if (strcmp (keyName (cur), "user/tests/filesys/PerfectBinaryKey") == 0, "wrong name");
			succeed_if (strcmp (keyValue (cur), "BinaryValue") == 0, "value not correct");
			succeed_if (strcmp (keyComment (cur), "Binary key with standard name") == 0, "comment not correct");
			succeed_if (keyIsInactive (cur) == 0, "key is active");
			succeed_if (keyIsBinary (cur) == 1, "key is not binary");
			succeed_if (keyGetGID (cur) == 40, "could not get gid value");
			break;
		case 5:
			succeed_if (strcmp (keyName (cur), "user/tests/filesys/PerfectDirectoryKey") == 0, "wrong name");
			succeed_if (strcmp (keyValue (cur), "DirectoryValue") == 0, "value not correct");
			succeed_if (strcmp (keyComment (cur), "Directory key with standard name") == 0, "comment not correct");
			succeed_if (keyIsInactive (cur) == 0, "key is active");
			succeed_if (keyIsString (cur) == 1, "key is not string");
			break;
		case 6:
			succeed_if (strcmp (keyName (cur), "user/tests/filesys/PerfectStringKey") == 0, "wrong name");
			succeed_if (strcmp (keyValue (cur), "StringValue") == 0, "value not correct");
			succeed_if (strcmp (keyComment (cur), "String key with\nstandard name") == 0, "comment not correct");
			succeed_if (keyIsInactive (cur) == 0, "key is active");
			succeed_if (keyIsString (cur) == 1, "key is not string");
			succeed_if (keyGetUID (cur) == 20, "could not get uid value");
			break;
		case 7:
			succeed_if (strcmp (keyName (cur), "user/tests/filesys/Ug.ly:Bin@a€ry Key") == 0, "wrong name");
			succeed_if (keyValue (cur) == 0, "value not correct");
			succeed_if (strcmp (keyComment (cur), "Binary key with ugly name") == 0, "comment not correct");
			succeed_if (keyIsInactive (cur) == 0, "key is active");
			succeed_if (keyIsBinary (cur) == 1, "key is not binary");
			succeed_if (keyGetMode (cur) == 230, "could not get mode");
			// <key type="binary" basename="Ug.ly:Bin@a€ry Key"><comment>Binary key with ugly name</comment></key>
			break;
		case 8:
			succeed_if (strcmp (keyName (cur), "user/tests/filesys/Ug.ly:Dir@ect€ory Key") == 0, "wrong name");
			succeed_if (strcmp (keyValue (cur), "") == 0, "value not correct");
			succeed_if (strcmp (keyComment (cur), "Directory with ugly name") == 0, "comment not correct");
			succeed_if (keyIsInactive (cur) == 0, "key is active");
			succeed_if (keyIsString (cur) == 1, "key is not string");
			// <key type="directory" basename="Ug.ly:Dir@ect€ory Key"><comment>Directory with ugly name</comment></key>
			break;
		case 9:
			succeed_if (strcmp (keyName (cur), "user/tests/filesys/Ug.ly:St@ri€n.g Key") == 0, "wrong name");
			succeed_if (strcmp (keyValue (cur), "With a string value") == 0, "value not correct");
			succeed_if (strcmp (keyComment (cur), "string key with ugly name") == 0, "comment not correct");
			succeed_if (keyIsInactive (cur) == 0, "key is active");
			succeed_if (keyIsString (cur) == 1, "key is not string");
			// <key type="string" basename="Ug.ly:St@ri€n.g Key" value="With a string value"><comment>string key with ugly
			// name</comment></key>
			break;
		}
	}

	ksDel (ks);
}

void test_keyset ()
{
	KeySet * ks;
	Key * cur;
	int counter;

	printf ("Testing KeySet from xml\n");

	ks = ksNew (0, KS_END);
	exit_if_fail (ksFromXMLfile (ks, srcdir_file ("xmltool/keyset.xml")) == 0, "ksFromXMLfile(key.xml) failed.");
	counter = 0;
	ksRewind (ks);
	while ((cur = ksNext (ks)))
	{
		counter++;

		/* Make tests ... */
		// printf ("counter: %d - %s\n", counter, keyName(cur));
		switch (counter)
		{
		// <key type="43" basename="0-27042916" value="0 216905227"><comment>2551516588474823843</comment></key>
		case 1:
			succeed_if (strcmp (keyName (cur), "user/tests/filesys/0-27042916") == 0, "name of first key not correct");
			succeed_if (strcmp (keyValue (cur), "0 216905227") == 0, "value of first key not correct");
			succeed_if (strcmp (keyComment (cur), "2551516588474823843") == 0, "comment of first key not correct");
			break;
		// <key type="253" basename="1-2449524622" value="1 1679328197"><comment>3246436893195629244</comment></key>
		case 2:
			succeed_if (strcmp (keyName (cur), "user/tests/filesys/1-2449524622") == 0, "name of 2. key not correct");
			succeed_if (strcmp (keyValue (cur), "1 1679328197") == 0, "value of 2. key not correct");
			succeed_if (strcmp (keyComment (cur), "3246436893195629244") == 0, "comment of 2. key not correct");
			break;
		// <key type="string" basename="dir-1-0">
		case 3:
			succeed_if (strcmp (keyName (cur), "user/tests/filesys/dir-1-0") == 0, "name of 3. key not correct");
			break;
		// <key type="114" basename="0-294164813" value="0 216245011"><comment>18454108762891828026</comment></key>
		case 4:
			succeed_if (strcmp (keyName (cur), "user/tests/filesys/dir-1-0/0-294164813") == 0, "name of 4. key not correct");
			succeed_if (strcmp (keyValue (cur), "0 216245011") == 0, "value of 4. key not correct");
			succeed_if (strcmp (keyComment (cur), "18454108762891828026") == 0, "comment of 4. key not correct");
			break;
		// <key type="135" basename="1-1479930365" value="1 2732423037"><comment>24597295372375238</comment></key>
		case 5:
			succeed_if (strcmp (keyName (cur), "user/tests/filesys/dir-1-0/1-1479930365") == 0, "name of 4. key not correct");
			succeed_if (strcmp (keyValue (cur), "1 2732423037") == 0, "value of 4. key not correct");
			succeed_if (strcmp (keyComment (cur), "24597295372375238") == 0, "comment of 4. key not correct");
			break;
		// <key type="string" basename="dir-2-0">
		case 6:
			succeed_if (strcmp (keyName (cur), "user/tests/filesys/dir-1-0/dir-2-0") == 0, "name of 3. key not correct");
			break;
		// <key type="144" basename="0-215571059" value="0 264857705"><comment>2188631490667217086</comment></key>
		case 7:
			succeed_if (strcmp (keyName (cur), "user/tests/filesys/dir-1-0/dir-2-0/0-215571059") == 0,
				    "name of 4. key not correct");
			succeed_if (strcmp (keyValue (cur), "0 264857705") == 0, "value of 4. key not correct");
			succeed_if (strcmp (keyComment (cur), "2188631490667217086") == 0, "comment of 4. key not correct");
			break;
		}
	}

	ksDel (ks);
}

#define MAX_SIZE 200

static void test_ksCommonParentName ()
{
	char ret[MAX_SIZE + 1];
	KeySet * ks =
		ksNew (10, keyNew ("system/sw/xorg/Monitors/Monitor1/vrefresh", 0), keyNew ("system/sw/xorg/Monitors/Monitor1/hrefresh", 0),
		       keyNew ("system/sw/xorg/Monitors/Monitor2/vrefresh", 0), keyNew ("system/sw/xorg/Monitors/Monitor2/hrefresh", 0),
		       keyNew ("system/sw/xorg/Devices/Device1/driver", 0), keyNew ("system/sw/xorg/Devices/Device1/mode", 0), KS_END);

	printf ("Test common parentname\n");

	succeed_if (ksGetCommonParentName (ks, ret, MAX_SIZE) > 0, "could not find correct parentname");
	succeed_if_same_string (ret, "system/sw/xorg");
	ksDel (ks);

	ks = ksNew (10, keyNew ("system", 0), keyNew ("user", 0), KS_END);
	succeed_if (ksGetCommonParentName (ks, ret, MAX_SIZE) == 0, "could find correct parentname");
	succeed_if_same_string (ret, "");
	ksDel (ks);

	ks = ksNew (10, keyNew ("system/some/thing", 0), keyNew ("system/other/thing", 0), KS_END);
	succeed_if (ksGetCommonParentName (ks, ret, MAX_SIZE) == 7, "could find correct parentname");
	succeed_if_same_string (ret, "system");
	ksDel (ks);

	ks = ksNew (10, keyNew ("system/here/in/deep/goes/ok/thing", 0), keyNew ("system/here/in/deep/goes/ok/other/thing", 0), KS_END);
	succeed_if (ksGetCommonParentName (ks, ret, MAX_SIZE) > 0, "could find correct parentname");
	succeed_if_same_string (ret, "system/here/in/deep/goes/ok");
	ksDel (ks);

	ks = ksNew (10, keyNew ("system/here/in/deep/goes/ok/thing", 0), keyNew ("system/here/in/deep/goes/ok/other/thing", 0),
		    keyNew ("user/unique/thing", 0), KS_END);
	succeed_if (ksGetCommonParentName (ks, ret, MAX_SIZE) == 0, "could find correct parentname");
	succeed_if_same_string (ret, "");
	ksDel (ks);

	ks = ksNew (10, keyNew ("user/unique/thing", 0), KS_END);
	succeed_if (ksGetCommonParentName (ks, ret, MAX_SIZE) > 0, "could find correct parentname");
	succeed_if_same_string (ret, "user/unique/thing");
	ksDel (ks);
}


int main (int argc, char ** argv)
{
	printf ("ELEKTRA PLUGIN TEST SUITE\n");
	printf ("========================================\n\n");

	init (argc, argv);

	test_key ();
	test_keyset ();
	test_ksCommonParentName ();

	/*
	test_readwrite();
	test_readwrite_hier();
	*/

	printf ("\ntest_xml RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
