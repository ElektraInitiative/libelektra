/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests_internal.h>

#include "./kdbtools.h"
#include "./xmltool.h"

#include <unistd.h>

void test_readwrite (void)
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

void test_readwrite_hier (void)
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


void test_key (void)
{
	KeySet * ks;
	Key * cur;
	int counter;

	printf ("Testing Key from xml\n");

	ks = ksNew (0, KS_END);
	exit_if_fail (ksFromXMLfile (ks, srcdir_file ("xmltool/key.xml")) == 0, "ksFromXMLfile(key.xml) failed.");
	counter = 0;

	for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
	{
		cur = ksAtCursor (ks, it);
		counter++;
		/* Prepend key root */
		// snprintf(buf, sizeof(buf), "%s/%s", root, keyName(cur));
		// keySetName(cur, buf);

		/* Make tests ... */
		// printf ("counter: %d - name: %s - value: %s\n", counter, (char*) keyName(cur), (char*)keyValue(cur));
		switch (counter)
		{
		case 1:
			succeed_if (strcmp (keyName (cur), "user:/tests/filesys/.HiddenBinaryKey") == 0, "wrong name");
			succeed_if (strcmp (keyValue (cur), "BinaryValue") == 0, "value not correct");
			succeed_if (strcmp (keyComment (cur), "Binary key with hidden name") == 0, "comment not correct");
			succeed_if (keyIsBinary (cur) == 1, "key is not binary");
			break;
		case 2:
			succeed_if (strcmp (keyName (cur), "user:/tests/filesys/.HiddenDirectoryKey") == 0, "wrong name");
			succeed_if (strcmp (keyValue (cur), "DirectoryValue") == 0, "value not correct");
			succeed_if (strcmp (keyComment (cur), "Directory key with hidden name") == 0, "comment not correct");
			succeed_if (keyIsString (cur) == 1, "key is not string");
			break;
		case 3:
			succeed_if (strcmp (keyName (cur), "user:/tests/filesys/.HiddenStringKey") == 0, "wrong name");
			succeed_if (strcmp (keyValue (cur), "StringValue") == 0, "value not correct");
			succeed_if (strcmp (keyComment (cur), "String key with hidden name") == 0, "comment not correct");
			succeed_if (keyIsString (cur) == 1, "key is not string");
			break;
		case 4:
			succeed_if (strcmp (keyName (cur), "user:/tests/filesys/PerfectBinaryKey") == 0, "wrong name");
			succeed_if (strcmp (keyValue (cur), "BinaryValue") == 0, "value not correct");
			succeed_if (strcmp (keyComment (cur), "Binary key with standard name") == 0, "comment not correct");
			succeed_if (keyIsBinary (cur) == 1, "key is not binary");
			break;
		case 5:
			succeed_if (strcmp (keyName (cur), "user:/tests/filesys/PerfectDirectoryKey") == 0, "wrong name");
			succeed_if (strcmp (keyValue (cur), "DirectoryValue") == 0, "value not correct");
			succeed_if (strcmp (keyComment (cur), "Directory key with standard name") == 0, "comment not correct");
			succeed_if (keyIsString (cur) == 1, "key is not string");
			break;
		case 6:
			succeed_if (strcmp (keyName (cur), "user:/tests/filesys/PerfectStringKey") == 0, "wrong name");
			succeed_if (strcmp (keyValue (cur), "StringValue") == 0, "value not correct");
			succeed_if (strcmp (keyComment (cur), "String key with\nstandard name") == 0, "comment not correct");
			succeed_if (keyIsString (cur) == 1, "key is not string");
			break;
		case 7:
			succeed_if (strcmp (keyName (cur), "user:/tests/filesys/Ug.ly:Bin@a€ry Key") == 0, "wrong name");
			succeed_if (keyValue (cur) == 0, "value not correct");
			succeed_if (strcmp (keyComment (cur), "Binary key with ugly name") == 0, "comment not correct");
			succeed_if (keyIsBinary (cur) == 1, "key is not binary");
			// <key type="binary" basename="Ug.ly:Bin@a€ry Key"><comment>Binary key with ugly name</comment></key>
			break;
		case 8:
			succeed_if (strcmp (keyName (cur), "user:/tests/filesys/Ug.ly:Dir@ect€ory Key") == 0, "wrong name");
			succeed_if (strcmp (keyValue (cur), "") == 0, "value not correct");
			succeed_if (strcmp (keyComment (cur), "Directory with ugly name") == 0, "comment not correct");
			succeed_if (keyIsString (cur) == 1, "key is not string");
			// <key type="directory" basename="Ug.ly:Dir@ect€ory Key"><comment>Directory with ugly name</comment></key>
			break;
		case 9:
			succeed_if (strcmp (keyName (cur), "user:/tests/filesys/Ug.ly:St@ri€n.g Key") == 0, "wrong name");
			succeed_if (strcmp (keyValue (cur), "With a string value") == 0, "value not correct");
			succeed_if (strcmp (keyComment (cur), "string key with ugly name") == 0, "comment not correct");
			succeed_if (keyIsString (cur) == 1, "key is not string");
			// <key type="string" basename="Ug.ly:St@ri€n.g Key" value="With a string value"><comment>string key with ugly
			// name</comment></key>
			break;
		}
	}

	ksDel (ks);
}

void test_keyset (void)
{
	KeySet * ks;
	Key * cur;
	int counter;

	printf ("Testing KeySet from xml\n");

	ks = ksNew (0, KS_END);
	exit_if_fail (ksFromXMLfile (ks, srcdir_file ("xmltool/keyset.xml")) == 0, "ksFromXMLfile(key.xml) failed.");
	counter = 0;

	for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
	{
		cur = ksAtCursor (ks, it);
		counter++;

		/* Make tests ... */
		// printf ("counter: %d - %s\n", counter, keyName(cur));
		switch (counter)
		{
		// <key type="43" basename="0-27042916" value="0 216905227"><comment>2551516588474823843</comment></key>
		case 1:
			succeed_if (strcmp (keyName (cur), "user:/tests/filesys/0-27042916") == 0, "name of first key not correct");
			succeed_if (strcmp (keyValue (cur), "0 216905227") == 0, "value of first key not correct");
			succeed_if (strcmp (keyComment (cur), "2551516588474823843") == 0, "comment of first key not correct");
			break;
		// <key type="253" basename="1-2449524622" value="1 1679328197"><comment>3246436893195629244</comment></key>
		case 2:
			succeed_if (strcmp (keyName (cur), "user:/tests/filesys/1-2449524622") == 0, "name of 2. key not correct");
			succeed_if (strcmp (keyValue (cur), "1 1679328197") == 0, "value of 2. key not correct");
			succeed_if (strcmp (keyComment (cur), "3246436893195629244") == 0, "comment of 2. key not correct");
			break;
		// <key type="string" basename="dir-1-0">
		case 3:
			succeed_if (strcmp (keyName (cur), "user:/tests/filesys/dir-1-0") == 0, "name of 3. key not correct");
			break;
		// <key type="114" basename="0-294164813" value="0 216245011"><comment>18454108762891828026</comment></key>
		case 4:
			succeed_if (strcmp (keyName (cur), "user:/tests/filesys/dir-1-0/0-294164813") == 0, "name of 4. key not correct");
			succeed_if (strcmp (keyValue (cur), "0 216245011") == 0, "value of 4. key not correct");
			succeed_if (strcmp (keyComment (cur), "18454108762891828026") == 0, "comment of 4. key not correct");
			break;
		// <key type="135" basename="1-1479930365" value="1 2732423037"><comment>24597295372375238</comment></key>
		case 5:
			succeed_if (strcmp (keyName (cur), "user:/tests/filesys/dir-1-0/1-1479930365") == 0, "name of 4. key not correct");
			succeed_if (strcmp (keyValue (cur), "1 2732423037") == 0, "value of 4. key not correct");
			succeed_if (strcmp (keyComment (cur), "24597295372375238") == 0, "comment of 4. key not correct");
			break;
		// <key type="string" basename="dir-2-0">
		case 6:
			succeed_if (strcmp (keyName (cur), "user:/tests/filesys/dir-1-0/dir-2-0") == 0, "name of 3. key not correct");
			break;
		// <key type="144" basename="0-215571059" value="0 264857705"><comment>2188631490667217086</comment></key>
		case 7:
			succeed_if (strcmp (keyName (cur), "user:/tests/filesys/dir-1-0/dir-2-0/0-215571059") == 0,
				    "name of 4. key not correct");
			succeed_if (strcmp (keyValue (cur), "0 264857705") == 0, "value of 4. key not correct");
			succeed_if (strcmp (keyComment (cur), "2188631490667217086") == 0, "comment of 4. key not correct");
			break;
		}
	}

	ksDel (ks);
}

#define MAX_SIZE 200

static void test_ksCommonParentName (void)
{
	char ret[MAX_SIZE + 1];
	KeySet * ks = ksNew (10, keyNew ("system:/sw/xorg/Monitors/Monitor1/vrefresh", KEY_END),
			     keyNew ("system:/sw/xorg/Monitors/Monitor1/hrefresh", KEY_END),
			     keyNew ("system:/sw/xorg/Monitors/Monitor2/vrefresh", KEY_END),
			     keyNew ("system:/sw/xorg/Monitors/Monitor2/hrefresh", KEY_END),
			     keyNew ("system:/sw/xorg/Devices/Device1/driver", KEY_END),
			     keyNew ("system:/sw/xorg/Devices/Device1/mode", KEY_END), KS_END);

	printf ("Test common parentname\n");

	succeed_if (ksGetCommonParentName (ks, ret, MAX_SIZE) > 0, "could not find correct parentname");
	succeed_if_same_string (ret, "system:/sw/xorg");
	ksDel (ks);

	ks = ksNew (10, keyNew ("system:/", KEY_END), keyNew ("user:/", KEY_END), KS_END);
	succeed_if (ksGetCommonParentName (ks, ret, MAX_SIZE) == 0, "could find correct parentname");
	succeed_if_same_string (ret, "");
	ksDel (ks);

	ks = ksNew (10, keyNew ("system:/some/thing", KEY_END), keyNew ("system:/other/thing", KEY_END), KS_END);
	succeed_if (ksGetCommonParentName (ks, ret, MAX_SIZE) == 9, "could find correct parentname");
	succeed_if_same_string (ret, "system:/");
	ksDel (ks);

	ks = ksNew (10, keyNew ("system:/some/thing", KEY_END), keyNew ("system:/something", KEY_END), KS_END);
	succeed_if (ksGetCommonParentName (ks, ret, MAX_SIZE) == 9, "could find correct parentname");
	succeed_if_same_string (ret, "system:/");
	ksDel (ks);

	ks = ksNew (10, keyNew ("system:/here/in/deep/goes/ok/thing", KEY_END),
		    keyNew ("system:/here/in/deep/goes/ok/other/thing", KEY_END), KS_END);
	succeed_if (ksGetCommonParentName (ks, ret, MAX_SIZE) > 0, "could find correct parentname");
	succeed_if_same_string (ret, "system:/here/in/deep/goes/ok");
	ksDel (ks);

	ks = ksNew (10, keyNew ("system:/here/in/deep/goes/ok/thing", KEY_END),
		    keyNew ("system:/here/in/deep/goes/ok/other/thing", KEY_END), keyNew ("user:/unique/thing", KEY_END), KS_END);
	succeed_if (ksGetCommonParentName (ks, ret, MAX_SIZE) == 0, "could find correct parentname");
	succeed_if_same_string (ret, "");
	ksDel (ks);

	ks = ksNew (10, keyNew ("user:/unique/thing", KEY_END), KS_END);
	succeed_if (ksGetCommonParentName (ks, ret, MAX_SIZE) > 0, "could find correct parentname");
	succeed_if_same_string (ret, "user:/unique/thing");
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

	print_result ("test_xml");

	return nbError;
}
