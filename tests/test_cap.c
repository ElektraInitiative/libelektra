#include <tests.h>

void test_system()
{
	Key * key = keyNew ("system", KEY_END);
	KDB * handle = kdbOpen();
	KDBCap *cap=0;

	printf ("Test system capability\n");
	cap=kdbGetCapability(handle, key);

	exit_if_fail (cap != 0, "could not get capabilities");
	succeed_if (strcmp (kdbcGetName(cap), "filesys") == 0, "You are not using filesys for system keys");
	succeed_if (strcmp (kdbcGetVersion(cap), "0.2.1") == 0, "wrong version");
	succeed_if (strcmp (kdbcGetLicence(cap), "BSD") == 0, "wrong licence");
	succeed_if (kdbcGetonlyFullGet(cap) == 0, "default backend must support single gets");
	succeed_if (kdbcGetonlyAddKeys(cap) == 0, "default backend must support adding keys");
	succeed_if (kdbcGetnoComment(cap) == 0, "default backend must support comments");
	succeed_if (kdbcGetnoValue(cap) == 0, "default backend must *really* support values");
	warn_if_fail (kdbcGetnoError(cap) == 0, "What a pity default backend does not support proper error codes");

	kdbClose (handle);
	keyDel (key);
}

void test_user()
{
	Key * key = keyNew ("user", KEY_END);
	KDB * handle = kdbOpen();
	KDBCap *cap=0;

	printf ("Test user capability\n");
	cap=kdbGetCapability(handle, key);

	exit_if_fail (cap != 0, "could not get capabilities");
	warn_if_fail (strcmp (kdbcGetName(cap), "filesys") == 0, "You are not using filesys for user keys");
	warn_if_fail (strcmp (kdbcGetLicence(cap), "BSD") == 0, "Your user keys are not stored in a bsd licenced backend");
	succeed_if (kdbcGetonlyFullGet(cap) == 0, "user backend must support single gets");
	succeed_if (kdbcGetonlyAddKeys(cap) == 0, "user backend must support adding keys");
	succeed_if (kdbcGetnoComment(cap) == 0, "user backend must support comments");
	succeed_if (kdbcGetnoValue(cap) == 0, "user backend must *really* support values");
	warn_if_fail (kdbcGetnoError(cap) == 0, "What a pity user backend does not support proper error codes");

	kdbClose (handle);
	keyDel (key);
}

void test_fstab()
{
	Key * key = keyNew ("system/tests/filesystems", KEY_VALUE, "fstab", KEY_END);
	KDB * handle = kdbOpen();
	KDBCap *cap = 0;
	KeySet * conf;

	printf ("Test fstab capability\n");
	if (kdbMount (handle, key,
		conf=ksNew (2,keyNew("system/path", KEY_VALUE, "/tmp/fstab", KEY_END), KS_END)) == -1)
	{
		ksDel (conf);
		keyDel (key);
		kdbClose (handle);
		printf ("Could not mount backend %s\n", "fstab");
		printf ("Will not continue with tests on that backend\n");
		return;
	}
	ksDel (conf);
	cap = kdbGetCapability(handle, key);

	exit_if_fail (cap != 0, "could not get capabilities");
	succeed_if (strcmp (kdbcGetName(cap), "fstab") == 0, "Not fstab in mounted path");
	succeed_if (strcmp (kdbcGetVersion(cap), "0.0.1") == 0, "wrong version");
	succeed_if (strcmp (kdbcGetLicence(cap), "BSD") == 0, "wrong licence");
	succeed_if (kdbcGetonlyFullGet(cap) == 1, "fstab only full get");
	succeed_if (kdbcGetonlyAddKeys(cap) == 1, "fstab only add keys");
	succeed_if (kdbcGetnoComment(cap) == 1, "fstab does not support comments");
	succeed_if (kdbcGetnoError(cap) == 1, "fstab does not return correct error values");

	kdbClose (handle);
	keyDel (key);
}

void test_hosts()
{
	Key * key = keyNew ("system/tests/hosts", KEY_VALUE, "hosts", KEY_END);
	KDB * handle = kdbOpen();
	KDBCap *cap = 0;
	KeySet * conf;

	printf ("Test hosts capability\n");
	if (kdbMount (handle, key,
		conf=ksNew (2,keyNew("system/path", KEY_VALUE, "/tmp/hosts", KEY_END), KS_END)) == -1)
	{
		ksDel (conf);
		keyDel (key);
		kdbClose (handle);
		printf ("Could not mount backend %s\n", "hosts");
		printf ("Will not continue with tests on that backend\n");
		return;
	}
	ksDel (conf);
	cap = kdbGetCapability(handle, key);

	exit_if_fail (cap != 0, "could not get capabilities");
	succeed_if (strcmp (kdbcGetName(cap), "hosts") == 0, "Not hosts in mounted path");
	succeed_if (strcmp (kdbcGetVersion(cap), "0.0.2") == 0, "wrong version");
	succeed_if (strcmp (kdbcGetLicence(cap), "BSD") == 0, "wrong licence");
	succeed_if (kdbcGetonlyFullGet(cap) == 1, "hosts only full get");
	succeed_if (kdbcGetonlyAddKeys(cap) == 1, "hosts only add keys");
	succeed_if (kdbcGetnoComment(cap) == 1, "hosts does not support comments");
	succeed_if (kdbcGetnoError(cap) == 1, "hosts does not return correct error values");

	kdbClose (handle);
	keyDel (key);
}

int main(int argc, char** argv)
{
	printf("ELEKTRA CAPABILTIY TEST SUITE\n");
	printf("========================================\n\n");

	init (argc, argv);

	test_system();
	test_user();

	test_fstab();
	test_hosts();

	printf("\ntest_cap RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);
	return nbError;
}
