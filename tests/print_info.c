#include <kdb.h>

#include <stdio.h>

void print_time (Key *key)
{
	printf ("atime: %lld\n", (long long int)keyGetATime(key));
	printf ("mtime: %lld\n", (long long int)keyGetMTime(key));
	printf ("ctime: %lld\n", (long long int)keyGetCTime(key));
}

void print_constant()
{
	printf ("\nFollowing constants must be defined in elektra\n");
	printf ("The Path Separator is %c\n", KDB_PATH_SEPARATOR);
	printf ("The default mode for new keys 0%o\n", KDB_FILE_MODE);
	printf ("The default mode added to directory keys 0%o\n", KEY_DEF_DIR);
}

void print_sizeof()
{
	printf ("\nFollowing types must be defined in elektra\n");
	printf ("Sizeof (int) is %zd\n", sizeof(int));
	printf ("Sizeof (uid_t) is %zd\n", sizeof(uid_t));
	printf ("Sizeof (gid_t) is %zd\n", sizeof(gid_t));
	printf ("Sizeof (size_t) is %zd\n", sizeof(size_t));
	printf ("Sizeof (ssize_t) is %zd\n", sizeof(ssize_t));
	printf ("Sizeof (time_t) is %zd\n", sizeof(time_t));

	printf ("Sizeof (type_t) is %zd\n", sizeof(type_t));
	printf ("Sizeof (keyswitch_t) is %zd\n", sizeof(keyswitch_t));
	printf ("Sizeof (option_t) is %zd\n", sizeof(option_t));

	printf ("Sizeof (cursor_t is %zd\n", sizeof(cursor_t));

	printf ("Sizeof (void *) is %zd\n", sizeof(void*));
	printf ("Sizeof (KDB *) is %zd\n", sizeof(KDB*));
	printf ("Sizeof (Key *) is %zd\n", sizeof(Key*));
	printf ("Sizeof (KeySet *) is %zd\n", sizeof(KeySet*));
}

void print_limits()
{
	printf ("\nFollowing limits must be defined in elektra\n");
	printf ("Maximum Integer is %d\n", INT_MAX);
	printf ("Minimum Integer is %d\n", INT_MIN);
	printf ("Maximum size_t is %d\n", SIZE_MAX);
	printf ("Maximum ssize_t is %d\n", SSIZE_MAX);
	printf ("Maximum length of a Keyname %d\n", MAX_KEY_LENGTH);
	printf ("Maximum length of a Path %d\n", KDB_MAX_PATH_LENGTH);
}

int main()
{
	print_constant();
	print_sizeof();
	print_limits();
}
