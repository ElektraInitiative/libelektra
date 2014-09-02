#include <benchmarks.h>

KDB * kdb;
Key * key;
KeySet * large;

void benchmarkCreate()
{
	large = ksNew(NUM_KEY*NUM_DIR, KS_END);
}

void benchmarkFillup ()
{
	int i,j;
	char name [KEY_NAME_LENGTH + 1];
	char value [] = "data";

	for (i=0; i< NUM_DIR; i++)
	{
		snprintf (name, KEY_NAME_LENGTH, "%s/%s%d", KEY_ROOT, "dir", i);
		ksAppendKey(large, keyNew (name, KEY_VALUE, value, KEY_END));
		for (j=0; j<NUM_KEY; j++)
		{
			snprintf (name, KEY_NAME_LENGTH, "%s/%s%d/%s%d", KEY_ROOT, "dir", i, "key", j);
			ksAppendKey(large, keyNew (name, KEY_VALUE, value, KEY_END));
		}
	}
}

void benchmarkOpen()
{
	kdb = kdbOpen(key);
}

void benchmarkInread()
{
	KeySet *n = ksNew(0, KS_END);
	kdbGet(kdb, n, key);
	ksDel(n);
}

void benchmarkReadin()
{
	KeySet *n = ksNew(0, KS_END);
	kdbGet(kdb, n, key);
	ksDel(n);
}

void benchmarkReread()
{
	kdbGet(kdb, large, key);
}

void benchmarkInwrite()
{
	kdbSet(kdb, large, key);
}

void benchmarkRewrite()
{
	kdbSet(kdb, large, key);
}

void benchmarkWriteout()
{
	kdbSet(kdb, large, key);
}

void benchmarkClose()
{
	kdbClose(kdb, key);
}


int main()
{
	key = keyNew (KEY_ROOT, KEY_END);

	timeInit ();
	benchmarkCreate();
	timePrint ("Created empty keyset");

	benchmarkFillup();
	timePrint ("New large keyset");

	benchmarkOpen();
	keySetName(key, KEY_ROOT);
	timePrint ("Opened key database");

	benchmarkInread();
	timePrint ("Initialize read");
	
	benchmarkInwrite();
	timePrint ("Initialize write");

	benchmarkWriteout();
	timePrint ("Write key database");

	benchmarkRewrite();
	timePrint ("Rewrite key database");

	benchmarkReadin();
	timePrint ("Read in key database");

	benchmarkReread();
	timePrint ("Re read key database");

	benchmarkClose();
	timePrint ("Closed key database");

	ksDel (large);
	keyDel (key);
}

