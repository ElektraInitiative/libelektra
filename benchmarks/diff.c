#include <benchmarks.h>
#include <elektra/core.h>
#include <elektra/diff.h>

int main (void)
{
	int runs = 100;
	int numKeys = 1000;

	KeySet * new = ksNew (0, KS_END);
	KeySet * old = ksNew (0, KS_END);

	char nameBuffer[100] = "";
	char valueBuffer[50] = "";

	for (int i = 0; i < numKeys; i++)
	{
		snprintf (nameBuffer, 99, "system:/a/%d", i * 2);
		snprintf (valueBuffer, 49, "%d", i);

		Key * k = keyNew (nameBuffer, KEY_VALUE, valueBuffer, KEY_END);

		ksAppendKey (new, k);
		ksAppendKey (old, k);
	}

	for (int i = 0; i < numKeys; i++)
	{
		snprintf (nameBuffer, 99, "system:/a/%d", i * 2 + 1);

		snprintf (valueBuffer, 49, "%d", i);
		ksAppendKey (new, keyNew (nameBuffer, KEY_VALUE, valueBuffer, KEY_END));

		snprintf (valueBuffer, 49, "%d modified", i);
		ksAppendKey (old, keyNew (nameBuffer, KEY_VALUE, valueBuffer, KEY_END));
	}

	for (int i = 0; i < numKeys; i++)
	{
		snprintf (nameBuffer, 99, "system:/b/%d", i * 2);
		snprintf (valueBuffer, 49, "%d", i);

		Key * k = keyNew (nameBuffer, KEY_VALUE, valueBuffer, KEY_END);

		ksAppendKey (new, k);
	}

	for (int i = 0; i < numKeys; i++)
	{
		snprintf (nameBuffer, 99, "system:/c/%d", i * 2);
		snprintf (valueBuffer, 49, "%d", i);

		Key * k = keyNew (nameBuffer, KEY_VALUE, valueBuffer, KEY_END);

		ksAppendKey (old, k);
	}

	int sum = 0;

	for (int i = 0; i < runs; i++)
	{
		timeInit ();

		ElektraDiff * diff = elektraDiffCalculate (new, old, NULL);

		int us = timeGetDiffMicroseconds ();
		sum += us;

		printf ("%d us\n", us);

		elektraDiffDel (diff);
	}

	printf ("Average: %.2f", (double) sum / runs);

	ksDel (new);
	ksDel (old);

	return 0;
}
