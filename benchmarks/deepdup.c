/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <benchmarks.h>

Key * key;

int main (void)
{
	timeInit ();
	benchmarkCreate ();
	timePrint ("Created empty keyset");

	benchmarkFillup ();
	timePrint ("New large keyset");

	KeySet * ksTemp = ksNew (0, KS_END);

	for (elektraCursor i = 0; i < ksGetSize (large); i++)
	{
		ksAppendKey (ksTemp, keyDup (ksAtCursor (large, i), KEY_CP_ALL));
	}

	timePrint ("Deep duplication");

	ksDel (large);
	ksDel (ksTemp);
	keyDel (key);
}
