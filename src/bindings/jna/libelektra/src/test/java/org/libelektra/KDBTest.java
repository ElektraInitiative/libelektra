package org.libelektra;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;
import java.util.Optional;
import org.junit.Test;
import org.libelektra.exception.KDBClosedException;

public class KDBTest
{

	private static final String PARENT_KEY_NAME = "user:/sw/tests/jna/1/";
	private static final String KEY_1_NAME = "user:/sw/tests/jna/1/key_name";
	private static final String KEY_1_VALUE = "key_value_1";

	private static final String KEY_2_NAME = "user:/sw/tests/jna/1/key_name/2";
	private static final String KEY_2_VALUE = "key_value_2";

	@Test public void test_openClose_shouldPass () throws KDBException
	{
		KDB kdb = KDB.open ();
		kdb.close ();
	}

	@Test (expected = KDBClosedException.class) public void test_accessCloseSession_shouldFail () throws KDBException
	{
		KDB kdb = KDB.open ();
		kdb.close ();
		kdb.get (Key.create (PARENT_KEY_NAME));
	}

	/*
	 * This test is currently designed to work with the current behavior of kdbGet
	 * which returns all keys of the given namespace. As seen in
	 * https://github.com/ElektraInitiative/libelektra/issues/1258, this will
	 * probably change.
	 */
	@Test public void test_setGet_shouldPass () throws KDBException
	{
		var key = Key.create (KEY_1_NAME, KEY_1_VALUE);
		var key2 = Key.create (KEY_2_NAME, KEY_2_VALUE);
		var parentKey = Key.create (PARENT_KEY_NAME);

		try (KDB kdb = KDB.open ())
		{
			var keySet = kdb.get (parentKey);
			keySet.append (key).append (key2);
			kdb.set (keySet, parentKey);
		}

		// now retrieve them
		try (KDB kdb = KDB.open ())
		{
			Optional<Key> oFoundKey = kdb.get (parentKey).lookup (key2);

			assertTrue (oFoundKey.isPresent ());
			assertEquals (key2.toString (), oFoundKey.get ().toString ());

			oFoundKey.get ().release (); // optional clean-up
		}

		// remove them
		try (KDB kdb = KDB.open ())
		{
			KeySet keySet = kdb.get (parentKey);
			Iterator<Key> keyIter = keySet.iterator ();
			while (keyIter.hasNext ())
			{
				Key next = keyIter.next ();
				if (next.getName ().equals (key.getName ()) || next.getName ().equals (key2.getName ()))
				{
					keyIter.remove ();
				}
				next.release (); // optional clean-up
			}
			kdb.set (keySet, parentKey);
			keySet.release (); // optional clean-up
		}

		// optional clean-up
		key.release ();
		key2.release ();
		parentKey.release ();
	}
}
