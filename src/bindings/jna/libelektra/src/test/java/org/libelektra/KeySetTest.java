package org.libelektra;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;
import java.util.Optional;
import org.junit.Before;
import org.junit.Test;
import org.libelektra.exception.KeySetReleasedException;

public class KeySetTest
{

	static final String KEY_1_NAME = "/key_test/1/key_name";
	static final String KEY_1_VALUE = "key_value_1";

	static final String KEY_2_NAME = "/key_test/2/key_name";
	static final String KEY_2_VALUE = "false";

	static final String KEY_3_NAME = "/key_test/3/key_name";
	static final String KEY_3_VALUE = "1";

	static final String KEY_4_NAME = "/key_test/4/key_name";
	static final String KEY_4_VALUE = "32123";

	static final String KEY_5_NAME = "/key_test/4/key_name/1";
	static final String KEY_5_VALUE = "214748365";

	static final String KEY_6_NAME = "/key_test/4/key_name/1/123";
	static final String KEY_6_VALUE = "121424748365";

	Key key, key2, key3, key4, key5, key6;

	@Before public void initializeSingleTest ()
	{
		key = Key.create (KEY_1_NAME, KEY_1_VALUE);
		key2 = Key.create (KEY_2_NAME, KEY_2_VALUE);
		key3 = Key.create (KEY_3_NAME, KEY_3_VALUE);
		key4 = Key.create (KEY_4_NAME, KEY_4_VALUE);
		key5 = Key.create (KEY_5_NAME, KEY_5_VALUE);
		key6 = Key.create (KEY_6_NAME, KEY_6_VALUE);
	}

	@Test public void test_keySetCreate_shouldPass ()
	{
		var ks = KeySet.create (6, key, key2, key3, key4, key5, key6);

		assertEquals (6, ks.size ());
	}

	@Test public void test_keySetCreateFromPointer_shouldPass ()
	{
		var ks = KeySet.create (6, key, key2, key3, key4, key5, key6);
		var ks2 = new KeySet (ks.getPointer ());

		assertEquals (ks.getPointer (), ks2.getPointer ());
		assertEquals (ks.size (), ks2.size ());
	}

	@Test public void test_keySetIterator_shouldPass ()
	{
		var ks = KeySet.create (6, key, key2, key3, key4, key5, key6);
		Iterator<Key> iterator = ks.iterator ();

		assertTrue (iterator.hasNext ());
		// note: compare key pointer instead of key object
		assertEquals (key.getPointer (), iterator.next ().getPointer ());
		assertTrue (iterator.hasNext ());
		assertEquals (key2.getPointer (), iterator.next ().getPointer ());
		assertTrue (iterator.hasNext ());
		assertEquals (key3.getPointer (), iterator.next ().getPointer ());
		assertTrue (iterator.hasNext ());
		assertEquals (key4.getPointer (), iterator.next ().getPointer ());
		assertTrue (iterator.hasNext ());
		assertEquals (key5.getPointer (), iterator.next ().getPointer ());
		assertTrue (iterator.hasNext ());
		assertEquals (key6.getPointer (), iterator.next ().getPointer ());
		assertFalse (iterator.hasNext ());
	}

	@Test public void test_keySetIteratorDelete_shouldPass ()
	{
		var ks = KeySet.create (6, key, key2, key3, key4, key5, key6);
		Iterator<Key> iterator = ks.iterator ();

		assertTrue (iterator.hasNext ());

		while (iterator.hasNext ())
		{
			var next = iterator.next ();
			if (key3.getPointer ().equals (next.getPointer ()))
			{
				iterator.remove ();
			}
		}

		assertFalse (iterator.hasNext ());
		assertEquals (5, ks.size ());
		assertTrue (ks.lookup (key3).isEmpty ());
		assertTrue (ks.lookup (key4).isPresent ());
		assertTrue (ks.lookup (key2).isPresent ());
	}

	@Test public void test_keySetToString_shouldPass ()
	{
		var ks = KeySet.create (6, key, key2, key3, key4, key5, key6);
		var expected_result = key.toString () + "\n" + key2.toString () + "\n" + key3.toString () + "\n" + key4.toString () + "\n" +
				      key5.toString () + "\n" + key6.toString ();

		assertEquals (expected_result, ks.toString ());
	}

	@Test public void test_keySetDup_shouldPass ()
	{
		var ks = KeySet.create (6, key, key2, key3, key4, key5, key6);
		var ks2 = ks.dup ();

		// note: compare pointers, because object will be cloned too
		assertEquals (ks.at (0).getPointer (), ks2.at (0).getPointer ());
		assertEquals (ks.at (3).getPointer (), ks2.at (3).getPointer ());
		assertEquals (ks.at (5).getPointer (), ks2.at (5).getPointer ());
	}

	@Test public void test_keySetCopy_shouldPass ()
	{
		var ks = KeySet.create (6, key, key2, key3, key4, key5, key6);
		var ks2 = KeySet.create (6).copy (ks);

		// note: compare pointers, because object will be cloned
		assertEquals (ks.at (0).getPointer (), ks2.at (0).getPointer ());
		assertEquals (ks.at (3).getPointer (), ks2.at (3).getPointer ());
		assertEquals (ks.at (5).getPointer (), ks2.at (5).getPointer ());
	}

	@Test public void test_keySetLength_shouldPass ()
	{
		var ks = KeySet.create (10);

		assertEquals (0, ks.size ());

		ks.append (key);

		assertEquals (1, ks.size ());

		ks.append (key2).append (key3).append (key4);

		assertEquals (4, ks.size ());

		ks.append (key5).append (key6);

		assertEquals (6, ks.size ());
	}

	@Test public void test_keySetAppend_shouldPass ()
	{
		var ks = KeySet.create (10);

		assertEquals (key.getName (), ks.append (key).at (0).getName ());
		assertEquals (key2.getName (), ks.append (key2).at (1).getName ());
		assertEquals (key3.getName (), ks.append (key3).at (2).getName ());
	}

	@Test public void test_keySetAppendKeySet_shouldPass ()
	{
		var ks = KeySet.create (10);
		var ks2 = KeySet.create (3, key, key2, key3);
		var ks3 = KeySet.create (3, key4, key5, key6);

		assertEquals (3, ks.append (ks2).size ());
		assertEquals (ks.at (0).getName (), ks2.at (0).getName ());
		assertEquals (ks.at (0).getString (), ks2.at (0).getString ());
		assertEquals (6, ks.append (ks3).size ());
	}

	@Test public void test_keySetCut_shouldPass ()
	{
		var ks = KeySet.create (6, key, key2, key3, key4, key5, key6);
		var ks2 = ks.cut (key4);

		assertEquals (3, ks2.size ());
		assertTrue (ks2.lookup (key4).isPresent ());
		assertTrue (ks2.lookup (key5).isPresent ());
		assertTrue (ks2.lookup (key6).isPresent ());
	}

	@Test public void test_keySetRemove_shouldPass ()
	{
		var ks = KeySet.create (6, key, key2, key3, key4, key5, key6);

		assertEquals (6, ks.size ());
		assertEquals (key6.getPointer (), ks.remove (5).getPointer ());
		assertEquals (5, ks.size ());
		assertEquals (key5.getPointer (), ks.remove (4).getPointer ());
		assertEquals (4, ks.size ());

		ks.remove (3);
		ks.remove (2);
		ks.remove (1);

		assertEquals (1, ks.size ());
		assertEquals (key.getPointer (), ks.remove (0).getPointer ());
		assertEquals (0, ks.size ());
	}

	@Test public void test_keySetRemoveKey_shouldPass ()
	{
		var ks = KeySet.create (2, key, key5);

		assertEquals (2, ks.size ());
		assertTrue (ks.lookup (key).isPresent ());
		assertTrue (ks.remove (key));
		assertEquals (1, ks.size ());
		assertTrue (ks.lookup (key).isEmpty ());

		assertFalse (ks.remove (key6));
	}

	@Test public void test_keySetHeadTail_shouldPass ()
	{
		var ks = KeySet.create (6, key, key2, key3, key4, key5, key6);

		assertEquals (key.getPointer (), ks.first ().getPointer ());
		assertEquals (key6.getPointer (), ks.last ().getPointer ());
	}

	@Test public void test_keySetLookup_shouldPass ()
	{
		Optional<Key> oFoundKey = KeySet.create (6, key, key2, key3, key4, key5, key6).lookup (key);

		assertTrue (oFoundKey.isPresent ());
		assertEquals (oFoundKey.get ().getPointer (), key.getPointer ());
	}

	@Test public void test_keySetLookupByName_shouldPass ()
	{
		Optional<Key> oFoundKey = KeySet.create (6, key, key2, key3, key4, key5, key6).lookup (key.getName ());

		assertTrue (oFoundKey.isPresent ());
		assertEquals (oFoundKey.get ().getPointer (), key.getPointer ());
	}

	@Test (expected = KeySetReleasedException.class) public void test_accessingKeySetAfterRelease_shouldThrow ()
	{
		var keySet = KeySet.create ();
		keySet.release ();

		keySet.getPointer ();
	}

	@Test public void test_keySetLookupMissShouldReturnNull_shouldPass ()
	{
		Optional<Key> oFoundKey = KeySet.create (5, key2, key3, key4, key5, key6).lookup (key);

		assertTrue (oFoundKey.isEmpty ());
	}

	@Test public void test_keySetLookupByNameMissShouldReturnNull_shouldPass ()
	{
		Optional<Key> oFoundKey = KeySet.create (5, key2, key3, key4, key5, key6).lookup ("NOT_IN_KEY_SET");

		assertTrue (oFoundKey.isEmpty ());
	}
}
