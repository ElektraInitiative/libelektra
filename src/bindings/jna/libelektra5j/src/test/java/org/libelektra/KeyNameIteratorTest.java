package org.libelektra;

import static org.junit.Assert.*;

import java.util.Iterator;
import org.junit.Test;

public class KeyNameIteratorTest
{

	static final String KEY_1_NAME = "/key_test/1/key_name";
	static final String KEY_1_NAME_PART_1 = "\u0001";
	static final String KEY_1_NAME_PART_2 = "key_test";
	static final String KEY_1_NAME_PART_3 = "1";
	static final String KEY_1_NAME_PART_4 = "key_name";
	static final String KEY_1_VALUE = "key_value_1";

	@Test public void test_keyNameIteratorHasNext_shouldPass ()
	{
		final Key key = Key.create (KEY_1_NAME, KEY_1_VALUE);
		final Iterator<String> iterator = key.iterator ();
		assertTrue (iterator.hasNext ());
		assertEquals (KEY_1_NAME_PART_1, iterator.next ());
		assertTrue (iterator.hasNext ());
		assertEquals (KEY_1_NAME_PART_2, iterator.next ());
		assertTrue (iterator.hasNext ());
		assertEquals (KEY_1_NAME_PART_3, iterator.next ());
		assertTrue (iterator.hasNext ());
		assertEquals (KEY_1_NAME_PART_4, iterator.next ());
		assertFalse (iterator.hasNext ());
	}

	@Test (expected = UnsupportedOperationException.class) public void test_keyNameIteratorDelete_shouldPass ()
	{
		final Key key = Key.create (KEY_1_NAME, KEY_1_VALUE);
		final Iterator<String> iterator = key.iterator ();
		assertTrue (iterator.hasNext ());
		iterator.remove ();
	}
}
