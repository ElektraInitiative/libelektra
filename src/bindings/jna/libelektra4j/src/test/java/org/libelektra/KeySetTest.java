package org.libelektra;

import static org.junit.Assert.*;

import java.util.Iterator;

import org.junit.Before;
import org.junit.Test;

public class KeySetTest {

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

	@Before
	public void initializeSingleTest() {
		key = Key.create(KEY_1_NAME, Key.KEY_VALUE, KEY_1_VALUE, Key.KEY_END);
		key2 = Key.create(KEY_2_NAME, Key.KEY_VALUE, KEY_2_VALUE, Key.KEY_END);
		key3 = Key.create(KEY_3_NAME, Key.KEY_VALUE, KEY_3_VALUE, Key.KEY_END);
		key4 = Key.create(KEY_4_NAME, Key.KEY_VALUE, KEY_4_VALUE, Key.KEY_END);
		key5 = Key.create(KEY_5_NAME, Key.KEY_VALUE, KEY_5_VALUE, Key.KEY_END);
		key6 = Key.create(KEY_6_NAME, Key.KEY_VALUE, KEY_6_VALUE, Key.KEY_END);
	}

	@Test
	public void test_keySetCreate_shouldPass() {
		final KeySet ks = KeySet.create(6, key, key2, key3, key4, key5, key6, KeySet.KS_END);
		assertEquals(6, ks.length());
	}

	@Test
	public void test_keySetCreateFromPointer_shouldPass() {
		final KeySet ks = KeySet.create(6, key, key2, key3, key4, key5, key6, KeySet.KS_END);
		final KeySet ks2 = new KeySet(ks.get());
		assertEquals(ks.get(), ks2.get());
		assertEquals(ks.length(), ks2.length());
	}

	@Test
	public void test_keySetIterator_shouldPass() {
		final KeySet ks = KeySet.create(6, key, key2, key3, key4, key5, key6, KeySet.KS_END);
		final Iterator<Key> iterator = ks.iterator();
		assertTrue(iterator.hasNext());
		// note: compare key pointer instead of key object
		assertEquals(key.get(), iterator.next().get());
		assertTrue(iterator.hasNext());
		assertEquals(key2.get(), iterator.next().get());
		assertTrue(iterator.hasNext());
		assertEquals(key3.get(), iterator.next().get());
		assertTrue(iterator.hasNext());
		assertEquals(key4.get(), iterator.next().get());
		assertTrue(iterator.hasNext());
		assertEquals(key5.get(), iterator.next().get());
		assertTrue(iterator.hasNext());
		assertEquals(key6.get(), iterator.next().get());
		assertFalse(iterator.hasNext());
	}

	@Test
	public void test_keySetToString_shouldPass() {
		final KeySet ks = KeySet.create(6, key, key2, key3, key4, key5, key6, KeySet.KS_END);
		final String expected_result = key.toString() + "\n" + key2.toString() + "\n" + key3.toString() + "\n" + key4.toString() + "\n"
				+ key5.toString() + "\n" + key6.toString();
		assertEquals(expected_result, ks.toString());
	}

	@Test
	public void test_keySetDup_shouldPass() {
		final KeySet ks = KeySet.create(6, key, key2, key3, key4, key5, key6, KeySet.KS_END);
		final KeySet ks2 = ks.dup();
		// note: compare pointers, because object will be cloned too
		assertEquals(ks.at(0).get(), ks2.at(0).get());
		assertEquals(ks.at(3).get(), ks2.at(3).get());
		assertEquals(ks.at(5).get(), ks2.at(5).get());
	}

	@Test
	public void test_keySetCopy_shouldPass() {
		final KeySet ks = KeySet.create(6, key, key2, key3, key4, key5, key6, KeySet.KS_END);
		final KeySet ks2 = KeySet.create(6, KeySet.KS_END);
		ks2.copy(ks);
		// note: compare pointers, because object will be cloned
		assertEquals(ks.at(0).get(), ks2.at(0).get());
		assertEquals(ks.at(3).get(), ks2.at(3).get());
		assertEquals(ks.at(5).get(), ks2.at(5).get());
	}

	@Test
	public void test_keySetLength_shouldPass() {
		final KeySet ks = KeySet.create(10, KeySet.KS_END);
		assertEquals(0, ks.length());
		ks.append(key);
		assertEquals(1, ks.length());
		ks.append(key2);
		ks.append(key3);
		ks.append(key4);
		assertEquals(4, ks.length());
		ks.append(key5);
		ks.append(key6);
		assertEquals(6, ks.length());
	}

	@Test
	public void test_keySetAppend_shouldPass() {
		final KeySet ks = KeySet.create(10, KeySet.KS_END);
		assertEquals(1, ks.append(key));
		assertEquals(2, ks.append(key2));
		assertEquals(3, ks.append(key3));
	}

	@Test
	public void test_keySetAppendKeySet_shouldPass() {
		final KeySet ks = KeySet.create(10, KeySet.KS_END);
		final KeySet ks2 = KeySet.create(3, key, key2, key3, KeySet.KS_END);
		final KeySet ks3 = KeySet.create(3, key4, key5, key6, KeySet.KS_END);
		assertEquals(3, ks.append(ks2));
		assertEquals(ks.at(0).getName(), ks2.at(0).getName());
		assertEquals(ks.at(0).getString(), ks2.at(0).getString());
		assertEquals(6, ks.append(ks3));
	}

	@Test
	public void test_keySetCut_shouldPass() {
		final KeySet ks = KeySet.create(6, key, key2, key3, key4, key5, key6, KeySet.KS_END);
		final KeySet ks2 = ks.cut(key4);
		assertEquals(3, ks2.length());
		assertNotNull(ks2.lookup(key4));
		assertNotNull(ks2.lookup(key5));
		assertNotNull(ks2.lookup(key6));
	}

	@Test
	public void test_keySetPop_shouldPass() {
		final KeySet ks = KeySet.create(6, key, key2, key3, key4, key5, key6, KeySet.KS_END);
		assertEquals(6, ks.length());
		assertEquals(key6.get(), ks.pop().get());
		assertEquals(5, ks.length());
		assertEquals(key5.get(), ks.pop().get());
		assertEquals(4, ks.length());
		ks.pop();
		ks.pop();
		ks.pop();
		assertEquals(1, ks.length());
		assertEquals(key.get(), ks.pop().get());
		assertEquals(0, ks.length());
	}

	@Test
	public void test_keySetCurrentNextRewind_shouldPass() {
		final KeySet ks = KeySet.create(6, key, key2, key3, key4, key5, key6, KeySet.KS_END);
		assertEquals(null, ks.current().get());
		assertEquals(key.get(), ks.next().get());
		assertEquals(key.get(), ks.current().get());
		assertEquals(key2.get(), ks.next().get());
		assertEquals(key2.get(), ks.current().get());
		ks.rewind();
		assertEquals(null, ks.current().get());
		assertEquals(key.get(), ks.next().get());
		assertEquals(key.get(), ks.current().get());
		assertEquals(key2.get(), ks.next().get());
		assertEquals(key2.get(), ks.current().get());
	}

	@Test
	public void test_keySetHeadTail_shouldPass() {
		final KeySet ks = KeySet.create(6, key, key2, key3, key4, key5, key6, KeySet.KS_END);
		assertEquals(key.get(), ks.head().get());
		assertEquals(key6.get(), ks.tail().get());
	}

	@Test
	public void test_keySetCursor_shouldPass() {
		final KeySet ks = KeySet.create(6, key, key2, key3, key4, key5, key6, KeySet.KS_END);
		assertEquals(-1, ks.getCursor());
		ks.next();
		assertEquals(0, ks.getCursor());
		ks.next();
		ks.next();
		assertEquals(2, ks.getCursor());
		ks.rewind();
		assertEquals(-1, ks.getCursor());
		ks.next();
		assertEquals(0, ks.getCursor());
		ks.next();
		ks.next();
		ks.next();
		assertEquals(3, ks.getCursor());
		// set cursor
		ks.setCursor(1);
		assertEquals(1, ks.getCursor());
		assertEquals(key3.get(), ks.next().get());
		// at
		assertEquals(key5.get(), ks.at(4).get());
	}

	@Test
	public void test_keySetLookup_shouldPass() {
		final KeySet ks = KeySet.create(6, key, key2, key3, key4, key5, key6, KeySet.KS_END);
		final Key searched_key = ks.lookup(key);
		assertEquals(searched_key.get(), key.get());
	}

	@Test
	public void test_keySetLookupByName_shouldPass() {
		final KeySet ks = KeySet.create(6, key, key2, key3, key4, key5, key6, KeySet.KS_END);
		final Key searched_key = ks.lookup(key.getName());
		assertEquals(searched_key.get(), key.get());
	}

}
