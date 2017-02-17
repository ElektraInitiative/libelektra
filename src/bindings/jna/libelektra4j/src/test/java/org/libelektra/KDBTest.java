package org.libelektra;

import static org.junit.Assert.assertEquals;

import java.util.Iterator;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.libelektra.KDB.KDBException;

public class KDBTest {

	private static final String PARENT_KEY_NAME = "user/sw/tests/jna/1/";
	private static final String KEY_1_NAME = "user/sw/tests/jna/1/key_name";
	private static final String KEY_1_VALUE = "key_value_1";

	private static final String KEY_2_NAME = "user/sw/tests/jna/1/key_name/2";
	private static final String KEY_2_VALUE = "key_value_2";

	private Key key, key2, parentKey;

	@Before
	public void initializeSingleTest() {
		key = Key.create(KEY_1_NAME, Key.KEY_VALUE, KEY_1_VALUE, Key.KEY_END);
		key2 = Key.create(KEY_2_NAME, Key.KEY_VALUE, KEY_2_VALUE, Key.KEY_END);
		parentKey = Key.create(PARENT_KEY_NAME, Key.KEY_END);
	}

	/*
	 * This test is currently designed to work with the current behavior of kdbGet which returns all keys of the given namespace. As seen in
	 * https://github.com/ElektraInitiative/libelektra/issues/1258, this will probably change.
	 */
	@Test
	public void test_kdbGet_shouldPass() throws KDBException {
		try (final KDB kdb = KDB.open(parentKey)) {
			final KeySet ks = KeySet.create(10, KeySet.KS_END);
			kdb.get(ks, parentKey);
			ks.append(key);
			ks.append(key2);
			kdb.set(ks, parentKey);
		}

		// now retrieve them
		try (final KDB kdb = KDB.open(parentKey)) {
			final KeySet ks = KeySet.create(10, KeySet.KS_END);
			kdb.get(ks, parentKey);
			final Key k = ks.lookup(key2);
			assertEquals(key2.toString(), k.toString());
		}
	}

	@After
	public void removeCreatedKeys() throws KDBException {
		try (final KDB kdb = KDB.open(parentKey)) {
			final KeySet ks = KeySet.create(10, KeySet.KS_END);
			kdb.get(ks, parentKey);
			final Iterator<Key> keyIter = ks.iterator();
			while (keyIter.hasNext()) {
				final Key next = keyIter.next();
				if (next.toString().equals(key.toString()) || next.toString().equals(key2.toString())) {
					keyIter.remove();
				}
			}
			kdb.set(ks, parentKey);
		}
	}

}
