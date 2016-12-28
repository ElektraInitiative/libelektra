package org.libelektra;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

public class KDBTest {

	private static final String KEY_1_NAME = "user/kdb_test/1/key_name";
	private static final String KEY_1_VALUE = "key_value_1";

	private static final String KEY_2_NAME = "user/kdb_test/1/key_name/2";
	private static final String KEY_2_VALUE = "key_value_2";

	private Key key, key2;

	@Before
	public void initializeSingleTest() {
		key = Key.create(KEY_1_NAME, Key.KEY_VALUE, KEY_1_VALUE, Key.KEY_END);
		key2 = Key.create(KEY_2_NAME, Key.KEY_VALUE, KEY_2_VALUE, Key.KEY_END);
	}

	@Test
	public void test_kdbGet_shouldPass() {
		try (KDB kdb = KDB.open(key)) {
			final KeySet ks = KeySet.create(10, key2, key, KeySet.KS_END);

			kdb.get(ks, key);
			final Key k = ks.lookup(key);
			assertEquals(k.toString(), key.toString());
		} catch (final KDB.KDBException e) {
			System.out.println(e);
		}
	}

}
