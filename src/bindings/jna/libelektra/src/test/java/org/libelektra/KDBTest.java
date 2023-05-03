package org.libelektra;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.libelektra.exception.KDBClosedException;

public class KDBTest {

  private static final String PARENT_KEY_NAME = "user:/sw/tests/jna/";

  private static final String KEY_0_NAME = "user:/sw/tests/jna/0/key_name";
  private static final String KEY_0_VALUE = "key_value_0";

  private static final String KEY_1_NAME = "user:/sw/tests/jna/1/key_name";
  private static final String KEY_1_VALUE = "key_value_1";

  private static final String KEY_2_NAME = "user:/sw/tests/jna/1/key_name/2";
  private static final String KEY_2_VALUE = "key_value_2";

  @Test
  public void test_openClose_shouldPass() throws KDBException {
    KDB kdb = KDB.open();
    kdb.close();
  }

  @Test(expected = KDBClosedException.class)
  public void test_accessCloseSession_shouldFail() throws KDBException {
    KDB kdb = KDB.open();
    kdb.close();
    kdb.get(Key.create(PARENT_KEY_NAME));
  }

  @Test
  public void test_getSetWithParentKey_shouldPass() throws KDBException {
    var getParentKey = Key.create(PARENT_KEY_NAME);
    var key0 = Key.create(KEY_0_NAME, KEY_0_VALUE);
    var key1 = Key.create(KEY_1_NAME, KEY_1_VALUE);
    var key2 = Key.create(KEY_2_NAME, KEY_2_VALUE);

    try (KDB kdb = KDB.open()) {
      var keySet = kdb.get(getParentKey);
      keySet.append(key0).append(key1).append(key2);
      kdb.set(keySet, getParentKey);
    }

    // now retrieve them
    try (KDB kdb = KDB.open()) {
      var keySet = kdb.get(getParentKey);

      var oFoundKey = keySet.lookup(key1);
      assertTrue(oFoundKey.isPresent());
      assertEquals(key1.toString(), oFoundKey.get().toString());

      oFoundKey = keySet.lookup(key2);
      assertTrue(oFoundKey.isPresent());
      assertEquals(key2.toString(), oFoundKey.get().toString());

      // should not be found because not below setParentKey
      oFoundKey = keySet.lookup(key0);
      assertTrue(oFoundKey.isPresent());
      assertEquals(key0.toString(), oFoundKey.get().toString());
    }

    // remove them
    try (KDB kdb = KDB.open()) {
      var keySet = kdb.get(getParentKey);
      var keyIter = keySet.iterator();
      while (keyIter.hasNext()) {
        var next = keyIter.next();
        if (next.getName().equals(key1.getName())
            || next.getName().equals(key2.getName())
            || next.getName().equals(key0.getName())) {
          keyIter.remove();
        }
      }
      kdb.set(keySet, getParentKey);
    }
  }

  @Test
  public void test_getImplementationsEquals_shouldPass() throws KDBException {
    var getParentKey = Key.create(PARENT_KEY_NAME);
    var key0 = Key.create(KEY_0_NAME, KEY_0_VALUE);
    var key1 = Key.create(KEY_1_NAME, KEY_1_VALUE);
    var key2 = Key.create(KEY_2_NAME, KEY_2_VALUE);

    try (KDB kdb = KDB.open()) {
      var keySet = kdb.get(getParentKey);
      keySet.append(key0).append(key1).append(key2);
      kdb.set(keySet, getParentKey);
    }

    // now compare both get implementations
    KeySet keySet1;
    var keySet2 = KeySet.create();

    try (KDB kdb = KDB.open()) {
      keySet1 = kdb.get(getParentKey);
    }

    try (KDB kdb = KDB.open()) {
      keySet2 = KeySet.create();
      kdb.get(keySet2, getParentKey);
    }

    assertEquals(keySet1, keySet2);

    // remove them
    try (KDB kdb = KDB.open()) {
      var keySet = kdb.get(getParentKey);

      keySet.remove(key0);
      keySet.remove(key1);
      keySet.remove(key2);

      kdb.set(keySet, getParentKey);
    }
  }
}
