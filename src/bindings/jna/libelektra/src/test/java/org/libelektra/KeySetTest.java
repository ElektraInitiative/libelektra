package org.libelektra;

import static org.junit.Assert.*;

import com.sun.jna.Pointer;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Optional;
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
    key = Key.create(KEY_1_NAME, KEY_1_VALUE);
    key2 = Key.create(KEY_2_NAME, KEY_2_VALUE);
    key3 = Key.create(KEY_3_NAME, KEY_3_VALUE);
    key4 = Key.create(KEY_4_NAME, KEY_4_VALUE);
    key5 = Key.create(KEY_5_NAME, KEY_5_VALUE);
    key6 = Key.create(KEY_6_NAME, KEY_6_VALUE);
  }

  @Test
  public void test_keySetCreate_shouldPass() {
    var ks = KeySet.create(6, key, key2, key3, key4, key5, key6);

    assertEquals(6, ks.size());
  }

  @Test
  public void test_keySetRefCntDelProtection_shouldPass() {
    var ks = KeySet.create();
    Pointer nativePointer = ks.getPointer();
    Elektra.INSTANCE.ksDel(nativePointer);
    // manual decreasing the reference count would potentially break cleaner
    Elektra.INSTANCE.ksDel(nativePointer);
    // no assertion since this tests a native library precondition
    // testing the native library's reference counter via direct calls to C API is out of scope
  }

  @Test
  public void test_keySetCreateFromPointer_shouldPass() {
    var ks = KeySet.create(6, key, key2, key3, key4, key5, key6);
    var ks2 = new KeySet(ks.getPointer());

    assertEquals(ks.getPointer(), ks2.getPointer());
    assertEquals(ks.size(), ks2.size());
  }

  @Test
  public void test_keySetToString_shouldPass() {
    var ks = KeySet.create(6, key, key2, key3, key4, key5, key6);
    var expected_result =
        key.toString()
            + "\n"
            + key2.toString()
            + "\n"
            + key3.toString()
            + "\n"
            + key4.toString()
            + "\n"
            + key5.toString()
            + "\n"
            + key6.toString();

    assertEquals(expected_result, ks.toString());
  }

  @Test
  public void test_keySetDup_shouldPass() {
    var ks = KeySet.create(6, key, key2, key3, key4, key5, key6);
    var ks2 = ks.dup();

    // note: compare pointers, because object will be cloned too
    assertEquals(ks.at(0).getPointer(), ks2.at(0).getPointer());
    assertEquals(ks.at(3).getPointer(), ks2.at(3).getPointer());
    assertEquals(ks.at(5).getPointer(), ks2.at(5).getPointer());
  }

  @Test
  public void test_keySetCopy_shouldPass() {
    var ks = KeySet.create(6, key, key2, key3, key4, key5, key6);
    var ks2 = KeySet.create(6).copy(ks);

    // note: compare pointers, because object will be cloned
    assertEquals(ks.at(0).getPointer(), ks2.at(0).getPointer());
    assertEquals(ks.at(3).getPointer(), ks2.at(3).getPointer());
    assertEquals(ks.at(5).getPointer(), ks2.at(5).getPointer());
  }

  @Test
  public void test_keySetAppend_shouldPass() {
    var ks = KeySet.create(10);

    assertEquals(key.getName(), ks.append(key).at(0).getName());
    assertEquals(key2.getName(), ks.append(key2).at(1).getName());
    assertEquals(key3.getName(), ks.append(key3).at(2).getName());
  }

  @Test
  public void test_keySetAppendKeySet_shouldPass() {
    var ks = KeySet.create(10);
    var ks2 = KeySet.create(3, key, key2, key3);
    var ks3 = KeySet.create(3, key4, key5, key6);

    assertEquals(3, ks.append(ks2).size());
    assertEquals(ks.at(0).getName(), ks2.at(0).getName());
    assertEquals(ks.at(0).getString(), ks2.at(0).getString());
    assertEquals(6, ks.append(ks3).size());
  }

  @Test
  public void test_keySetCut_shouldPass() {
    var ks = KeySet.create(6, key, key2, key3, key4, key5, key6);
    var ks2 = ks.cut(key4);

    assertEquals(3, ks2.size());
    assertTrue(ks2.lookup(key4).isPresent());
    assertTrue(ks2.lookup(key5).isPresent());
    assertTrue(ks2.lookup(key6).isPresent());
  }

  @Test
  public void test_keySetRemove_shouldPass() {
    var ks = KeySet.create(6, key, key2, key3, key4, key5, key6);

    assertEquals(6, ks.size());
    assertEquals(key6.getPointer(), ks.remove(5).getPointer());
    assertEquals(5, ks.size());
    assertEquals(key5.getPointer(), ks.remove(4).getPointer());
    assertEquals(4, ks.size());

    ks.remove(3);
    ks.remove(2);
    ks.remove(1);

    assertEquals(1, ks.size());
    assertEquals(key.getPointer(), ks.remove(0).getPointer());
    assertEquals(0, ks.size());
  }

  @Test
  public void test_keySetRemoveByKey_shouldPass() {
    var ks = KeySet.create(2, key, key5);

    assertEquals(2, ks.size());
    assertTrue(ks.lookup(key).isPresent());
    var oRemovedKey = ks.remove(key);
    assertTrue(oRemovedKey.isPresent());
    assertEquals(key.getName(), oRemovedKey.get().getName());
    assertEquals(1, ks.size());
    assertTrue(ks.lookup(key).isEmpty());
  }

  @Test
  public void test_keySetRemoveByKey_shouldThrow() {
    var ks = KeySet.create(2, key, key5);
    assertTrue(ks.remove(key6).isEmpty());
  }

  @Test
  public void test_keySetRemoveByKeyName_shouldPass() {
    var ks = KeySet.create(2, key, key5);

    assertEquals(2, ks.size());
    assertTrue(ks.lookup(key).isPresent());
    var oRemovedKey = ks.remove(key.getName());
    assertTrue(oRemovedKey.isPresent());
    assertEquals(key.getName(), oRemovedKey.get().getName());
    assertEquals(1, ks.size());
    assertTrue(ks.lookup(key).isEmpty());
  }

  @Test
  public void test_keySetRemoveByKeyName_shouldThrow() {
    var ks = KeySet.create(2, key, key5);
    assertTrue(ks.remove(key6.getName()).isEmpty());
  }

  @Test
  public void test_keySetLookup_shouldPass() {
    Optional<Key> oFoundKey = KeySet.create(6, key, key2, key3, key4, key5, key6).lookup(key);

    assertTrue(oFoundKey.isPresent());
    assertEquals(oFoundKey.get().getPointer(), key.getPointer());
  }

  @Test
  public void test_keySetLookupByName_shouldPass() {
    Optional<Key> oFoundKey =
        KeySet.create(6, key, key2, key3, key4, key5, key6).lookup(key.getName());

    assertTrue(oFoundKey.isPresent());
    assertEquals(oFoundKey.get().getPointer(), key.getPointer());
  }

  @Test(expected = IllegalStateException.class)
  public void test_accessingKeySetAfterRelease_shouldThrow() {
    var keySet = KeySet.create();
    keySet.release();

    keySet.getPointer();
  }

  @Test
  public void test_keySetLookupMissShouldReturnNull_shouldPass() {
    Optional<Key> oFoundKey = KeySet.create(5, key2, key3, key4, key5, key6).lookup(key);

    assertTrue(oFoundKey.isEmpty());
  }

  @Test
  public void test_keySetLookupByNameMissShouldReturnNull_shouldPass() {
    Optional<Key> oFoundKey =
        KeySet.create(5, key2, key3, key4, key5, key6).lookup("NOT_IN_KEY_SET");

    assertTrue(oFoundKey.isEmpty());
  }

  @Test
  public void test_keySetIndexOf_shouldPass() {
    var keySet = KeySet.create(5, key2, key3, key4, key5, key6);

    assertEquals(0, keySet.indexOf(key2));
    assertEquals(1, keySet.indexOf(key3));
    assertEquals(2, keySet.indexOf(key4));
    assertEquals(3, keySet.indexOf(key5));
    assertEquals(4, keySet.indexOf(key6));
  }

  @Test(expected = IllegalArgumentException.class)
  public void test_keySetIndexOf_shouldFail() {
    var keySet = KeySet.create(5, key2, key3, key4, key5, key6);

    keySet.indexOf(key);
  }

  @Test
  public void test_keySetIterator_shouldPass() {
    var ks = KeySet.create(6, key, key2, key3, key4, key5, key6);
    Iterator<Key> iterator = ks.iterator();

    assertTrue(iterator.hasNext());
    // note: compare key pointer instead of key object
    assertEquals(key.getPointer(), iterator.next().getPointer());
    assertTrue(iterator.hasNext());
    assertEquals(key2.getPointer(), iterator.next().getPointer());
    assertTrue(iterator.hasNext());
    assertEquals(key3.getPointer(), iterator.next().getPointer());
    assertTrue(iterator.hasNext());
    assertEquals(key4.getPointer(), iterator.next().getPointer());
    assertTrue(iterator.hasNext());
    assertEquals(key5.getPointer(), iterator.next().getPointer());
    assertTrue(iterator.hasNext());
    assertEquals(key6.getPointer(), iterator.next().getPointer());
    assertFalse(iterator.hasNext());
  }

  @Test
  public void test_keySetIteratorRemove_shouldPass() {
    var ks = KeySet.create(6, key, key2, key3, key4, key5, key6);
    Iterator<Key> iterator = ks.iterator();

    assertTrue(iterator.hasNext());

    while (iterator.hasNext()) {
      var next = iterator.next();
      if (key3.getPointer().equals(next.getPointer())) {
        iterator.remove();
      }
    }

    assertFalse(iterator.hasNext());
    assertEquals(5, ks.size());
    assertTrue(ks.lookup(key3).isEmpty());
    assertTrue(ks.lookup(key4).isPresent());
    assertTrue(ks.lookup(key2).isPresent());
  }

  @Test
  public void test_keySetSize_shouldPass() {
    var ks = KeySet.create(10);

    assertEquals(0, ks.size());

    ks.append(key);

    assertEquals(1, ks.size());

    ks.append(key2).append(key3).append(key4);

    assertEquals(4, ks.size());

    ks.append(key5).append(key6);

    assertEquals(6, ks.size());
  }

  @Test
  public void test_keySetIsEmpty_shouldPass() {
    var keySet = KeySet.create(5, key2, key3, key4, key5, key6);

    assertFalse(keySet.isEmpty());

    keySet = KeySet.create();

    assertTrue(keySet.isEmpty());
  }

  @Test
  public void test_keySetContains_shouldPass() {
    var keySet = KeySet.create(5, key2, key3, key4, key5, key6);

    assertFalse(keySet.contains(key));
    assertTrue(keySet.contains(key2));
    assertTrue(keySet.contains(key3));
    assertTrue(keySet.contains(key4));
    assertTrue(keySet.contains(key5));
    assertTrue(keySet.contains(key6));
  }

  @Test
  public void test_keySetToArray_shouldPass() {
    Object[] array = KeySet.create(5, key2, key3, key4, key5, key6).toArray();

    assertEquals(key2, array[0]);
    assertEquals(key3, array[1]);
    assertEquals(key4, array[2]);
    assertEquals(key5, array[3]);
    assertEquals(key6, array[4]);
  }

  @Test
  public void test_keySetToArrayType_shouldPass() {
    Key[] array = KeySet.create(5, key2, key3, key4, key5, key6).toArray(new Key[5]);

    assertEquals(key2, array[0]);
    assertEquals(key3, array[1]);
    assertEquals(key4, array[2]);
    assertEquals(key5, array[3]);
    assertEquals(key6, array[4]);
  }

  @Test
  public void test_keySetAdd_shouldPass() {
    var keySet = KeySet.create(5);

    assertTrue(keySet.add(key2));
    assertTrue(keySet.add(key3));
    assertTrue(keySet.add(key4));
    assertTrue(keySet.add(key5));
    assertTrue(keySet.add(key6));

    assertTrue(keySet.contains(key2));
    assertTrue(keySet.contains(key3));
    assertTrue(keySet.contains(key4));
    assertTrue(keySet.contains(key5));
    assertTrue(keySet.contains(key6));
  }

  @Test
  public void test_keySetRemoveObject_shouldPass() {
    var keySet = KeySet.create(key, key2);

    assertTrue(keySet.remove((Object) key2));
    assertFalse(keySet.remove((Object) key2));
    assertTrue(keySet.contains(key));
    assertFalse(keySet.contains(new Object()));
  }

  @Test
  public void test_keySetContainsAll_shouldPass() {
    var keySet = KeySet.create(5, key2, key3, key4, key5, key6);

    assertFalse(keySet.containsAll(Arrays.asList(key, key2)));
    assertTrue(keySet.containsAll(Arrays.asList(key2)));
    assertTrue(keySet.containsAll(Arrays.asList(key2, key3, key4, key5, key6)));
  }

  @Test(expected = NullPointerException.class)
  public void test_keySetContainsAll_shouldFail() {
    var keySet = KeySet.create();

    keySet.containsAll(Arrays.asList(null, key));
  }

  @Test
  public void test_keySetAddAll_shouldPass() {
    var keySet = KeySet.create();

    assertTrue(keySet.addAll(Arrays.asList(key, key2)));

    assertTrue(keySet.contains(key));
    assertTrue(keySet.contains(key2));
  }

  @Test
  public void test_keySetRetainAll_shouldPass() {
    var keySet = KeySet.create(5, key2, key3, key4, key5, key6);

    assertFalse(keySet.retainAll(Arrays.asList(key2, key3, key4, key5, key6)));
    assertTrue(keySet.retainAll(Arrays.asList(key2, key3, key4, key5)));
    assertTrue(keySet.contains(key2));
    assertTrue(keySet.contains(key3));
    assertTrue(keySet.contains(key4));
    assertTrue(keySet.contains(key5));
    assertFalse(keySet.contains(key6));
  }

  @Test
  public void test_keySetRemoveAll_shouldPass() {
    var keySet = KeySet.create(5, key2, key3, key4, key5, key6);

    assertFalse(keySet.removeAll(Arrays.asList(key)));
    assertTrue(keySet.removeAll(Arrays.asList(key2, key3, key4, key5)));
    assertFalse(keySet.contains(key2));
    assertFalse(keySet.contains(key3));
    assertFalse(keySet.contains(key4));
    assertFalse(keySet.contains(key5));
    assertTrue(keySet.contains(key6));
  }

  @Test
  public void test_keySetClear_shouldPass() {
    var keySet = KeySet.create(5, key2, key3, key4, key5, key6);
    keySet.clear();

    assertEquals(0, keySet.size());
    assertFalse(keySet.contains(key2));
    assertFalse(keySet.contains(key3));
    assertFalse(keySet.contains(key4));
    assertFalse(keySet.contains(key5));
    assertFalse(keySet.contains(key6));
  }

  @Test
  public void test_keySetComperator_shouldPass() {
    var keySet = KeySet.create();

    assertNull(keySet.comparator());
  }

  @Test
  public void test_keySetFirst_shouldPass() {
    var keySet = KeySet.create(5, key2, key3, key4, key5, key6);

    assertEquals(key2, keySet.first());
    assertNotEquals(key3, keySet.first());
    assertNotEquals(key4, keySet.first());
    assertNotEquals(key5, keySet.first());
    assertNotEquals(key6, keySet.first());
  }

  @Test
  public void test_keySetLast_shouldPass() {
    var keySet = KeySet.create(5, key2, key3, key4, key5, key6);

    assertNotEquals(key2, keySet.last());
    assertNotEquals(key3, keySet.last());
    assertNotEquals(key4, keySet.last());
    assertNotEquals(key5, keySet.last());
    assertEquals(key6, keySet.last());
  }

  @Test
  public void test_keySetSubSet_shouldPass() {
    var keySet = KeySet.create(key, key4, key6);

    // fromElement > toElement
    assertThrows(IllegalArgumentException.class, () -> keySet.subSet(key4, key));

    // key2 is not part of key set
    assertThrows(IllegalArgumentException.class, () -> keySet.subSet(key2, key6));

    // key3 is not part of key set
    assertThrows(IllegalArgumentException.class, () -> keySet.subSet(key, key3));

    // fromElement == toElement
    assertTrue(keySet.subSet(key, key).isEmpty());

    // sub set [key, key4[ = {key}
    var subSet = keySet.subSet(key, key4);

    assertEquals(1, subSet.size());
    assertEquals(key, subSet.first());
    assertEquals(key, subSet.last());
    assertTrue(subSet.contains(key));
    assertFalse(subSet.contains(key2));
    assertFalse(subSet.contains(key3));
    assertFalse(subSet.contains(key4));
    assertFalse(subSet.contains(key5));
    assertFalse(subSet.contains(key6));

    // insert key2 into key set
    // -> sub set = {key, key2}
    // -> key set = {key, key2, key4, key6}
    assertTrue(keySet.add(key2));

    assertEquals(2, subSet.size());
    assertEquals(key, subSet.first());
    assertEquals(key2, subSet.last());
    assertTrue(subSet.contains(key));
    assertTrue(subSet.contains(key2));
    assertFalse(subSet.contains(key3));
    assertFalse(subSet.contains(key4));
    assertFalse(subSet.contains(key5));
    assertFalse(subSet.contains(key6));

    // sub set already contains key and key2
    assertFalse(subSet.add(key));
    assertFalse(subSet.add(key2));

    // insert key3 into sub set
    // -> sub set = {key, key2, key3}
    // -> key set = {key, key2, key3, key4, key6}
    assertTrue(subSet.add(key3));

    // key4, key5 and key6 are above exclusive upper bound
    assertThrows(IllegalArgumentException.class, () -> subSet.add(key4));
    assertThrows(IllegalArgumentException.class, () -> subSet.add(key5));
    assertThrows(IllegalArgumentException.class, () -> subSet.add(key6));
    assertFalse(subSet.remove(key4));
    assertFalse(subSet.remove(key5));
    assertFalse(subSet.remove(key6));

    assertEquals(3, subSet.size());
    assertEquals(key, subSet.first());
    assertEquals(key3, subSet.last());
    assertTrue(subSet.contains(key));
    assertTrue(subSet.contains(key2));
    assertTrue(subSet.contains(key3));
    assertTrue(keySet.contains(key3));
    assertFalse(subSet.contains(key4));
    assertFalse(subSet.contains(key5));
    assertFalse(subSet.contains(key6));

    // remove key from sub set
    // -> sub set = {key2, key3}
    // -> key set = {key2, key3, key4, key6}
    assertTrue(subSet.remove(key));

    assertEquals(2, subSet.size());
    assertEquals(key2, subSet.first());
    assertEquals(key3, subSet.last());
    assertFalse(subSet.contains(key));
    assertFalse(keySet.contains(key));
    assertTrue(subSet.contains(key2));
    assertTrue(subSet.contains(key3));
    assertFalse(subSet.contains(key4));
    assertFalse(subSet.contains(key5));
    assertFalse(subSet.contains(key6));

    // re-add key to sub set
    // -> sub set = {key, key2, key3}
    // -> key set = {key, key2, key3, key4, key6}
    assertTrue(subSet.add(key));

    assertEquals(3, subSet.size());
    assertEquals(key, subSet.first());
    assertEquals(key3, subSet.last());
    assertTrue(subSet.contains(key));
    assertTrue(subSet.contains(key2));
    assertTrue(subSet.contains(key3));
    assertTrue(keySet.contains(key3));
    assertFalse(subSet.contains(key4));
    assertFalse(subSet.contains(key5));
    assertFalse(subSet.contains(key6));

    // sub sub set [key2, key6[ = {key2, key3, key4}
    var subSubSet = keySet.subSet(key, key6).subSet(key2, key6);

    assertEquals(3, subSubSet.size());
    assertEquals(key2, subSubSet.first());
    assertEquals(key4, subSubSet.last());
    assertFalse(subSubSet.contains(key));
    assertTrue(subSubSet.contains(key2));
    assertTrue(subSubSet.contains(key3));
    assertTrue(subSubSet.contains(key4));
    assertFalse(subSubSet.contains(key5));
    assertFalse(subSubSet.contains(key6));

    // add key5 to sub sub set
    // -> sub set = {key, key2, key3}
    // -> sub sub set = {key, key2, key3, key5}
    // -> key set = {key, key2, key3, key4, key5, key6}
    assertTrue(subSubSet.add(key5));

    assertThrows(IllegalArgumentException.class, () -> subSubSet.add(key));
    assertThrows(IllegalArgumentException.class, () -> subSubSet.add(key6));

    assertEquals(4, subSubSet.size());
    assertEquals(key2, subSubSet.first());
    assertEquals(key5, subSubSet.last());
    assertFalse(subSubSet.contains(key));
    assertTrue(subSubSet.contains(key2));
    assertTrue(subSubSet.contains(key3));
    assertTrue(subSubSet.contains(key4));
    assertTrue(subSubSet.contains(key5));
    assertFalse(subSubSet.contains(key6));
  }

  @Test
  public void test_keySetHeadSetShouldPass() {
    var keySet = KeySet.create(key, key4, key6);

    // key2 is not part of key set
    assertThrows(IllegalArgumentException.class, () -> keySet.headSet(key2));

    // key3 is not part of key set
    assertThrows(IllegalArgumentException.class, () -> keySet.headSet(key3));

    // key5 is not part of key set
    assertThrows(IllegalArgumentException.class, () -> keySet.headSet(key5));

    // toElement == first()
    assertTrue(keySet.headSet(key).isEmpty());

    // head set key4[ = {key}
    var headSet = keySet.headSet(key4);

    assertEquals(1, headSet.size());
    assertEquals(key, headSet.first());
    assertEquals(key, headSet.last());
    assertTrue(headSet.contains(key));
    assertFalse(headSet.contains(key2));
    assertFalse(headSet.contains(key3));
    assertFalse(headSet.contains(key4));
    assertFalse(headSet.contains(key5));
    assertFalse(headSet.contains(key6));

    // insert key2 into key set
    // -> head set = {key, key2}
    // -> key set = {key, key2, key4, key6}
    assertTrue(keySet.add(key2));

    assertEquals(2, headSet.size());
    assertEquals(key, headSet.first());
    assertEquals(key2, headSet.last());
    assertTrue(headSet.contains(key));
    assertTrue(headSet.contains(key2));
    assertFalse(headSet.contains(key3));
    assertFalse(headSet.contains(key4));
    assertFalse(headSet.contains(key5));
    assertFalse(headSet.contains(key6));

    // head set already contains key and key2
    assertFalse(headSet.add(key));
    assertFalse(headSet.add(key2));

    // insert key3 into head set
    // -> head set = {key, key2, key3}
    // -> key set = {key, key2, key3, key4, key6}
    assertTrue(headSet.add(key3));

    // key4, key5 and key6 are above exclusive upper bound
    assertThrows(IllegalArgumentException.class, () -> headSet.add(key4));
    assertThrows(IllegalArgumentException.class, () -> headSet.add(key5));
    assertThrows(IllegalArgumentException.class, () -> headSet.add(key6));
    assertFalse(headSet.remove(key4));
    assertFalse(headSet.remove(key5));
    assertFalse(headSet.remove(key6));

    assertEquals(3, headSet.size());
    assertEquals(key, headSet.first());
    assertEquals(key3, headSet.last());
    assertTrue(headSet.contains(key));
    assertTrue(headSet.contains(key2));
    assertTrue(headSet.contains(key3));
    assertTrue(keySet.contains(key3));
    assertFalse(headSet.contains(key4));
    assertFalse(headSet.contains(key5));
    assertFalse(headSet.contains(key6));

    // remove key from head set
    // -> head set = {key2, key3}
    // -> key set = {key2, key3, key4, key6}
    assertTrue(headSet.remove(key));

    assertEquals(2, headSet.size());
    assertEquals(key2, headSet.first());
    assertEquals(key3, headSet.last());
    assertFalse(headSet.contains(key));
    assertFalse(keySet.contains(key));
    assertTrue(headSet.contains(key2));
    assertTrue(headSet.contains(key3));
    assertFalse(headSet.contains(key4));
    assertFalse(headSet.contains(key5));
    assertFalse(headSet.contains(key6));

    // re-add key to head set
    // -> sub set = {key, key2, key3}
    // -> key set = {key, key2, key3, key4, key6}
    assertTrue(headSet.add(key));

    assertEquals(3, headSet.size());
    assertEquals(key, headSet.first());
    assertEquals(key3, headSet.last());
    assertTrue(headSet.contains(key));
    assertTrue(headSet.contains(key2));
    assertTrue(headSet.contains(key3));
    assertTrue(keySet.contains(key3));
    assertFalse(headSet.contains(key4));
    assertFalse(headSet.contains(key5));
    assertFalse(headSet.contains(key6));
  }

  @Test
  public void test_keySetTailSetShouldPass() {
    var keySet = KeySet.create(key, key2, key4, key6);

    // key3 is not part of key set
    assertThrows(IllegalArgumentException.class, () -> keySet.tailSet(key3));

    // key5 is not part of key set
    assertThrows(IllegalArgumentException.class, () -> keySet.tailSet(key5));

    // tail set [key2 = {key2, key4, key6}
    var tailSet = keySet.tailSet(key2);

    assertEquals(3, tailSet.size());
    assertEquals(key2, tailSet.first());
    assertEquals(key6, tailSet.last());
    assertFalse(tailSet.contains(key));
    assertTrue(tailSet.contains(key2));
    assertFalse(tailSet.contains(key3));
    assertTrue(tailSet.contains(key4));
    assertFalse(tailSet.contains(key5));
    assertTrue(tailSet.contains(key6));

    // insert key3 into key set
    // -> tail set = {key2, key3, key4, key6}
    // -> key set = {key, key2, key3, key4, key6}
    assertTrue(keySet.add(key3));

    assertEquals(4, tailSet.size());
    assertEquals(key2, tailSet.first());
    assertEquals(key6, tailSet.last());
    assertFalse(tailSet.contains(key));
    assertTrue(tailSet.contains(key2));
    assertTrue(tailSet.contains(key3));
    assertTrue(tailSet.contains(key4));
    assertFalse(tailSet.contains(key5));
    assertTrue(tailSet.contains(key6));

    // tail set already contains key2, key3, key4 and key6
    assertFalse(tailSet.add(key2));
    assertFalse(tailSet.add(key3));
    assertFalse(tailSet.add(key4));
    assertFalse(tailSet.add(key6));

    // insert key5 into head set
    // -> tail set = {key2, key3, key4, key5, key6}
    // -> key set = {key, key2, key3, key4, key5, key6}
    assertTrue(tailSet.add(key5));

    // key is below inclusive lower bound
    assertThrows(IllegalArgumentException.class, () -> tailSet.add(key));
    assertFalse(tailSet.remove(key));

    assertEquals(5, tailSet.size());
    assertEquals(key2, tailSet.first());
    assertEquals(key6, tailSet.last());
    assertFalse(tailSet.contains(key));
    assertTrue(tailSet.contains(key2));
    assertTrue(tailSet.contains(key3));
    assertTrue(tailSet.contains(key4));
    assertTrue(tailSet.contains(key5));
    assertTrue(keySet.contains(key5));
    assertTrue(tailSet.contains(key6));

    // remove key2 from tail set
    // -> tail set = {key3, key4, key5, key6}
    // -> key set = {key, key3, key4, key5, key6}
    assertTrue(tailSet.remove(key2));

    assertEquals(4, tailSet.size());
    assertEquals(key3, tailSet.first());
    assertEquals(key6, tailSet.last());
    assertFalse(tailSet.contains(key));
    assertFalse(tailSet.contains(key2));
    assertFalse(keySet.contains(key2));
    assertTrue(tailSet.contains(key3));
    assertTrue(tailSet.contains(key4));
    assertTrue(tailSet.contains(key5));
    assertTrue(tailSet.contains(key6));

    // re-add key2 to head set
    // -> tail set = {key2, key3, key4, key5, key6}
    // -> key set = {key, key2, key3, key4, key5, key6}
    assertTrue(tailSet.add(key2));

    assertEquals(5, tailSet.size());
    assertEquals(key2, tailSet.first());
    assertEquals(key6, tailSet.last());
    assertFalse(tailSet.contains(key));
    assertTrue(tailSet.contains(key2));
    assertTrue(tailSet.contains(key3));
    assertTrue(tailSet.contains(key4));
    assertTrue(tailSet.contains(key5));
    assertTrue(keySet.contains(key5));
    assertTrue(tailSet.contains(key6));
  }

  @Test
  public void test_keySetLower_shouldPass() {
    var keySet = KeySet.create(5, key2, key3, key4, key5);

    assertNull(keySet.lower(key));
    assertNull(keySet.lower(key2));
    assertEquals(key2, keySet.lower(key3));
    assertEquals(key3, keySet.lower(key4));
    assertEquals(key4, keySet.lower(key5));
    assertEquals(key5, keySet.lower(key6));
    assertEquals(key2, keySet.lower(Key.create(KEY_2_NAME + "/1")));
  }

  @Test
  public void test_keySetFloor_shouldPass() {
    var keySet = KeySet.create(5, key2, key3, key4, key5);

    assertNull(keySet.floor(key));
    assertEquals(key2, keySet.floor(key2));
    assertEquals(key3, keySet.floor(key3));
    assertEquals(key4, keySet.floor(key4));
    assertEquals(key5, keySet.floor(key5));
    assertEquals(key5, keySet.floor(key6));
    assertEquals(key2, keySet.floor(Key.create(KEY_2_NAME + "/1")));
  }

  @Test
  public void test_keySetCeiling_shouldPass() {
    var keySet = KeySet.create(5, key2, key3, key4, key5);

    assertEquals(key3, keySet.ceiling(Key.create(KEY_2_NAME + "/1")));
    assertEquals(key2, keySet.ceiling(key));
    assertEquals(key2, keySet.ceiling(key2));
    assertEquals(key3, keySet.ceiling(key3));
    assertEquals(key4, keySet.ceiling(key4));
    assertEquals(key5, keySet.ceiling(key5));
    assertNull(keySet.ceiling(key6));
  }

  @Test
  public void test_keySetHigher_shouldPass() {
    var keySet = KeySet.create(5, key2, key3, key4, key5);

    assertEquals(key3, keySet.higher(Key.create(KEY_2_NAME + "/1")));
    assertEquals(key2, keySet.higher(key));
    assertEquals(key3, keySet.higher(key2));
    assertEquals(key4, keySet.higher(key3));
    assertEquals(key5, keySet.higher(key4));
    assertNull(keySet.higher(key5));
    assertNull(keySet.higher(key6));
  }

  @Test
  public void test_keySetPollFirst_shouldPass() {
    var keySet = KeySet.create(2, key, key5);

    assertEquals(2, keySet.size());
    assertTrue(keySet.lookup(key).isPresent());
    assertEquals(key, keySet.first());
    var oPolledKey = keySet.pollFirst();
    assertNotNull(oPolledKey);
    assertEquals(key.getName(), oPolledKey.getName());
    assertEquals(1, keySet.size());
    assertTrue(keySet.lookup(key).isEmpty());

    assertEquals(key5, keySet.pollFirst());
    assertNull(keySet.pollFirst());
  }

  @Test
  public void test_keySetPollLast_shouldPass() {
    var keySet = KeySet.create(5, key2, key3, key4, key5, key6);

    assertEquals(5, keySet.size());
    assertTrue(keySet.lookup(key6).isPresent());
    assertEquals(key6, keySet.last());
    var oPolledKey = keySet.pollLast();
    assertNotNull(oPolledKey);
    assertEquals(key6.getName(), oPolledKey.getName());
    assertEquals(4, keySet.size());
    assertTrue(keySet.lookup(key6).isEmpty());

    assertNull(KeySet.create().pollLast());
  }

  @Test
  public void test_keySetDescendingSet_shouldPass() {
    var keySet = KeySet.create(3, key3, key4, key5).descendingSet();
    // key5, key4, key3

    assertEquals(key5, keySet.first());
    assertEquals(key3, keySet.last());

    assertNull(keySet.lower(key6));
    assertNull(keySet.lower(key5));
    assertEquals(key5, keySet.lower(key4));
    assertEquals(key4, keySet.lower(key3));
    assertEquals(key3, keySet.lower(key));
    assertEquals(key4, keySet.lower(Key.create(KEY_3_NAME + "1")));

    assertNull(keySet.floor(key6));
    assertEquals(key5, keySet.floor(key5));
    assertEquals(key4, keySet.floor(key4));
    assertEquals(key3, keySet.floor(key3));
    assertEquals(key3, keySet.lower(key));
    assertEquals(key4, keySet.floor(Key.create(KEY_3_NAME + "1")));

    assertEquals(key3, keySet.ceiling(Key.create(KEY_3_NAME + "1")));
    assertEquals(key5, keySet.ceiling(key6));
    assertEquals(key5, keySet.ceiling(key5));
    assertEquals(key4, keySet.ceiling(key4));
    assertEquals(key3, keySet.ceiling(key3));
    assertNull(keySet.ceiling(key2));

    assertEquals(key3, keySet.ceiling(Key.create(KEY_3_NAME + "1")));
    assertEquals(key5, keySet.ceiling(key6));
    assertEquals(key4, keySet.higher(key5));
    assertEquals(key3, keySet.higher(key4));
    assertNull(keySet.higher(key3));
    assertNull(keySet.higher(key2));
    assertNull(keySet.higher(key));
  }

  @Test
  public void test_keySetDescendingIterator_shouldPass() {
    var ks = KeySet.create(6, key, key2, key3, key4, key5, key6);
    Iterator<Key> iterator = ks.descendingIterator();

    assertTrue(iterator.hasNext());
    // note: compare key pointer instead of key object
    assertEquals(key6.getPointer(), iterator.next().getPointer());
    assertTrue(iterator.hasNext());
    assertEquals(key5.getPointer(), iterator.next().getPointer());
    assertTrue(iterator.hasNext());
    assertEquals(key4.getPointer(), iterator.next().getPointer());
    assertTrue(iterator.hasNext());
    assertEquals(key3.getPointer(), iterator.next().getPointer());
    assertTrue(iterator.hasNext());
    assertEquals(key2.getPointer(), iterator.next().getPointer());
    assertTrue(iterator.hasNext());
    assertEquals(key.getPointer(), iterator.next().getPointer());
    assertFalse(iterator.hasNext());
  }

  @Test
  public void test_keySetDescendingIteratorRemove_shouldPass() {
    var ks = KeySet.create(6, key, key2, key3, key4, key5, key6);
    Iterator<Key> iterator = ks.descendingIterator();

    assertTrue(iterator.hasNext());

    while (iterator.hasNext()) {
      var next = iterator.next();
      if (key3.getPointer().equals(next.getPointer())) {
        iterator.remove();
      }
    }

    assertFalse(iterator.hasNext());
    assertEquals(5, ks.size());
    assertTrue(ks.lookup(key3).isEmpty());
    assertTrue(ks.lookup(key4).isPresent());
    assertTrue(ks.lookup(key2).isPresent());
  }

  @Test
  public void test_keySetSubSet2_shouldPass() {
    var keySet = KeySet.create(key, key4, key6);

    // fromElement > toElement
    assertThrows(IllegalArgumentException.class, () -> keySet.subSet(key4, false, key, true));

    // key2 is not part of key set
    assertThrows(IllegalArgumentException.class, () -> keySet.subSet(key2, false, key6, true));

    // key3 is not part of key set
    assertThrows(IllegalArgumentException.class, () -> keySet.subSet(key, false, key3, true));

    // fromElement == toElement
    assertTrue(keySet.subSet(key, key).isEmpty());

    // sub set key[, [key4 = {key}
    var subSet = keySet.subSet(key, false, key4, true); // true, false

    assertEquals(1, subSet.size());
    assertEquals(key4, subSet.first());
    assertEquals(key4, subSet.last());
    assertFalse(subSet.contains(key));
    assertFalse(subSet.contains(key2));
    assertFalse(subSet.contains(key3));
    assertTrue(subSet.contains(key4));
    assertFalse(subSet.contains(key5));
    assertFalse(subSet.contains(key6));

    // insert key2 into key set
    // -> sub set = {key2, key4}
    // -> key set = {key, key2, key4, key6}
    assertTrue(keySet.add(key2));

    assertEquals(2, subSet.size());
    assertEquals(key2, subSet.first());
    assertEquals(key4, subSet.last());
    assertFalse(subSet.contains(key));
    assertTrue(subSet.contains(key2));
    assertFalse(subSet.contains(key3));
    assertTrue(subSet.contains(key4));
    assertFalse(subSet.contains(key5));
    assertFalse(subSet.contains(key6));

    // sub set already contains key and key4
    assertFalse(subSet.add(key2));
    assertFalse(subSet.add(key4));

    // insert key3 into sub set
    // -> sub set = {key2, key3, key4}
    // -> key set = {key, key2, key3, key4, key6}
    assertTrue(subSet.add(key3));

    // key, key5 and key6 are above exclusive upper bound
    assertThrows(IllegalArgumentException.class, () -> subSet.add(key));
    assertThrows(IllegalArgumentException.class, () -> subSet.add(key5));
    assertThrows(IllegalArgumentException.class, () -> subSet.add(key6));
    assertFalse(subSet.remove(key));
    assertFalse(subSet.remove(key5));
    assertFalse(subSet.remove(key6));

    assertEquals(3, subSet.size());
    assertEquals(key2, subSet.first());
    assertEquals(key4, subSet.last());
    assertFalse(subSet.contains(key));
    assertTrue(subSet.contains(key2));
    assertTrue(subSet.contains(key3));
    assertTrue(keySet.contains(key3));
    assertTrue(subSet.contains(key4));
    assertFalse(subSet.contains(key5));
    assertFalse(subSet.contains(key6));

    // remove key from sub set
    // -> sub set = {key2, key4}
    // -> key set = {key2, key3, key4, key6}
    assertTrue(subSet.remove(key3));

    assertEquals(2, subSet.size());
    assertEquals(key2, subSet.first());
    assertEquals(key4, subSet.last());
    assertFalse(subSet.contains(key3));
    assertFalse(keySet.contains(key3));
    assertTrue(subSet.contains(key2));
    assertTrue(subSet.contains(key4));
    assertFalse(subSet.contains(key));
    assertFalse(subSet.contains(key5));
    assertFalse(subSet.contains(key6));

    // re-add key to sub set
    // -> sub set = {key2, key3, key4}
    // -> key set = {key, key2, key3, key4, key6}
    assertTrue(subSet.add(key3));

    assertEquals(3, subSet.size());
    assertEquals(key2, subSet.first());
    assertEquals(key4, subSet.last());
    assertTrue(subSet.contains(key2));
    assertTrue(subSet.contains(key3));
    assertTrue(subSet.contains(key4));
    assertTrue(keySet.contains(key3));
    assertFalse(subSet.contains(key));
    assertFalse(subSet.contains(key5));
    assertFalse(subSet.contains(key6));

    // sub sub set key2[, [key6 = {key3, key4, key6}
    var subSubSet = keySet.subSet(key, false, key6, true).subSet(key2, false, key6, true);

    assertEquals(3, subSubSet.size());
    assertEquals(key3, subSubSet.first());
    assertEquals(key6, subSubSet.last());
    assertFalse(subSubSet.contains(key));
    assertFalse(subSubSet.contains(key2));
    assertTrue(subSubSet.contains(key3));
    assertTrue(subSubSet.contains(key4));
    assertFalse(subSubSet.contains(key5));
    assertTrue(subSubSet.contains(key6));

    // add key5 to sub sub set
    // -> sub set = {key2, key3, key4}
    // -> sub sub set = {key3, key4, key5, key6}
    // -> key set = {key, key2, key3, key4, key5, key6}
    assertTrue(subSubSet.add(key5));

    assertThrows(IllegalArgumentException.class, () -> subSubSet.add(key));
    assertThrows(IllegalArgumentException.class, () -> subSubSet.add(key2));

    assertEquals(4, subSubSet.size());
    assertEquals(key3, subSubSet.first());
    assertEquals(key6, subSubSet.last());
    assertFalse(subSubSet.contains(key));
    assertFalse(subSubSet.contains(key2));
    assertTrue(subSubSet.contains(key3));
    assertTrue(subSubSet.contains(key4));
    assertTrue(subSubSet.contains(key5));
    assertTrue(subSubSet.contains(key6));
  }

  @Test
  public void test_keySetHeadSet2ShouldPass() {
    var keySet = KeySet.create(key, key4, key6);

    // key2 is not part of key set
    assertThrows(IllegalArgumentException.class, () -> keySet.headSet(key2, false));
    assertThrows(IllegalArgumentException.class, () -> keySet.headSet(key2, true));

    // key3 is not part of key set
    assertThrows(IllegalArgumentException.class, () -> keySet.headSet(key3, false));
    assertThrows(IllegalArgumentException.class, () -> keySet.headSet(key3, true));

    // key5 is not part of key set
    assertThrows(IllegalArgumentException.class, () -> keySet.headSet(key5, false));
    assertThrows(IllegalArgumentException.class, () -> keySet.headSet(key5, true));

    // toElement == first()
    assertTrue(keySet.headSet(key, false).isEmpty());
    assertFalse(keySet.headSet(key, true).isEmpty());
    assertEquals(1, keySet.headSet(key, true).size());

    // head set key4( = {key, key4}
    var headSet = keySet.headSet(key4, true);

    assertEquals(2, headSet.size());
    assertEquals(key, headSet.first());
    assertEquals(key4, headSet.last());
    assertTrue(headSet.contains(key));
    assertFalse(headSet.contains(key2));
    assertFalse(headSet.contains(key3));
    assertTrue(headSet.contains(key4));
    assertFalse(headSet.contains(key5));
    assertFalse(headSet.contains(key6));

    // insert key2 into key set
    // -> head set = {key, key2, key4}
    // -> key set = {key, key2, key4, key6}
    assertTrue(keySet.add(key2));

    assertEquals(3, headSet.size());
    assertEquals(key, headSet.first());
    assertEquals(key4, headSet.last());
    assertTrue(headSet.contains(key));
    assertTrue(headSet.contains(key2));
    assertFalse(headSet.contains(key3));
    assertTrue(headSet.contains(key4));
    assertFalse(headSet.contains(key5));
    assertFalse(headSet.contains(key6));

    // head set already contains key and key2
    assertFalse(headSet.add(key));
    assertFalse(headSet.add(key2));
    assertFalse(headSet.add(key4));

    // insert key3 into head set
    // -> head set = {key, key2, key3, key4}
    // -> key set = {key, key2, key3, key4, key6}
    assertTrue(headSet.add(key3));

    // key5 and key6 are above exclusive upper bound
    assertThrows(IllegalArgumentException.class, () -> headSet.add(key5));
    assertThrows(IllegalArgumentException.class, () -> headSet.add(key6));
    assertFalse(headSet.remove(key5));
    assertFalse(headSet.remove(key6));

    assertEquals(4, headSet.size());
    assertEquals(key, headSet.first());
    assertEquals(key4, headSet.last());
    assertTrue(headSet.contains(key));
    assertTrue(headSet.contains(key2));
    assertTrue(headSet.contains(key3));
    assertTrue(keySet.contains(key3));
    assertTrue(headSet.contains(key4));
    assertFalse(headSet.contains(key5));
    assertFalse(headSet.contains(key6));

    // remove key from head set
    // -> head set = {key2, key3, key4}
    // -> key set = {key2, key3, key4, key6}
    assertTrue(headSet.remove(key));

    assertEquals(3, headSet.size());
    assertEquals(key2, headSet.first());
    assertEquals(key4, headSet.last());
    assertFalse(headSet.contains(key));
    assertFalse(keySet.contains(key));
    assertTrue(headSet.contains(key2));
    assertTrue(headSet.contains(key3));
    assertTrue(headSet.contains(key4));
    assertFalse(headSet.contains(key5));
    assertFalse(headSet.contains(key6));

    // re-add key to head set
    // -> sub set = {key, key2, key3, key4}
    // -> key set = {key, key2, key3, key4, key6}
    assertTrue(headSet.add(key));

    assertEquals(4, headSet.size());
    assertEquals(key, headSet.first());
    assertEquals(key4, headSet.last());
    assertTrue(headSet.contains(key));
    assertTrue(headSet.contains(key2));
    assertTrue(headSet.contains(key3));
    assertTrue(keySet.contains(key3));
    assertTrue(headSet.contains(key4));
    assertFalse(headSet.contains(key5));
    assertFalse(headSet.contains(key6));
  }

  @Test
  public void test_keySetTailSet2ShouldPass() {
    var keySet = KeySet.create(key, key2, key4, key6);

    // key3 is not part of key set
    assertThrows(IllegalArgumentException.class, () -> keySet.tailSet(key3, false));

    // key5 is not part of key set
    assertThrows(IllegalArgumentException.class, () -> keySet.tailSet(key5, false));

    // tail set key[ = {key2, key4, key6}
    var tailSet = keySet.tailSet(key, false);

    assertEquals(3, tailSet.size());
    assertEquals(key2, tailSet.first());
    assertEquals(key6, tailSet.last());
    assertFalse(tailSet.contains(key));
    assertTrue(tailSet.contains(key2));
    assertFalse(tailSet.contains(key3));
    assertTrue(tailSet.contains(key4));
    assertFalse(tailSet.contains(key5));
    assertTrue(tailSet.contains(key6));

    // insert key3 into key set
    // -> tail set = {key2, key3, key4, key6}
    // -> key set = {key, key2, key3, key4, key6}
    assertTrue(keySet.add(key3));

    assertEquals(4, tailSet.size());
    assertEquals(key2, tailSet.first());
    assertEquals(key6, tailSet.last());
    assertFalse(tailSet.contains(key));
    assertTrue(tailSet.contains(key2));
    assertTrue(tailSet.contains(key3));
    assertTrue(tailSet.contains(key4));
    assertFalse(tailSet.contains(key5));
    assertTrue(tailSet.contains(key6));

    // tail set already contains key2, key3, key4 and key6
    assertFalse(tailSet.add(key2));
    assertFalse(tailSet.add(key3));
    assertFalse(tailSet.add(key4));
    assertFalse(tailSet.add(key6));

    // insert key5 into head set
    // -> tail set = {key2, key3, key4, key5, key6}
    // -> key set = {key, key2, key3, key4, key5, key6}
    assertTrue(tailSet.add(key5));

    // key is below inclusive lower bound
    assertThrows(IllegalArgumentException.class, () -> tailSet.add(key));
    assertFalse(tailSet.remove(key));

    assertEquals(5, tailSet.size());
    assertEquals(key2, tailSet.first());
    assertEquals(key6, tailSet.last());
    assertFalse(tailSet.contains(key));
    assertTrue(tailSet.contains(key2));
    assertTrue(tailSet.contains(key3));
    assertTrue(tailSet.contains(key4));
    assertTrue(tailSet.contains(key5));
    assertTrue(keySet.contains(key5));
    assertTrue(tailSet.contains(key6));

    // remove key2 from tail set
    // -> tail set = {key3, key4, key5, key6}
    // -> key set = {key, key3, key4, key5, key6}
    assertTrue(tailSet.remove(key2));

    assertEquals(4, tailSet.size());
    assertEquals(key3, tailSet.first());
    assertEquals(key6, tailSet.last());
    assertFalse(tailSet.contains(key));
    assertFalse(tailSet.contains(key2));
    assertFalse(keySet.contains(key2));
    assertTrue(tailSet.contains(key3));
    assertTrue(tailSet.contains(key4));
    assertTrue(tailSet.contains(key5));
    assertTrue(tailSet.contains(key6));

    // re-add key2 to head set
    // -> tail set = {key2, key3, key4, key5, key6}
    // -> key set = {key, key2, key3, key4, key5, key6}
    assertTrue(tailSet.add(key2));

    assertEquals(5, tailSet.size());
    assertEquals(key2, tailSet.first());
    assertEquals(key6, tailSet.last());
    assertFalse(tailSet.contains(key));
    assertTrue(tailSet.contains(key2));
    assertTrue(tailSet.contains(key3));
    assertTrue(tailSet.contains(key4));
    assertTrue(tailSet.contains(key5));
    assertTrue(keySet.contains(key5));
    assertTrue(tailSet.contains(key6));
  }
}
