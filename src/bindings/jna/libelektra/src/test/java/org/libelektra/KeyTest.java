package org.libelektra;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Random;
import org.junit.Test;
import org.libelektra.exception.KeyBinaryValueException;
import org.libelektra.exception.KeyNameException;
import org.libelektra.exception.KeyStringValueException;

public class KeyTest {

  static final String KEY_1_NAME = "/key_test/1/key_name";
  static final String KEY_1_NAME_PART_1 = "\u0001";
  static final String KEY_1_NAME_PART_2 = "key_test";
  static final String KEY_1_NAME_PART_3 = "1";
  static final String KEY_1_NAME_PART_4 = "key_name";
  static final String KEY_1_VALUE = "key_value_1";
  static final String KEY_1_META_1_NAME = "/key_test/meta/1";
  static final String KEY_1_META_1_VALUE = "meta_1_value";
  static final String KEY_1_META_2_NAME = "/key_test/meta/2";
  static final String KEY_1_META_2_VALUE = "meta_2_value";
  static final int KEY_1_BINARY_VALUE_SIZE = 20;

  static final String KEY_2_NAME = "/key_test/2/key_name";
  static final String KEY_2_NAME_PART_1 = "\u0001";
  static final String KEY_2_NAME_PART_2 = "key_test";
  static final String KEY_2_NAME_PART_3 = "2";
  static final String KEY_2_NAME_PART_4 = "key_name";
  static final String KEY_2_VALUE = "0";

  static final String KEY_3_NAME = "/key_test/3/key_name";
  static final String KEY_3_VALUE = "1";

  static final String KEY_4_NAME = "/key_test/4/key_name";
  static final String KEY_4_VALUE = "32123";

  static final String KEY_5_NAME = "/key_test/5/key_name";
  static final String KEY_5_VALUE = "214748365";

  static final String KEY_6_NAME = "/key_test/6/key_name";
  static final String KEY_6_VALUE = "121424748365";

  static final String KEY_7_NAME = "/key_test/7/key_name";
  static final String KEY_7_VALUE = "121424748365123123123123123123354";

  static final String KEY_8_NAME = "/key_test/8/key_name";
  static final String KEY_8_VALUE = "123.456";

  static final String KEY_9_NAME = "/key_test/9/key_name";
  static final String KEY_9_VALUE = "12345.678911";

  static final String KEY_10_NAME = "/key_test/10/key_name";
  static final String KEY_10_VALUE = "blub";

  static final String KEY_11_NAME = "/key_test/10/key_name/sub";
  static final String KEY_11_VALUE = "blub2";

  static final String KEY_12_NAME = "/key_test/10/key_name/sub/1";
  static final String KEY_12_VALUE = "blub3";

  static byte[] key1BinaryValue = new byte[KEY_1_BINARY_VALUE_SIZE];

  static {
    new Random().nextBytes(key1BinaryValue);
  }

  @Test
  public void test_create_shouldPass() {
    Key.create().getPointer();
  }

  @Test
  public void test_createKey_shouldPass() {
    var key = Key.create(KEY_1_NAME, KEY_1_VALUE);

    assertEquals(KEY_1_NAME, key.toString());
    assertEquals(KEY_1_VALUE, key.getString());
  }

  @Test
  public void test_createKeyNullValue_shouldPass() {
    var key = Key.create(KEY_1_NAME, (Key) null);

    assertEquals(KEY_1_NAME, key.toString());
    assertEquals("", key.getString());
  }

  @Test(expected = IllegalStateException.class)
  public void test_accessingKeyAfterRelease_shouldThrow() {
    var key = Key.create(KEY_1_NAME, KEY_1_VALUE);
    key.release();

    key.getPointer();
  }

  @Test
  public void test_createKeyMetadata_shouldPass() {
    var key = Key.create(KEY_1_NAME, KEY_1_VALUE);

    assertEquals(KEY_1_NAME, key.toString());
    assertEquals(KEY_1_VALUE, key.getString());
  }

  @Test
  public void test_createKeyFromPointer_shouldPass() {
    var key = Key.create(KEY_1_NAME, KEY_1_VALUE);
    var key2 = new Key(key.getPointer());

    assertEquals(key.toString(), key2.toString()); // equal key name
    assertEquals(key.getString(), key2.getString()); // equal key value
    assertEquals(key.getPointer(), key2.getPointer()); // equal pointer
  }

  @Test
  public void test_keyWithNullValue_shouldPass() {
    var key = Key.create(KEY_2_NAME);

    assertEquals("", key.getString());
    assertFalse(key.isNull());

    key.setString("test");

    assertFalse(key.isNull());

    key.setNull();

    assertTrue(key.isNull());
  }

  @Test
  public void test_keyWithBooleanValue_shouldPass() {
    var key = Key.create(KEY_2_NAME, KEY_2_VALUE);

    assertFalse(key.getBoolean());

    key.setBoolean(true);

    assertTrue(key.getBoolean());
    assertEquals("1", key.getString());

    key.setString("false");

    assertFalse(key.getBoolean());

    key = Key.create(KEY_2_NAME).setBoolean(true);

    assertTrue(key.getBoolean());

    key.setBoolean(false);

    assertFalse(key.getBoolean());
    assertEquals("0", key.getString());
  }

  @Test
  public void test_keyWithByteValue_shouldPass() {
    byte expected = Byte.parseByte(KEY_3_VALUE);
    var key = Key.create(KEY_3_NAME, KEY_3_VALUE);

    assertEquals(expected, key.getByte());

    key = Key.create(KEY_3_NAME).setByte(expected);

    assertEquals(expected, key.getByte());
  }

  @Test(expected = NumberFormatException.class)
  public void test_keyWithWrongByteValue_shouldFail() {
    var key = Key.create(KEY_4_NAME, KEY_4_VALUE);
    key.getByte();
  }

  @Test
  public void test_keyWithShortValue_shouldPass() {
    short expected = Short.parseShort(KEY_4_VALUE);
    var key = Key.create(KEY_4_NAME, KEY_4_VALUE);

    assertEquals(expected, key.getShort());

    key = Key.create(KEY_4_NAME).setShort(expected);

    assertEquals(expected, key.getShort());
  }

  @Test(expected = NumberFormatException.class)
  public void test_keyWithTooBigShortValue_shouldFail() {
    var key = Key.create(KEY_5_NAME, KEY_5_VALUE);
    key.getShort();
  }

  @Test
  public void test_keyWithIntValue_shouldPass() {
    int expected = Integer.parseInt(KEY_5_VALUE);
    var key = Key.create(KEY_5_NAME, KEY_5_VALUE);

    assertEquals(expected, key.getInt());

    key = Key.create(KEY_5_NAME).setInt(expected);

    assertEquals(expected, key.getInt());
  }

  @Test(expected = NumberFormatException.class)
  public void test_keyWithTooBigIntegerValue_shouldFail() {
    var key = Key.create(KEY_6_NAME, KEY_6_VALUE);
    key.getInt();
  }

  @Test
  public void test_keyWithLongValue_shouldPass() {
    long expected = Long.parseLong(KEY_6_VALUE);
    var key = Key.create(KEY_6_NAME, KEY_6_VALUE);

    assertEquals(expected, key.getLong());

    key = Key.create(KEY_6_NAME).setLong(expected);

    assertEquals(expected, key.getLong());
  }

  @Test(expected = NumberFormatException.class)
  public void test_keyWithTooBigLongValue_shouldFail() {
    var key = Key.create(KEY_7_NAME, KEY_7_VALUE);
    key.getLong();
  }

  @Test
  public void test_keyWithFloatValue_shouldPass() {
    float expected = Float.parseFloat(KEY_8_VALUE);
    var key = Key.create(KEY_8_NAME, KEY_8_VALUE);

    assertEquals(expected, key.getFloat(), 0.0f);

    key = Key.create(KEY_8_NAME).setFloat(expected);

    assertEquals(expected, key.getFloat(), 0.0f);
  }

  @Test
  public void test_keyWithDoubleValue_shouldPass() {
    double expected = Double.parseDouble(KEY_9_VALUE);
    var key = Key.create(KEY_9_NAME, KEY_9_VALUE);

    assertEquals(expected, key.getDouble(), 0.0d);

    key = Key.create(KEY_9_NAME).setDouble(expected);

    assertEquals(expected, key.getDouble(), 0.0d);
  }

  @Test
  public void test_keyWithStringValue_shouldPass() {
    var key = Key.create(KEY_1_NAME, KEY_1_VALUE);

    assertEquals(KEY_1_VALUE, key.getString());

    key = Key.create(KEY_1_NAME).setString(KEY_1_VALUE);

    assertEquals(KEY_1_VALUE, key.getString());
  }

  @Test
  public void test_keyWithBinaryValue_shouldPass() {
    var key = Key.create(KEY_1_NAME);

    assertFalse(key.isBinary());
    assertTrue(key.isString());

    key.setBinary(key1BinaryValue);

    assertEquals(KEY_1_BINARY_VALUE_SIZE, key.getValueSize());
    assertTrue(key.isBinary());
    assertFalse(key.isString());

    byte[] read = key.getBinary();

    assertArrayEquals(key1BinaryValue, read);

    key.setString(KEY_1_VALUE);

    assertEquals(KEY_1_VALUE, key.getString());
    assertFalse(key.isBinary());
    assertTrue(key.isString());
  }

  @Test(expected = KeyBinaryValueException.class)
  public void test_keyGetBinaryIsString_shouldThrow() {
    var key = Key.create(KEY_1_NAME, KEY_1_VALUE);
    key.getBinary();
  }

  @Test(expected = KeyStringValueException.class)
  public void test_keyGetStringIsBinary_shouldThrow() {
    var key = Key.create(KEY_1_NAME).setBinary(key1BinaryValue);
    key.getString();
  }

  @Test
  public void test_keyMetaInformation_shouldPass() {
    // setup key with meta
    var key =
        Key.create(KEY_1_NAME, KEY_1_VALUE)
            .setMeta(KEY_1_META_1_NAME, KEY_1_META_1_VALUE)
            .setMeta(KEY_1_META_2_NAME, KEY_1_META_2_VALUE);

    // check meta
    var metakeys = key.meta();

    var oMeta = metakeys.at(0);
    assertEquals("meta:" + KEY_1_META_1_NAME, oMeta.getName());
    assertEquals(KEY_1_META_1_VALUE, oMeta.getString());

    oMeta = metakeys.at(1);
    assertEquals("meta:" + KEY_1_META_2_NAME, oMeta.getName());
    assertEquals(KEY_1_META_2_VALUE, oMeta.getString());

    // setup another key
    var key2 = Key.create(KEY_2_NAME, KEY_2_VALUE);
    key2.copyAllMeta(key);

    // check meta for second key
    metakeys = key2.meta();
    oMeta = metakeys.at(0);

    assertEquals("meta:" + KEY_1_META_1_NAME, oMeta.getName());
    assertEquals(KEY_1_META_1_VALUE, oMeta.getString());

    oMeta = metakeys.at(1);

    assertEquals("meta:" + KEY_1_META_2_NAME, oMeta.getName());
    assertEquals(KEY_1_META_2_VALUE, oMeta.getString());
  }

  @Test
  public void test_keyMetaIterator_shouldPass() {
    // setup key with meta
    var key =
        Key.create(KEY_1_NAME, KEY_1_VALUE)
            .setMeta(KEY_1_META_1_NAME, KEY_1_META_1_VALUE)
            .setMeta(KEY_1_META_2_NAME, KEY_1_META_2_VALUE);
    var iter = key.iterator();

    assertTrue(iter.hasNext());

    ReadableKey metaKey = iter.next();

    assertEquals("meta:" + KEY_1_META_1_NAME, metaKey.getName());
    assertEquals(KEY_1_META_1_VALUE, metaKey.getString());
    assertTrue(iter.hasNext());

    metaKey = iter.next();

    assertEquals("meta:" + KEY_1_META_2_NAME, metaKey.getName());
    assertEquals(KEY_1_META_2_VALUE, metaKey.getString());
    assertFalse(iter.hasNext());

    iter.remove();

    assertThrows(NoSuchElementException.class, () -> iter.next());

    var oMetaKey = key.getMeta(KEY_1_META_1_NAME);

    assertTrue(oMetaKey.isPresent());
    assertEquals(KEY_1_META_1_VALUE, oMetaKey.get().getString());

    oMetaKey = key.getMeta(KEY_1_META_2_NAME);

    assertFalse(oMetaKey.isPresent());
  }

  @Test
  public void test_keyCompare_shouldPass() {
    var key = Key.create(KEY_1_NAME, KEY_1_VALUE);
    var key2 = Key.create(KEY_2_NAME, KEY_2_VALUE);

    assertEquals(0, key.compareTo(key));
    assertEquals(-1, key.compareTo(key2));
    assertEquals(1, key2.compareTo(key));
  }

  @Test
  public void test_keyIsBelow_shouldPass() {
    var key = Key.create(KEY_10_NAME, KEY_10_VALUE);
    var key2 = Key.create(KEY_11_NAME, KEY_11_VALUE);
    var key3 = Key.create(KEY_12_NAME, KEY_12_VALUE);

    assertTrue(key2.isBelow(key));
    assertTrue(key3.isBelow(key));
    assertTrue(key3.isBelow(key2));
    assertFalse(key.isBelow(key));
  }

  @Test
  public void test_keyIsBelowOrSame_shouldPass() {
    var key = Key.create(KEY_10_NAME, KEY_10_VALUE);
    var key2 = Key.create(KEY_11_NAME, KEY_11_VALUE);
    var key3 = Key.create(KEY_12_NAME, KEY_12_VALUE);

    assertTrue(key.isBelowOrSame(key));
    assertTrue(key2.isBelowOrSame(key));
    assertTrue(key2.isBelowOrSame(key2));
    assertTrue(key3.isBelowOrSame(key));
    assertTrue(key3.isBelowOrSame(key2));
    assertTrue(key3.isBelowOrSame(key3));
  }

  @Test
  public void test_keyIsDirectlyBelow_shouldPass() {
    var key = Key.create(KEY_10_NAME, KEY_10_VALUE);
    var key2 = Key.create(KEY_11_NAME, KEY_11_VALUE);
    var key3 = Key.create(KEY_12_NAME, KEY_12_VALUE);

    assertFalse(key.isDirectlyBelow(key));
    assertTrue(key2.isDirectlyBelow(key));
    assertFalse(key2.isDirectlyBelow(key2));
    assertFalse(key2.isDirectlyBelow(key3));
    assertTrue(key3.isDirectlyBelow(key2));
    assertFalse(key3.isDirectlyBelow(key));
  }

  @Test
  public void test_keyGetNameSize_shouldPass() {
    var key = Key.create(KEY_1_NAME, KEY_1_VALUE);
    var key2 = Key.create(KEY_2_NAME, KEY_2_VALUE);
    var key3 = Key.create(KEY_3_NAME, KEY_3_VALUE);

    assertEquals(KEY_1_NAME.length() + 1, key.getNameSize());
    assertEquals(KEY_2_NAME.length() + 1, key2.getNameSize());
    assertEquals(KEY_3_NAME.length() + 1, key3.getNameSize());
  }

  @Test
  public void test_keySetName_shouldPass() {
    var newKeyName = "/some_random/test/stuff_or/whatever";
    var key = Key.create(KEY_1_NAME, KEY_1_VALUE).setName(newKeyName);

    assertEquals(newKeyName, key.getName());
  }

  @Test(expected = KeyNameException.class)
  public void test_keySetName_shouldFail() {
    var newKeyName = "some_random/test/stuff_or/whatever"; // initial slash missing
    var key = Key.create(KEY_1_NAME, KEY_1_VALUE);

    key.setName(newKeyName);
  }

  @Test
  public void test_keyGetBaseName_shouldPass() {
    var key = Key.create(KEY_1_NAME, KEY_1_VALUE);
    var key2 = Key.create(KEY_2_NAME, KEY_2_VALUE);

    assertEquals(KEY_1_NAME_PART_4, key.getBaseName());
    assertEquals(KEY_2_NAME_PART_4, key2.getBaseName());
  }

  @Test
  public void test_keySetBaseName_shouldPass() {
    // note: slashes in base name will be escaped
    var newBaseName = "/some_random/string";
    var key = Key.create(KEY_1_NAME, KEY_1_VALUE).setBaseName(newBaseName);

    assertEquals(newBaseName, key.getBaseName());
  }

  @Test
  public void test_keyAddBaseName_shouldPass() {
    // note: slashes in basename will be escaped
    var newBaseName = "/some_random/string";
    var newBaseName2 = "another_new/nice/basename";
    var key = Key.create(KEY_1_NAME, KEY_1_VALUE).addBaseName(newBaseName);

    assertEquals(newBaseName, key.getBaseName());

    key.addBaseName(newBaseName2);

    assertEquals(newBaseName2, key.getBaseName());
  }

  @Test
  public void test_keyGetValueSize_shouldPass() {
    var key = Key.create(KEY_1_NAME, KEY_1_VALUE);
    var key2 = Key.create(KEY_2_NAME, KEY_2_VALUE);
    var key3 = Key.create(KEY_3_NAME, KEY_3_VALUE);

    assertEquals(KEY_1_VALUE.length() + 1, key.getValueSize());
    assertEquals(KEY_2_VALUE.length() + 1, key2.getValueSize());
    assertEquals(KEY_3_VALUE.length() + 1, key3.getValueSize());
  }

  @Test
  public void test_keyNameIteratorHasNext_shouldPass() {
    Iterator<String> iterator = Key.create(KEY_1_NAME, KEY_1_VALUE).keyNameIterator();

    assertTrue(iterator.hasNext());
    assertEquals(KEY_1_NAME_PART_1, iterator.next());
    assertTrue(iterator.hasNext());
    assertEquals(KEY_1_NAME_PART_2, iterator.next());
    assertTrue(iterator.hasNext());
    assertEquals(KEY_1_NAME_PART_3, iterator.next());
    assertTrue(iterator.hasNext());
    assertEquals(KEY_1_NAME_PART_4, iterator.next());
    assertFalse(iterator.hasNext());
  }

  @Test(expected = UnsupportedOperationException.class)
  public void test_keyNameIteratorDelete_shouldFail() {
    Iterator<String> iterator = Key.create(KEY_1_NAME, KEY_1_VALUE).keyNameIterator();

    iterator.remove();
  }
}
