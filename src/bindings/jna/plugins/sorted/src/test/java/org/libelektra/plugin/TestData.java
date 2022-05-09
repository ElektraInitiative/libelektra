package org.libelektra.plugin;

import static org.libelektra.Plugin.PROCESS_CONTRACT_ROOT;

import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;

@SuppressWarnings("SameParameterValue")
public class TestData {
  private static final String parentKeyName = "/test/plugin/sorted";

  public static class KeySetWrapper {
    public final Key parentKey;
    public final KeySet keySet;

    KeySetWrapper(Key parentKey, Key... keys) {
      this.parentKey = parentKey;
      this.keySet = KeySet.create(keys).append(parentKey);
    }
  }

  private static Key getParentKeyWith(String arraySize) {
    return getParentKeyWith(arraySize, "");
  }

  private static Key getParentKeyWith(String arraySize, String sortKey) {
    return Key.create(parentKeyName).setMeta("array", arraySize).setMeta("check/sorted", sortKey);
  }

  private static Key getParentKeyWith(String arraySize, String sortKey, String direction) {
    return Key.create(parentKeyName)
        .setMeta("array", arraySize)
        .setMeta("check/sorted", sortKey)
        .setMeta("check/sorted/direction", direction);
  }

  public static KeySetWrapper givenASortedSetByPrimitiveInt() {
    return new KeySetWrapper(
        getParentKeyWith("#2"),
        Key.create(parentKeyName + "/#0", "0"),
        Key.create(parentKeyName + "/#1", "1"),
        Key.create(parentKeyName + "/#2", "2"));
  }

  public static KeySetWrapper givenAnUnsortedSetByPrimitiveInt() {
    return new KeySetWrapper(
        getParentKeyWith("#2"),
        Key.create(parentKeyName + "/#0", "3"),
        Key.create(parentKeyName + "/#1", "1"),
        Key.create(parentKeyName + "/#2", "2"));
  }

  public static KeySetWrapper givenADescendingSortedSetByPrimitiveInt() {
    return new KeySetWrapper(
        getParentKeyWith("#2", "", "desc"),
        Key.create(parentKeyName + "/#0", "3"),
        Key.create(parentKeyName + "/#1", "2"),
        Key.create(parentKeyName + "/#2", "1"));
  }

  public static KeySetWrapper givenASortedSetByComplexInt() {
    return new KeySetWrapper(
        getParentKeyWith("#2", "/key", "asc"),
        Key.create(parentKeyName + "/#0", "a"),
        Key.create(parentKeyName + "/#1", "c"),
        Key.create(parentKeyName + "/#2", "b"),
        Key.create(parentKeyName + "/#0/key", "1"),
        Key.create(parentKeyName + "/#1/key", "2"),
        Key.create(parentKeyName + "/#2/key", "3"));
  }

  public static KeySetWrapper givenAUnsortedSetByComplexInt() {
    return new KeySetWrapper(
        getParentKeyWith("#2", "/key", "asc"),
        Key.create(parentKeyName + "/#0", "a"),
        Key.create(parentKeyName + "/#1", "b"),
        Key.create(parentKeyName + "/#2", "c"),
        Key.create(parentKeyName + "/#0/key", "1"),
        Key.create(parentKeyName + "/#1/key", "3"),
        Key.create(parentKeyName + "/#2/key", "2"));
  }

  public static KeySet givenEmptyKeySet() {
    return KeySet.create();
  }

  public static Key givenProcessContractKey() {
    return Key.create(Plugin.PROCESS_CONTRACT_ROOT);
  }

  public static String getContractString(KeySet keySet, String keyName) {
    return keySet.lookup(PROCESS_CONTRACT_ROOT + keyName).map(Key::getString).orElseThrow();
  }
}
