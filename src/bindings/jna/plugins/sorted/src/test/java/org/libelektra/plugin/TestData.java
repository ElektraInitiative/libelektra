package org.libelektra.plugin;

import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;

import static org.libelektra.Plugin.PROCESS_CONTRACT_ROOT;

public class TestData {
    private static final String parentKeyName = "/test/plugin/sorted";

    public static Key givenParentKey() {
        return Key.create(parentKeyName);
    }

    @SuppressWarnings("SameParameterValue")
    private static Key getParentKeyWith(String arraySize) {
        return Key.create(parentKeyName)
                .setMeta("array", arraySize)
                .setMeta("check/sorted", "");
    }

    private static Key getParentKeyWith(String arraySize, String sortKey) {
        return Key.create(parentKeyName)
                .setMeta("array", arraySize)
                .setMeta("check/sorted", sortKey);
    }

    private static Key getParentKeyWith(String arraySize, String sortKey, String direction) {
        return Key.create(parentKeyName)
                .setMeta("array", arraySize)
                .setMeta("check/sorted", sortKey)
                .setMeta("check/sorted/direction", direction);
    }

    public static KeySet givenASortedSetByPrimitiveInt() {
        return KeySet.create(
                getParentKeyWith("#2"),
                Key.create(parentKeyName + "/#0", "0"),
                Key.create(parentKeyName + "/#1", "1"),
                Key.create(parentKeyName + "/#2", "2")
        );
    }

    public static KeySet givenAnUnsortedSetByPrimitiveInt() {
        return KeySet.create(
                getParentKeyWith("#2"),
                Key.create(parentKeyName + "/#0", "3"),
                Key.create(parentKeyName + "/#1", "1"),
                Key.create(parentKeyName + "/#2", "2")
        );
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
