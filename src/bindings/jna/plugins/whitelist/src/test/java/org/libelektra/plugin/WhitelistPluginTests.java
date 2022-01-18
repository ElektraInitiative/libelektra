package org.libelektra.plugin;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.libelektra.Plugin.JNI_MODULE_CONTRACT_ROOT;

import java.util.Random;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.libelektra.KDBException;
import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;
import org.libelektra.ReadableKey;
import org.libelektra.exception.SemanticValidationException;

public class WhitelistPluginTests {

  private Plugin plugin;

  @Before
  public void setup() {
    plugin = new WhitelistPlugin();
    plugin.open(plugin.getConfig(), Key.create());
  }

  @After
  public void tearDown() {
    plugin.close(Key.create());
  }

  @Test
  public void test_getJniModuleContractRoot_shouldPass() throws KDBException {
    var key = Key.create(JNI_MODULE_CONTRACT_ROOT);
    var keySet = KeySet.create();
    int result = plugin.get(keySet, key);
    var infoProvides =
        keySet
            .lookup(JNI_MODULE_CONTRACT_ROOT + "/infos/provides")
            .map(Key::getString)
            .orElseThrow();
    var infoPlacements =
        keySet
            .lookup(JNI_MODULE_CONTRACT_ROOT + "/infos/placements")
            .map(Key::getString)
            .orElseThrow();
    var infoMetdata =
        keySet
            .lookup(JNI_MODULE_CONTRACT_ROOT + "/infos/metadata")
            .map(Key::getString)
            .orElseThrow();

    assertEquals(Plugin.STATUS_SUCCESS, result);
    assertTrue(infoProvides.contains("check"));
    assertTrue(infoPlacements.contains("presetstorage"));
    assertTrue(infoMetdata.contains("check/whitelist/#"));
  }

  @Test
  public void test_get_shouldPass() throws KDBException {
    int result = plugin.get(KeySet.create(), Key.create());

    assertEquals(Plugin.STATUS_NO_UPDATE, result);
  }

  @Test
  public void test_setInvalid_shouldPass() throws KDBException {
    var parentKey = Key.create();
    var key = addSpecMetaData(Key.create("user:/test")).setString("not-allowed");
    int result = plugin.set(KeySet.create(key), parentKey);
    var errorNumber = parentKey.getMeta("error/number").map(ReadableKey::getString).orElseThrow();

    assertEquals(Plugin.STATUS_ERROR, result);
    assertEquals(SemanticValidationException.ERROR_NUMBER, errorNumber);
  }

  @Test
  public void test_setBinary_shouldPass() throws KDBException {
    byte[] binaryValue = new byte[20];
    new Random().nextBytes(binaryValue);
    var parentKey = Key.create();
    var key =
        addSpecMetaData(Key.create("user:/test")).setString("allowed0").setBinary(binaryValue);
    int result = plugin.set(KeySet.create(key), parentKey);
    var errorNumber = parentKey.getMeta("error/number").map(ReadableKey::getString).orElseThrow();

    assertEquals(Plugin.STATUS_ERROR, result);
    assertEquals(SemanticValidationException.ERROR_NUMBER, errorNumber);
  }

  @Test
  public void test_setValid_shouldPass() throws KDBException {
    var parentKey = Key.create();
    Key key;
    int result;
    key = addSpecMetaData(Key.create("user:/test")).setString("allowed0");
    result = plugin.set(KeySet.create(key), parentKey);

    assertEquals(Plugin.STATUS_SUCCESS, result);
    assertTrue(parentKey.getMeta("error/number").isEmpty());

    key = addSpecMetaData(Key.create("user:/test")).setString("allowed1");
    result = plugin.set(KeySet.create(key), parentKey);

    assertEquals(Plugin.STATUS_SUCCESS, result);
    assertTrue(parentKey.getMeta("error/number").isEmpty());

    key = addSpecMetaData(Key.create("user:/test")).setString("allowed3");
    result = plugin.set(KeySet.create(key), parentKey);

    assertEquals(Plugin.STATUS_SUCCESS, result);
    assertTrue(parentKey.getMeta("error/number").isEmpty());
    assertTrue(parentKey.getMeta("warnings/#0/number").isPresent());
    assertEquals(
        SemanticValidationException.ERROR_NUMBER,
        parentKey.getMeta("warnings/#0/number").get().getString());
    assertEquals(
        SemanticValidationException.ERROR_NUMBER,
        parentKey.getMeta("warnings/#1/number").get().getString());
  }

  private Key addSpecMetaData(Key key) {
    key.setMeta("check/whitelist/#__0", "allowed0");
    key.setMeta("check/whitelist/#__1", "allowed1");
    key.setMeta("check/whitelist/#100", "allowed3");
    key.setMeta("check/whitelist/garbage", "something");
    key.setMeta("check/whitelist/#__2/garbage", "someotherthing");
    return key;
  }
}
