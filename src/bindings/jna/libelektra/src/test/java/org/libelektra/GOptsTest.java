package org.libelektra;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.libelektra.Key.CreateArgumentTag.KEY_META;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class GOptsTest {

  private static final String BASE_KEY = "/tests/java/gopts";
  private static final String SPEC_BASE_KEY = "spec:" + BASE_KEY;

  @Before
  public void setupSpec() throws KDBException {
    var specKeySet =
        KeySet.create(
            Key.create(SPEC_BASE_KEY, KEY_META, "command", ""),
            Key.create(
                SPEC_BASE_KEY + "/printversion",
                KEY_META,
                "description",
                "print version information and exit (ignoring all other"
                    + " options/commands/parameters)",
                KEY_META,
                "opt",
                "v",
                KEY_META,
                "opt/arg",
                "none",
                KEY_META,
                "opt/long",
                "version"),
            Key.create(
                SPEC_BASE_KEY + "/getter",
                KEY_META,
                "description",
                "get a key's value",
                KEY_META,
                "command",
                "get"),
            Key.create(
                SPEC_BASE_KEY + "/getter/verbose",
                KEY_META,
                "description",
                "print additional information about where the value comes from",
                KEY_META,
                "opt",
                "v",
                KEY_META,
                "opt/long",
                "verbose",
                KEY_META,
                "opt/arg",
                "none"),
            Key.create(
                SPEC_BASE_KEY + "/getter/keyname",
                KEY_META,
                "description",
                "name of the key to read",
                KEY_META,
                "args",
                "indexed",
                KEY_META,
                "args/index",
                "0"),
            Key.create(
                SPEC_BASE_KEY + "/setter",
                KEY_META,
                "description",
                "set a key's value",
                KEY_META,
                "command",
                "set"),
            Key.create(
                SPEC_BASE_KEY + "/setter/verbose",
                KEY_META,
                "description",
                "print additional information about where the value will be stored",
                KEY_META,
                "opt",
                "v",
                KEY_META,
                "opt/long",
                "verbose",
                KEY_META,
                "opt/arg",
                "none"),
            Key.create(
                SPEC_BASE_KEY + "/setter/keyname",
                KEY_META,
                "description",
                "name of the key to write",
                KEY_META,
                "args",
                "indexed",
                KEY_META,
                "args/index",
                "0"),
            Key.create(
                SPEC_BASE_KEY + "/setter/value",
                KEY_META,
                "description",
                "value to be written",
                KEY_META,
                "args",
                "indexed",
                KEY_META,
                "args/index",
                "1"),
            Key.create(
                SPEC_BASE_KEY + "/dynamic/#",
                KEY_META,
                "description",
                "dynamically call a user-supplied command",
                KEY_META,
                "args",
                "remaining"));
    var specParentKey = Key.create(SPEC_BASE_KEY);

    try (KDB kdb = KDB.open()) {
      var keySet = kdb.get(specParentKey);
      if (keySet.cut(specParentKey).size() > 0) {
        throw new IllegalStateException("Couldn't set up spec, keys exist!");
      }
      keySet.append(specKeySet);
      kdb.set(keySet, specParentKey);
    }
  }

  @Test
  public void test_gopts() throws KDBException {
    var args = new String[] {"test", "get", "-v", "user:/"};
    var env = new String[0];

    var config = KeySet.create();
    var parentKey = Key.create(BASE_KEY);
    var contract = KDB.goptsContract(args, env, parentKey, config);

    try (final KDB kdb = KDB.open(contract)) {
      var keySet = kdb.get(parentKey);

      assertTrue(keySet.lookup(BASE_KEY).isPresent());
      assertEquals(keySet.lookup(BASE_KEY).get().getString(), "getter");
      assertTrue(keySet.lookup(BASE_KEY + "/getter/keyname").isPresent());
      assertEquals(keySet.lookup(BASE_KEY + "/getter/keyname").get().getString(), "user:/");
      assertTrue(keySet.lookup(BASE_KEY + "/getter/verbose").isPresent());
      assertEquals(keySet.lookup(BASE_KEY + "/getter/verbose").get().getString(), "1");
    }
  }

  @After
  public void removeSpec() throws KDBException {
    var specParentKey = Key.create(SPEC_BASE_KEY);
    try (final KDB kdb = KDB.open()) {
      var keySet = kdb.get(specParentKey);
      keySet.cut(specParentKey);
      kdb.set(keySet, specParentKey);
    }
  }
}
