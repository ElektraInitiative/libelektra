package org.libelektra.app;

import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.stream.Collectors;
import org.libelektra.KDB;
import org.libelektra.KDBException;
import org.libelektra.Key;
import org.libelektra.KeySet;

public class App {

  private static final String MOUNT_SPACE = "system:/";
  private static final String KEY_PREFIX = MOUNT_SPACE + "elektra/version/infos/";

  public static void main(String[] args) {
    System.out.println("Example started");
    var stringStringMap = loadConfigurationSettings();
    System.out.println("Result:");
    System.out.println(stringStringMap);
    System.out.println("Example terminated");
    // do whatever you need with the value starting from here
  }

  private static Map<String, String> loadConfigurationSettings() {
    // all keys we want to retrieve
    String[] keys = new String[] {"author", "description", "licence", "versions"};

    // read the keys
    return readKeys(keys);
  }

  private static Map<String, String> readKeys(String[] keys) {
    System.out.println("Reading following keys:");
    Arrays.stream(keys).map(k -> KEY_PREFIX + k).forEach(System.out::println);

    // open KDB with autoclose functionality
    // keep in mind this is an expensive operation, avoid calling it too frequently
    try (KDB kdb = KDB.open()) {
      // fetch key set
      KeySet keySet = kdb.get(Key.create(MOUNT_SPACE));

      // fetch values
      return Arrays.stream(keys)
          .map(k -> KEY_PREFIX + k)
          .collect(
              Collectors.toMap(
                  k -> k,
                  // if not found, set it to "(no-spec-default)"
                  // if specification with default value is applied, this actually should never
                  // happen
                  k -> keySet.lookup(k).map(Key::getString).orElse("(no-spec-default)")));
    } catch (KDBException e) {
      e.printStackTrace();
    }
    return Collections.emptyMap();
  }
}
