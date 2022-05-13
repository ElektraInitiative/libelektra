package org.libelektra.plugin;

import static java.util.Map.*;

import java.util.Map;
import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;

class SortedContract {
  private static final Map<String, String> valuesByKeyName =
      ofEntries(
          entry("/infos", "Sorted Java Plugin, loaded by process plugin"),
          entry(
              "/infos/author",
              "Markus Bointner <e11808221@student.tuwien.ac.at>, Philipp Leeb"
                  + " <e11808219@student.tuwien.ac.at>"),
          entry("/infos/license", "BSD"),
          entry("/infos/provides", "check"),
          entry("/infos/placements", "presetstorage postgetstorage"),
          entry("/infos/metadata", "check/sorted check/sorted/direction"),
          entry(
              "/infos/description",
              "Enforces a given order of array elements based on a custom defined key or primitive"
                  + " values"),
          entry("/infos/status", "experimental"),
          entry("/infos/version", "1"),
          entry("/exports/has/set", "1"),
          entry("/exports/has/get", "1"));

  public static void appendAllTo(KeySet keySet) {
    valuesByKeyName.forEach(
        (key, value) -> keySet.append(Key.create(Plugin.PROCESS_CONTRACT_ROOT + key, value)));
  }
}
