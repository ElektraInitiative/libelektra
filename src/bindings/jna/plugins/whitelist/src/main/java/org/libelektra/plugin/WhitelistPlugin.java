package org.libelektra.plugin;

import java.util.HashSet;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.regex.Pattern;
import org.libelektra.ErrorCode;
import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;

/** Plugin enforcing key values to adhere to a specified whitelist */
public class WhitelistPlugin implements Plugin {

  private static final String PLUGIN_NAME = "Whitelist";
  private static final Pattern META_WHITELISTENTRY_PATTERN =
      Pattern.compile("meta:/check/whitelist/.*");
  private static final Pattern META_WHITELISTENTRY_VALID_PATTERN =
      Pattern.compile("meta:/check/whitelist/#_*\\d+");

  private static int validate(KeySet keySet, Key parentKey, BiFunction<ErrorCode, String, Key> setErrorOrWarning, int problemStatus) {
    // iterate key set and validate each key
    var status = STATUS_SUCCESS;
    for (var key : keySet) {
      if(validate(key, parentKey, setErrorOrWarning, problemStatus) == STATUS_ERROR) {
        status = STATUS_ERROR;
      }
    }
    return status;
  }
  private static int validate(Key key, Key parentKey, BiFunction<ErrorCode, String, Key> setErrorOrWarning, int problemStatus) {
      // look whether a whitelist has been defined
      Set<String> whitelist = new HashSet<>();
      for (var metaKey : key) {
        if (META_WHITELISTENTRY_PATTERN.matcher(metaKey.getName()).matches()) {
          if (META_WHITELISTENTRY_VALID_PATTERN.matcher(metaKey.getName()).matches()) {
            whitelist.add(metaKey.getString());
          } else {
            parentKey.addWarning(
                ErrorCode.VALIDATION_SEMANTIC,
                String.format(
                    "Key '%s' specification contains an invalid whitelist check '%s' which is"
                        + " therefore ignored.",
                    key.getName(), metaKey.getName()));
          }
        }
      }

      if (!whitelist.isEmpty()) {
        if (key.isBinary()) {
          setErrorOrWarning.apply(
              ErrorCode.VALIDATION_SEMANTIC,
              String.format(
                  "Key '%s' has a binary value but has a whitelist check specification.",
                  key.getName()));
          return problemStatus;
        }
        if (!whitelist.contains(key.getString())) {
          setErrorOrWarning.apply(
              ErrorCode.VALIDATION_SEMANTIC,
              String.format(
                  "Value of key '%s' with value '%s' does not adhere to whitelist of possible"
                      + " values: %s",
                  key.getName(), key.getString(), String.join(", ", whitelist)));
          return problemStatus;
        }
      }
      return STATUS_SUCCESS;
    }

  @Override
  public int open(KeySet conf, Key errorKey) {
    throw new UnsupportedOperationException();
  }

  @Override
  public int get(KeySet keySet, Key parentKey) {
    // if plugin meta data is requested, return additional contract values
    if (parentKey.isBelowOrSame(Key.create(PROCESS_CONTRACT_ROOT))) {
      keySet.append(
          Key.create(
              PROCESS_CONTRACT_ROOT + "/infos",
              PLUGIN_NAME + " Java plugin, loaded by the process plugin"));
      keySet.append(
          Key.create(PROCESS_CONTRACT_ROOT + "/infos/author", "Michael Tucek <michael@tucek.eu>"));
      keySet.append(Key.create(PROCESS_CONTRACT_ROOT + "/infos/provides", "check"));
      keySet.append(Key.create(PROCESS_CONTRACT_ROOT + "/infos/placements", "presetstorage"));
      keySet.append(Key.create(PROCESS_CONTRACT_ROOT + "/infos/metadata", "check/whitelist/#"));
      keySet.append(
          Key.create(
              PROCESS_CONTRACT_ROOT + "/infos/description", "Enforces a whitelist for key values"));
      keySet.append(Key.create(PROCESS_CONTRACT_ROOT + "/infos/status", "preview maintained"));
      keySet.append(Key.create(PROCESS_CONTRACT_ROOT + "/exports/has/set", "1"));

      //validate
      return validate(keySet, parentKey, parentKey::addWarning, STATUS_SUCCESS);
    }

    throw new UnsupportedOperationException();
  }

  @Override
  public int set(KeySet keySet, Key parentKey) {
    return validate(keySet, parentKey, parentKey::setError, STATUS_ERROR);
  }

  @Override
  public int error(KeySet keySet, Key parentKey) {
    throw new UnsupportedOperationException();
  }

  @Override
  public int close(Key parentKey) {
    throw new UnsupportedOperationException();
  }

  @Override
  public String getName() {
    return PLUGIN_NAME;
  }
}
